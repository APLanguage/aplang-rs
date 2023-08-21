use std::{ffi::OsStr, io::Read, path::Path};

use anyhow::Result;
use chumsky::{prelude::Rich, primitive::end, span::SimpleSpan, Parser};
use either::Either;
use lasso::Rodeo;
use slotmap::SlotMap;
use thiserror::Error;

use crate::{
    parsing::{
        ast::declarations::{Declaration, Function, Struct, Variable},
        parsers::{file::file_parser, ParserState},
        tokenizer::{tokenize, Token},
    },
    source::{RefVirtualFile, SourceFile},
    utils::walkdir::{WalkAction, WalkDir},
};

use super::{
    files::APLangWorkspaceFile,
    scopes::{ScopeType, Scopes},
    AstFiles, DeclarationDelegate, DeclarationId, DeclarationInfo, DeclarationPool, Dependencies,
    File, FileId, Files, FunctionId, ModuleData, ModuleId, Project, StructId, VariableId,
    VirtualFile, Workspace,
};

#[derive(Debug, Error)]
pub enum ReadWorkspaceError {
    #[error("No aplang.toml found")]
    NoWorkspaceFile,
    #[error("Errors found while parsing")]
    ParseErrors(Vec<(FileId, Rich<'static, Token, SimpleSpan, &'static str>)>),
}

pub enum ReadWorkspaceResult {
    ErrFile(anyhow::Error),
    ErrProject(anyhow::Error, Files),
    Ok(Workspace),
}
pub fn read_aplang_file(path: &Path) -> Result<APLangWorkspaceFile> {
    let aplang_toml = path.join("aplang.toml");
    if !aplang_toml.exists() {
        return Err(ReadWorkspaceError::NoWorkspaceFile.into());
    }
    let mut contents = String::new();
    std::fs::File::open(aplang_toml)?.read_to_string(&mut contents)?;
    Ok(toml::from_str::<APLangWorkspaceFile>(&contents)?)
}

pub fn read_workspace(rodeo: &mut Rodeo, path: &Path) -> ReadWorkspaceResult {
    let aplang_file = match read_aplang_file(path) {
        Ok(f) => f,
        Err(e) => return ReadWorkspaceResult::ErrFile(e),
    };
    let project = match read_project(rodeo, path) {
        ReadProjectResult::Err(err, files) => return ReadWorkspaceResult::ErrProject(err, files),
        ReadProjectResult::Ok(p) => p,
    };
    let (deps, id) =
        Dependencies::new_from_project(rodeo.get_or_intern(&aplang_file.project.name), project);
    ReadWorkspaceResult::Ok(Workspace {
        aplang_file,
        dependencies: deps,
        _project: id,
    })
}

pub enum ReadProjectResult {
    Err(anyhow::Error, Files),
    Ok(Project),
}

fn read_project(rodeo: &mut Rodeo, path: &Path) -> ReadProjectResult {
    let (files, scopes) = read_files(rodeo, path);
    let mut functions = SlotMap::<FunctionId, DeclarationInfo<Function>>::with_key();
    let mut structs = SlotMap::<StructId, DeclarationInfo<Struct>>::with_key();
    let mut variables = SlotMap::<VariableId, DeclarationInfo<Variable>>::with_key();
    let mut declaration_delegates = SlotMap::<DeclarationId, DeclarationDelegate>::with_key();

    let mut modules = SlotMap::<ModuleId, ModuleData>::with_key();
    let mut parse_errors: Vec<(FileId, Rich<'static, Token, _, &'static str>)> = vec![];
    for (file_id, file) in files.files.iter() {
        let virt_file = RefVirtualFile::new(file.src());
        let mut state = ParserState::new(rodeo, &virt_file);

        let result = file_parser()
            .then_ignore(end())
            .parse_with_state(tokenize(virt_file.whole_file()), &mut state);

        if result.has_errors() {
            for error in result.into_errors() {
                parse_errors.push((file_id, error.into_owned::<'static>()));
            }
        } else if let Some((uses, declarations)) = result.into_output() {
            let mut module_functions = vec![];
            let mut module_structs = vec![];
            let mut module_variables = vec![];

            for declaration in declarations.into_vec().into_iter() {
                match declaration {
                    Declaration::Variable(v) => {
                        let name = v.name.0;
                        let id = variables.insert(DeclarationInfo { decl: v, file_id });
                        module_variables.push((name, id));
                        declaration_delegates.insert(DeclarationDelegate::Variable(id));
                    }
                    Declaration::Function(f) => {
                        let name = f.name.0;
                        let id = functions.insert(DeclarationInfo { decl: f, file_id });
                        module_functions.push((name, id));
                        declaration_delegates.insert(DeclarationDelegate::Function(id));
                    }
                    Declaration::Struct(s) => {
                        let name = s.name.0;
                        let id = structs.insert(DeclarationInfo { decl: s, file_id });
                        module_structs.push((name, id));
                        declaration_delegates.insert(DeclarationDelegate::Struct(id));
                    }
                };
            }

            modules.insert(ModuleData {
                imports: Either::Left(uses),
                functions: module_functions,
                structs: module_structs,
                variables: module_variables,
                file_id,
            });
        }
    }
    if !parse_errors.is_empty() {
        return ReadProjectResult::Err(ReadWorkspaceError::ParseErrors(parse_errors).into(), files);
    }

    ReadProjectResult::Ok(Project {
        src: AstFiles { files: modules },
        pool: DeclarationPool {
            functions,
            structs,
            variables,
            declarations: declaration_delegates,
        },
        files,
        scopes,
    })
}

fn read_files(rodeo: &mut Rodeo, path: &Path) -> (Files, Scopes) {
    let mut files = SlotMap::<FileId, Box<dyn File>>::with_key();
    let mut scopes = Scopes::new(path);
    let mut current_scope = scopes.root_id();
    for (action, path) in WalkDir::new(path.join("src"))
        .min_depth(1)
        .into_iter()
        .filter_map(|e| e.ok().map(|(a, e)| (a, e.into_path())))
    {
        match action {
            WalkAction::EnterDir => {
                let Some(name) = path.file_name().and_then(OsStr::to_str) else {
                    continue;
                };
                current_scope =
                    scopes.add(current_scope, ScopeType::Package(rodeo.get_or_intern(name)));
            }
            WalkAction::ListFile => {
                let Some(name) = path
                    .file_name()
                    .and_then(OsStr::to_str)
                    .map(|n| rodeo.get_or_intern(n))
                else {
                    continue;
                };
                let Some(src) = std::fs::read_to_string(&path).ok() else {
                    continue;
                };
                let file_id = files.insert(Box::new(VirtualFile {
                    src,
                    path: path.into(),
                }));
                scopes.add(current_scope, ScopeType::File(name, file_id));
            }
            WalkAction::ExitDir => {
                current_scope = scopes
                    .parent(current_scope)
                    .expect("BUG: it should have a parent");
            }
        }
    }
    (Files { files }, scopes)
}
