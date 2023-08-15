pub mod files;

use std::{io::Read, path::Path};

use anyhow::Result;
use serde::Deserialize;

use crate::parsing::{
    ast::declarations::{Declaration, Variable},
    tokenizer::Token,
};
use chumsky::{prelude::Rich, primitive::end, span::SimpleSpan, Parser};
use lasso::{Rodeo, Spur};
use slotmap::{new_key_type, SlotMap};
use thiserror::Error;
use toml::Table;

use crate::{
    parsing::{
        ast::declarations::{Function, Struct, UseDeclaration},
        parsers::{file::file_parser, ParserState},
        tokenizer::tokenize,
    },
    source::{RefVirtualFile, SourceFile},
};

use self::files::APLangWorkspaceFile;

new_key_type! { pub struct FunctionId; }
new_key_type! { pub struct StructId; }
new_key_type! { pub struct VariableId; }
new_key_type! { pub struct FileId; }
new_key_type! { pub struct ModuleId; }

pub struct Workspace {
    project: Project,
    aplang_file: APLangWorkspaceFile,
}

pub struct DeclarationInfo<D> {
    decl: D,
    file_id: FileId,
}

pub struct DeclarationPool {
    functions: SlotMap<FunctionId, DeclarationInfo<Function>>,
    structs: SlotMap<StructId, DeclarationInfo<Struct>>,
    variables: SlotMap<VariableId, DeclarationInfo<Variable>>,
}

pub trait File {
    fn src(&self) -> &str;
    fn path(&self) -> &Path;
}

impl File for VirtualFile {
    fn src(&self) -> &str {
        &self.src
    }

    fn path(&self) -> &Path {
        &self.path
    }
}

pub struct VirtualFile {
    src: String,
    path: Box<Path>,
}

enum FileOrFolder {
    File(FileId),
    Folder(Spur),
}

pub struct Files {
    files: SlotMap<FileId, Box<dyn File>>,
}

impl Files {
    pub fn file_by_id(&self, file_id: FileId) -> Option<&Box<dyn File>>{
        self.files.get(file_id)
    }
}

pub struct Project {
    src: AstFiles,
    pool: DeclarationPool,
    files: Files,
}

pub struct AstFiles {
    files: SlotMap<ModuleId, ModuleData>,
}

pub struct ModuleData {
    imports: Box<[UseDeclaration]>,
    functions: Vec<(Spur, FunctionId)>,
    structs: Vec<(Spur, StructId)>,
    variables: Vec<(Spur, VariableId)>,
    file_id: FileId,
}

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
    ReadWorkspaceResult::Ok(Workspace {
        project,
        aplang_file,
    })
}

pub enum ReadProjectResult {
    Err(anyhow::Error, Files),
    Ok(Project),
}

fn read_project(rodeo: &mut Rodeo, path: &Path) -> ReadProjectResult {
    let files = read_files(path);
    let mut functions: SlotMap<FunctionId, DeclarationInfo<Function>> = SlotMap::with_key();
    let mut structs: SlotMap<StructId, DeclarationInfo<Struct>> = SlotMap::with_key();
    let mut variables: SlotMap<VariableId, DeclarationInfo<Variable>> = SlotMap::with_key();
    let mut modules: SlotMap<ModuleId, ModuleData> = SlotMap::with_key();
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
                    }
                    Declaration::Function(f) => {
                        let name = f.name.0;
                        let id = functions.insert(DeclarationInfo { decl: f, file_id });
                        module_functions.push((name, id));
                    }
                    Declaration::Struct(s) => {
                        let name = s.name.0;
                        let id = structs.insert(DeclarationInfo { decl: s, file_id });
                        module_structs.push((name, id));
                    }
                };
            }

            modules.insert(ModuleData {
                imports: uses,
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
        },
        files,
    })
}

fn read_files(path: &Path) -> Files {
    let mut files = SlotMap::<FileId, Box<dyn File>>::with_key();
    for entry in walkdir::WalkDir::new(path.join("src"))
        .min_depth(1)
        .into_iter()
        .filter_map(|e| e.ok().map(|e| e.into_path()).filter(|p| p.is_file()))
        .filter_map(|p| std::fs::read_to_string(&p).ok().map(|src| (p, src)))
    {
        files.insert(Box::new(VirtualFile {
            src: entry.1,
            path: entry.0.into(),
        }));
    }
    Files { files }
}
