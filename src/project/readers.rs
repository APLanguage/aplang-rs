use std::{cell::RefCell, collections::HashMap, ffi::OsStr, io::Read, path::Path, rc::Rc};

use anyhow::Result;
use chumsky::{prelude::Rich, primitive::end, span::SimpleSpan, Parser};
use either::Either;
use lasso::Rodeo;
use slotmap::SlotMap;
use thiserror::Error;

use crate::{
    parsing::{
        ast::declarations::Declaration,
        parsers::{file::file_parser, ParserState},
        tokenizer::{tokenize, Token},
    },
    resolution::name_resolution::resolve_workspace_outlines,
    source::{RefVirtualFile, SourceFile},
};

use walkdir::{WalkAction, WalkDir};

use super::{
    files::APLangWorkspaceFile,
    scopes::{ScopeType, Scopes},
    std_lib::create_std_lib,
    AstFiles, DeclarationDelegate, DeclarationId, DeclarationInfo, DeclarationPool,
    DeclarationResolutionStage, Dependencies, DependencyInfo, File, FileId, Files, FunctionId,
    ModuleData, ModuleId, Project, ProjectLink, StructId, StructLink, TypeRegistry, VariableId,
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
    Ok(Box<Workspace>),
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
    let mut types = TypeRegistry::new_by_spur_supplier(|s| rodeo.get_or_intern_static(s));
    let mut deps = Dependencies {
        deps: SlotMap::with_key(),
        by_name: HashMap::new(),
    };

    let project = match read_project(
        // just creating the egg manually
        ProjectLink::Project,
        rodeo,
        path.join("src").as_path(),
        &mut types,
    ) {
        ReadProjectResult::Err(err, files) => return ReadWorkspaceResult::ErrProject(err, files),
        ReadProjectResult::Ok(p) => *p,
    };
    let std_id = deps.add_dependency_with_key(|std_id| DependencyInfo {
        name: rodeo.get_or_intern_static("std"),
        project: create_std_lib(std_id, rodeo, &mut types),
    });
    let mut workspace = Workspace {
        project_name: rodeo.get_or_intern(&aplang_file.project.name),
        aplang_file,
        dependencies: deps,
        project,
        type_registery: Rc::new(RefCell::new(types)),
    };
    let std_id = ProjectLink::Dependency(std_id);
    let std_errs = &resolve_workspace_outlines(rodeo, &mut workspace, std_id);
    if !std_errs.is_empty() {
        let project = workspace.get_project(std_id);
        for (f_id, errs) in std_errs {
            eprintln!(
                "found {} errs in {}:",
                errs.len(),
                project.files.file_by_id(*f_id).unwrap().path().display()
            );
            for e in errs {
                eprintln!("  {}", e);
            }
        }
        panic!("There are resolution errors in std!")
    }
    ReadWorkspaceResult::Ok(Box::new(workspace))
}

pub enum ReadProjectResult {
    Err(anyhow::Error, Files),
    Ok(Box<Project>),
}

pub fn read_project(
    project_link: ProjectLink,
    rodeo: &mut Rodeo,
    path: &Path,
    types: &mut TypeRegistry,
) -> ReadProjectResult {
    let (files, scopes) = read_files(rodeo, path);
    read_project_from_files(project_link, rodeo, files, scopes, types)
}

pub fn read_project_from_files(
    project_link: ProjectLink,
    rodeo: &mut Rodeo,
    files: Files,
    mut scopes: Scopes,
    types: &mut TypeRegistry,
) -> ReadProjectResult {
    let mut functions = SlotMap::<FunctionId, _>::with_key();
    let mut structs = SlotMap::<StructId, _>::with_key();
    let mut variables = SlotMap::<VariableId, _>::with_key();
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
                let (name, id) = match declaration {
                    Declaration::Variable(v) => {
                        let name = v.name.0;
                        let id = variables.insert(DeclarationInfo {
                            decl: DeclarationResolutionStage {
                                ast: v,
                                outline: None,
                                full: None,
                            },
                            file_id,
                        });
                        module_variables.push((name, id));
                        (
                            name,
                            declaration_delegates.insert(DeclarationDelegate::Variable(id)),
                        )
                    }
                    Declaration::Function(f) => {
                        let name = f.name.0;
                        let id = functions.insert(DeclarationInfo {
                            decl: DeclarationResolutionStage {
                                ast: f,
                                outline: None,
                                full: None,
                            },
                            file_id,
                        });
                        module_functions.push((name, id));
                        (
                            name,
                            declaration_delegates.insert(DeclarationDelegate::Function(id)),
                        )
                    }
                    Declaration::Struct(s) => {
                        let name = s.name.0;
                        let id = structs.insert_with_key(|key| DeclarationInfo {
                            decl: DeclarationResolutionStage {
                                ast: (
                                    s,
                                    types.create_struct(StructLink {
                                        struct_id: key,
                                        project_link,
                                    }),
                                ),
                                outline: None,
                                full: None,
                            },
                            file_id,
                        });
                        module_structs.push((name, id));
                        (
                            name,
                            declaration_delegates.insert(DeclarationDelegate::Struct(id)),
                        )
                    }
                };
                scopes.add(
                    scopes
                        .scope_of_file(file_id)
                        .expect("BUG: File should have been registered as scope"),
                    ScopeType::Declaration(name, id),
                );
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

    ReadProjectResult::Ok(Box::new(Project {
        src: AstFiles::new(modules),
        pool: DeclarationPool {
            functions,
            structs,
            variables,
            declarations: declaration_delegates,
        },
        files,
        scopes,
    }))
}

fn read_files(rodeo: &mut Rodeo, path: &Path) -> (Files, Scopes) {
    let mut files = SlotMap::<FileId, Box<dyn File>>::with_key();
    let mut scopes = Scopes::new(path);
    let mut current_scope = scopes.root_id();
    for (action, path) in WalkDir::new(path)
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
                if !path
                    .extension()
                    .and_then(|n| n.to_str())
                    .is_some_and(|name| name == "aplang")
                {
                    continue;
                }
                let Some(name) = path.file_stem().and_then(OsStr::to_str) else {
                    continue;
                };

                if name.chars().any(|c| !c.is_ascii_alphabetic() && c != '_') {
                    println!("Skipped file {} cuz invalid name.", path.display());
                    continue;
                }

                let name = rodeo.get_or_intern(name);
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
