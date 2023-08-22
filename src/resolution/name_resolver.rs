use std::collections::HashMap;

use chumsky::span::SimpleSpan;
use either::Either::{Left, Right};
use itertools::Itertools;
use lasso::Rodeo;
use logos::Source;

use crate::parsing::ast::declarations::{FlatUseDeclaration, Function};
use crate::parsing::ast::ParsedType;

use crate::parsing::utilities::Spanned;
use crate::project::{
    DeclarationResolutionStage, FileId, FunctionId, ModuleId, ResolvedUses, UseTarget,
    UseTargetSingle, UseTargetStar, Workspace,
};
use crate::source::DeclarationPath;

pub fn resolve_uses(rodeo: &mut Rodeo, workspace: &mut Workspace) {
    let project = workspace.project();
    let mut resolutions = HashMap::<ModuleId, ResolvedUses>::new();

    for (module_id, module) in project.src.files.iter() {
        let Left(uses) = &module.imports else {
            continue;
        };
        let mut resolved = ResolvedUses::new();
        let src = project
            .files
            .file_by_id(module.file_id)
            .expect("The file should exist")
            .src();

        for u in uses.iter() {
            let dependency_id = match workspace.dependencies.get_dependency_by_name(
                &u.scope
                    .iter()
                    .map(|s| {
                        rodeo.get_or_intern(
                            src.slice(s.into_range())
                                .expect("The span should be correct"),
                        )
                    })
                    .collect_vec(),
            ) {
                Ok(d) => d,
                Err(i) => {
                    resolved.add_error(*u.scope.get(i).unwrap());
                    continue;
                }
            };

            let scopes = workspace
                .dependencies
                .get_dependency_scope(dependency_id)
                .unwrap();
            let root = scopes.root_id();

            for flat in u.flatten_tree() {
                let FlatUseDeclaration {
                    path: flat_spans,
                    star,
                    single_alias,
                } = flat;

                match scopes.scope_child_by_path(
                    root,
                    &flat_spans
                        .iter()
                        .map(|s| rodeo.get_or_intern(src.slice(s.into_range()).unwrap()))
                        .collect_vec(),
                ) {
                    Ok(scope_id) => {
                        resolved.add(
                            dependency_id,
                            if star {
                                UseTarget::UseTargetStar(UseTargetStar {
                                    scope: scope_id,
                                    end_targets: {
                                        let children = project
                                            .scopes
                                            .scope_children(scope_id)
                                            .expect("BUG: the scope should exist!");
                                        children
                                            .iter()
                                            .map(|&id| (project.scopes.scope_name(id).unwrap(), id))
                                            .collect_vec()
                                    }
                                    .into_boxed_slice(),
                                })
                            } else {
                                UseTarget::UseTargetSingle(UseTargetSingle {
                                    alias_name: single_alias,
                                    scope: scope_id,
                                    name: project.scopes.scope_name(scope_id).unwrap(),
                                })
                            },
                        );
                    }
                    Err(i) => resolved.add_error(flat_spans[i]),
                }
            }
        }
        resolutions.insert(module_id, resolved);
    }
    let project = workspace.project_mut();
    for (module_id, resolved_uses) in resolutions.into_iter() {
        project.src.files.get_mut(module_id).unwrap().imports = Right(resolved_uses);
    }
}

#[derive(Debug)]
pub struct ResolvedFunctionOutline {
    parameters: Box<[Result<DeclarationPath, SimpleSpan>]>,
    ret_ty: Option<Result<DeclarationPath, SimpleSpan>>,
}

pub fn resolve_workspace_outlines(workspace: &mut Workspace) -> HashMap<FileId, Vec<SimpleSpan>> {
    resolve_function_outline(workspace)
}

pub fn resolve_function_outline(workspace: &mut Workspace) -> HashMap<FileId, Vec<SimpleSpan>> {
    let project = workspace.project();
    let workspace_read: &Workspace = workspace;
    let mut errors = HashMap::<FileId, Vec<SimpleSpan>>::new();
    let mut to_update = HashMap::<FunctionId, ResolvedFunctionOutline>::new();

    for (func_id, func) in project.pool.functions.iter() {
        let resolved = match &project
            .src
            .get_module_by_file(func.file_id)
            .unwrap()
            .imports
        {
            Left(_) => panic!("Should've been resolved"),
            Right(resolved) => resolved,
        };
        let DeclarationResolutionStage {
            ast: Function { parameters, ty, .. },
            ..
        } = &func.decl;
        let parameters = parameters
            .iter()
            .map(|param| {
                let Spanned(ty, span) = &param.ty;
                match ty {
                    ParsedType::Data(name_to_find) => {
                        if let Some((dep_id, _, dec_id, _)) = resolved
                            .find_struct(&workspace_read.dependencies, name_to_find.0)
                            .next()
                        {
                            Ok(DeclarationPath {
                                module_id: dep_id,
                                declaration_id: dec_id,
                            })
                        } else {
                            Err(*span)
                        }
                    }
                    ParsedType::Array(_) => todo!("array type resolution"),
                }
            })
            .collect_vec()
            .into_boxed_slice();
        let ret_ty = match ty {
            Some(Spanned(t, s)) => {
                let name_to_search = match t {
                    ParsedType::Data(Spanned(name_to_search, _)) => *name_to_search,
                    ParsedType::Array(_) => todo!("array type resolution"),
                };
                Some(
                    resolved
                        .find_struct(&workspace_read.dependencies, name_to_search)
                        .next()
                        .map(|(dep_id, _, dec_id, _)| DeclarationPath {
                            module_id: dep_id,
                            declaration_id: dec_id,
                        })
                        .ok_or(*s),
                )
            }
            None => None,
        };
        for e in parameters
            .iter()
            .filter_map(|r| r.err())
            .chain(ret_ty.and_then(|r| r.err().to_owned()))
        {
            match errors.get_mut(&func.file_id) {
                Some(v) => v.push(e),
                None => {
                    errors.insert(func.file_id, vec![e]);
                }
            }
        }
        to_update.insert(func_id, ResolvedFunctionOutline { parameters, ret_ty });
    }

    for (func_id, outline) in to_update.into_iter() {
        if let Some(func_info) = workspace.project_mut().pool.functions.get_mut(func_id) {
            func_info.decl.outline = Some(outline);
        }
    }
    errors
}
