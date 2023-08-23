use std::collections::HashMap;

use chumsky::span::SimpleSpan;
use either::Either::{Left, Right};
use itertools::Itertools;
use lasso::Rodeo;
use logos::Source;

use crate::parsing::ast::declarations::{FlatUseDeclaration, Function, Struct, Variable};
use crate::parsing::ast::ParsedType;

use crate::parsing::utilities::Spanned;
use crate::project::{
    FileId, FunctionId, ModuleId, ResolvedUses, StructId, UseTarget, UseTargetSingle,
    UseTargetStar, VariableId, Workspace,
};
use crate::typing::TypeId;

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
            .expect("BUG: The file should exist")
            .src();

        for u in uses.iter() {
            let dependency_id = match workspace.dependencies.get_dependency_by_name(
                &u.scope
                    .iter()
                    .map(|s| {
                        rodeo.get_or_intern(
                            src.slice(s.into_range())
                                .expect("BUG: The span should be correct"),
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
                                        let children = scopes
                                            .scope_children(scope_id)
                                            .expect("BUG: the scope should exist!");
                                        children
                                            .iter()
                                            .map(|&id| (scopes.scope_name(id).unwrap(), id))
                                            .collect_vec()
                                    }
                                    .into_boxed_slice(),
                                })
                            } else {
                                UseTarget::UseTargetSingle(UseTargetSingle {
                                    alias_name: single_alias,
                                    scope: scope_id,
                                    name: scopes.scope_name(scope_id).unwrap(),
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
    parameters: Box<[Result<TypeId, SimpleSpan>]>,
    ret_ty: Option<Result<TypeId, SimpleSpan>>,
}

#[derive(Debug)]
pub struct ResolvedVariableOutline {
    ty: Option<Result<TypeId, SimpleSpan>>,
}

#[derive(Debug)]
pub struct ResolvedStructOutline {
    fields: Box<[Result<TypeId, SimpleSpan>]>,
}

pub fn resolve_workspace_outlines(workspace: &mut Workspace) -> HashMap<FileId, Vec<SimpleSpan>> {
    let func_errs = resolve_function_outline(workspace);
    let var_errs = resolve_variable_outline(workspace);
    let struct_errs = resolve_struct_outline(workspace);
    let mut errs = HashMap::new();
    for (k, v) in func_errs.into_iter().chain(var_errs).chain(struct_errs) {
        errs.insert(k, v);
    }
    errs
}

pub fn resolve_function_outline(workspace: &mut Workspace) -> HashMap<FileId, Vec<SimpleSpan>> {
    let project = workspace.project();
    let workspace_read: &Workspace = workspace;
    let mut errors = HashMap::<FileId, Vec<SimpleSpan>>::new();
    let mut to_update = HashMap::<FunctionId, ResolvedFunctionOutline>::new();

    for (func_id, func) in project.pool.functions.iter() {
        let module = &project
            .src
            .get_module_by_file(func.file_id)
            .expect("BUG: a module should've been created for the file");
        let resolved = match &module.imports {
            Left(_) => panic!("Should've been resolved"),
            Right(resolved) => resolved,
        };
        let Function { parameters, ty, .. } = &func.decl.ast;
        let parameters: Box<[Result<TypeId, SimpleSpan>]> = resolve_multiple(
            parameters.iter().map(|param| &param.ty),
            resolved,
            workspace_read,
            func.file_id,
        );
        let ret_ty = resolve_singular(ty, resolved, workspace_read, func.file_id);
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

pub fn resolve_variable_outline(workspace: &mut Workspace) -> HashMap<FileId, Vec<SimpleSpan>> {
    let project = workspace.project();
    let workspace_read: &Workspace = workspace;
    let mut errors = HashMap::<FileId, Vec<SimpleSpan>>::new();
    let mut to_update = HashMap::<VariableId, ResolvedVariableOutline>::new();

    for (var_id, var) in project.pool.variables.iter() {
        let module = &project
            .src
            .get_module_by_file(var.file_id)
            .expect("BUG: a module should've been created for the file");
        let resolved = match &module.imports {
            Left(_) => panic!("Should've been resolved"),
            Right(resolved) => resolved,
        };
        let Variable { ty, .. } = &var.decl.ast;
        let ty = resolve_singular(ty, resolved, workspace_read, var.file_id);
        if let Some(e) = ty.and_then(|r| r.err().to_owned()) {
            match errors.get_mut(&var.file_id) {
                Some(v) => v.push(e),
                None => {
                    errors.insert(var.file_id, vec![e]);
                }
            }
        }
        to_update.insert(var_id, ResolvedVariableOutline { ty });
    }

    for (var_id, outline) in to_update.into_iter() {
        if let Some(func_info) = workspace.project_mut().pool.variables.get_mut(var_id) {
            func_info.decl.outline = Some(outline);
        }
    }
    errors
}

pub fn resolve_struct_outline(workspace: &mut Workspace) -> HashMap<FileId, Vec<SimpleSpan>> {
    let project = workspace.project();
    let workspace_read: &Workspace = workspace;
    let mut errors = HashMap::<FileId, Vec<SimpleSpan>>::new();
    let mut to_update = HashMap::<StructId, ResolvedStructOutline>::new();

    for (struct_id, strct) in project.pool.structs.iter() {
        let module = &project
            .src
            .get_module_by_file(strct.file_id)
            .expect("BUG: a module should've been created for the file");
        let resolved = match &module.imports {
            Left(_) => panic!("Should've been resolved"),
            Right(resolved) => resolved,
        };
        let (Struct { fields, .. }, _) = &strct.decl.ast;
        let fields = resolve_multiple(
            fields.iter().map(|field| &field.ty),
            resolved,
            workspace_read,
            strct.file_id,
        );
        for e in fields.iter().filter_map(|r| r.err()) {
            match errors.get_mut(&strct.file_id) {
                Some(v) => v.push(e),
                None => {
                    errors.insert(strct.file_id, vec![e]);
                }
            }
        }
        to_update.insert(struct_id, ResolvedStructOutline { fields });
    }
    for (struct_id, outline) in to_update.into_iter() {
        if let Some(func_info) = workspace.project_mut().pool.structs.get_mut(struct_id) {
            func_info.decl.outline = Some(outline);
        }
    }
    errors
}

fn resolve_singular(
    ty: &Option<Spanned<ParsedType>>,
    resolved: &ResolvedUses,
    workspace_read: &Workspace,
    file_id: FileId,
) -> Option<Result<TypeId, SimpleSpan>> {
    let dependency_id = workspace_read.project_dep_id();
    let scopes = &workspace_read.project().scopes;
    let scope = scopes.scope_of_file(file_id).unwrap();
    let Some(Spanned(t, s)) = ty else {
        return None;
    };
    let name_for_search = *match t {
        ParsedType::Data(Spanned(name_for_search, _)) => name_for_search,
        ParsedType::Array(_) => todo!("array type resolution"),
    };
    Some(scopes
        .scope_children_by_name(scope, name_for_search)
        .into_iter()
        .flatten()
        .filter_map(|scope_id| {
            scopes
                .scope_as_declaration(scope_id)
                .and_then(|(_name, dec_id)| {
                    workspace_read
                        .dependencies
                        .get_dependency(dependency_id)?
                        .project
                        .pool
                        .type_id_of_declaration(dec_id)
                })
        })
        .chain(
            resolved
                .find_struct(&workspace_read.dependencies, name_for_search)
                .map(|(.., type_id)| type_id),
        )
        .next()
        .ok_or(*s))
}

fn resolve_multiple<'a, I: Iterator<Item = &'a Spanned<ParsedType>>>(
    fields: I,
    resolved: &ResolvedUses,
    workspace_read: &Workspace,
    file_id: FileId,
) -> Box<[Result<TypeId, SimpleSpan>]> {
    let dependency_id = workspace_read.project_dep_id();
    let scopes = &workspace_read.project().scopes;
    let scope = scopes.scope_of_file(file_id).unwrap();
    fields
        .map(|Spanned(ty, span)| match ty {
            ParsedType::Data(name_to_find) => scopes
                .scope_children_by_name(scope, name_to_find.0)
                .into_iter()
                .flatten()
                .filter_map(|scope_id| {
                    scopes
                        .scope_as_declaration(scope_id)
                        .and_then(|(_name, dec_id)| {
                            workspace_read
                                .dependencies
                                .get_dependency(dependency_id)?
                                .project
                                .pool
                                .type_id_of_declaration(dec_id)
                        })
                })
                .chain(
                    resolved
                        .find_struct(&workspace_read.dependencies, name_to_find.0)
                        .map(|(.., type_id)| type_id),
                )
                .next()
                .ok_or(*span),
            ParsedType::Array(_) => todo!("array type resolution"),
        })
        .collect_vec()
        .into_boxed_slice()
}
