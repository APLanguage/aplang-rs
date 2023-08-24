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
    DependencyId, FileId, FunctionId, ModuleId, ResolvedUses, StructId, UseTarget, UseTargetSingle,
    UseTargetStar, VariableId, Workspace,
};
use crate::typing::TypeId;

use super::FileScopedNameResolver;

pub fn resolve_uses(
    rodeo: &mut Rodeo,
    workspace: &mut Workspace,
    dependency_id: DependencyId,
) -> HashMap<FileId, Vec<SimpleSpan>> {
    let mut errors = HashMap::<FileId, Vec<SimpleSpan>>::new();

    let project = &workspace
        .dependencies
        .get_dependency(dependency_id)
        .unwrap()
        .project;
    let mut resolutions = HashMap::<ModuleId, ResolvedUses>::new();

    for (module_id, module) in project.src.iter() {
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

            let scopes = &workspace
                .dependencies
                .get_dependency_scopes(dependency_id)
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
                    Err(i) => {
                        resolved.add_error(flat_spans[i]);
                        match errors.get_mut(&module.file_id) {
                            Some(v) => v.push(flat_spans[i]),
                            None => {
                                errors.insert(module.file_id, vec![flat_spans[i]]);
                            }
                        }
                    }
                }
            }
        }
        resolutions.insert(module_id, resolved);
    }
    let project = &mut workspace.dependencies.get_dependency_mut(dependency_id).unwrap().project;
    for (module_id, resolved_uses) in resolutions.into_iter() {
        project
            .src
            .get_module_by_module_mut(module_id)
            .unwrap()
            .1
            .imports = Right(resolved_uses);
    }
    errors
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

pub fn resolve_workspace_outlines(
    rodeo: &mut Rodeo,
    workspace: &mut Workspace,
    dependency_id: DependencyId,
) -> HashMap<FileId, Vec<SimpleSpan>> {
    let import_errs = resolve_uses(rodeo, workspace, dependency_id);
    if !import_errs.is_empty() {
        return import_errs;
    }
    let func_errs = resolve_function_outline(workspace, dependency_id);
    let var_errs = resolve_variable_outline(workspace, dependency_id);
    let struct_errs = resolve_struct_outline(workspace, dependency_id);
    let mut errs = HashMap::new();
    for (k, v) in func_errs.into_iter().chain(var_errs).chain(struct_errs) {
        errs.insert(k, v);
    }
    errs
}

pub fn resolve_function_outline(
    workspace: &mut Workspace,
    dependency_id: DependencyId,
) -> HashMap<FileId, Vec<SimpleSpan>> {
    let workspace_read: &Workspace = workspace;
    let project = &workspace_read
        .dependencies
        .get_dependency(dependency_id)
        .unwrap()
        .project;

    let mut errors = HashMap::<FileId, Vec<SimpleSpan>>::new();
    let mut to_update = HashMap::<FunctionId, ResolvedFunctionOutline>::new();

    for (func_id, func) in project.pool.functions.iter() {
        let name_resolver = workspace_read.resolver_by_file(dependency_id, func.file_id);
        let Function { parameters, ty, .. } = &func.decl.ast;
        let parameters: Box<[Result<TypeId, SimpleSpan>]> =
            resolve_multiple(parameters.iter().map(|param| &param.ty), &name_resolver);
        let ret_ty = ty.as_ref().map(|ty| resolve_singular(ty, &name_resolver));
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

    let project_mut = &mut workspace
        .dependencies
        .get_dependency_mut(dependency_id)
        .unwrap()
        .project;
    for (func_id, outline) in to_update.into_iter() {
        if let Some(func_info) = project_mut.pool.functions.get_mut(func_id) {
            func_info.decl.outline = Some(outline);
        }
    }
    errors
}

pub fn resolve_variable_outline(
    workspace: &mut Workspace,
    dependency_id: DependencyId,
) -> HashMap<FileId, Vec<SimpleSpan>> {
    let workspace_read: &Workspace = workspace;

    let project = &workspace_read
        .dependencies
        .get_dependency(dependency_id)
        .unwrap()
        .project;

    let mut errors = HashMap::<FileId, Vec<SimpleSpan>>::new();
    let mut to_update = HashMap::<VariableId, ResolvedVariableOutline>::new();

    for (var_id, var) in project.pool.variables.iter() {
        let name_resolver = workspace_read.resolver_by_file(dependency_id, var.file_id);
        let Variable { ty, .. } = &var.decl.ast;
        let ty = ty.as_ref().map(|ty| resolve_singular(ty, &name_resolver));
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
    let project_mut = &mut workspace
        .dependencies
        .get_dependency_mut(dependency_id)
        .unwrap()
        .project;
    for (var_id, outline) in to_update.into_iter() {
        if let Some(func_info) = project_mut.pool.variables.get_mut(var_id) {
            func_info.decl.outline = Some(outline);
        }
    }
    errors
}

pub fn resolve_struct_outline(
    workspace: &mut Workspace,
    dependency_id: DependencyId,
) -> HashMap<FileId, Vec<SimpleSpan>> {
    let project = &workspace.dependencies.get_dependency(dependency_id).unwrap().project;
    let workspace_read: &Workspace = workspace;
    let mut errors = HashMap::<FileId, Vec<SimpleSpan>>::new();
    let mut to_update = HashMap::<StructId, ResolvedStructOutline>::new();

    for (struct_id, strct) in project.pool.structs.iter() {
        let name_resolver =
            workspace_read.resolver_by_file(dependency_id, strct.file_id);
        let (Struct { fields, .. }, _) = &strct.decl.ast;
        let fields = resolve_multiple(fields.iter().map(|field| &field.ty), &name_resolver);
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
    let project_mut = &mut workspace
        .dependencies
        .get_dependency_mut(dependency_id)
        .unwrap()
        .project;
    for (struct_id, outline) in to_update.into_iter() {
        if let Some(func_info) = project_mut.pool.structs.get_mut(struct_id) {
            func_info.decl.outline = Some(outline);
        }
    }
    errors
}

fn resolve_singular(
    ty: &Spanned<ParsedType>,
    name_resolver: &FileScopedNameResolver,
) -> Result<TypeId, SimpleSpan> {
    let Spanned(t, s) = ty;
    let name_for_search = *match t {
        ParsedType::Data(Spanned(name_for_search, _)) => name_for_search,
        ParsedType::Array(_) => todo!("array type resolution"),
    };
    name_resolver.resolve_type(name_for_search).ok_or(*s)
}

fn resolve_multiple<'a, I: Iterator<Item = &'a Spanned<ParsedType>>>(
    fields: I,
    name_resolver: &FileScopedNameResolver,
) -> Box<[Result<TypeId, SimpleSpan>]> {
    fields
        .map(|Spanned(ty, span)| match ty {
            ParsedType::Data(name_to_find) => {
                name_resolver.resolve_type(name_to_find.0).ok_or(*span)
            }
            ParsedType::Array(_) => todo!("array type resolution"),
        })
        .collect_vec()
        .into_boxed_slice()
}
