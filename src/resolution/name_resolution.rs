use std::collections::HashMap;

use chumsky::span::SimpleSpan;
use either::Either::{Left, Right};
use itertools::Itertools;
use lasso::Rodeo;
use logos::Source;

use crate::{
    parsing::{
        ast::{
            declarations::{FlatUseDeclaration, Function, Struct, Variable},
            ParsedType,
        },
        Spanned,
    },
    project::{
        DependencyId, FileId, FunctionId, ModuleId, ResolvedUses, StructId, UseTarget,
        UseTargetSingle, UseTargetStar, VariableId, Workspace,
    },
    typing::TypeId,
};

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
                        errors
                            .entry(module.file_id)
                            .or_insert_with(std::vec::Vec::new)
                            .push(flat_spans[i]);
                    }
                }
            }
        }
        resolutions.insert(module_id, resolved);
    }
    let project = &mut workspace
        .dependencies
        .get_dependency_mut(dependency_id)
        .unwrap()
        .project;
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
    pub parameters: Box<[Result<TypeId, TypeId>]>,
    pub ret_ty: Option<Result<TypeId, TypeId>>,
}

#[derive(Debug)]
pub struct ResolvedVariableOutline {
    ty: Option<Result<TypeId, TypeId>>,
}

#[derive(Debug)]
pub struct ResolvedStructOutline {
    pub fields: Box<[Result<TypeId, TypeId>]>,
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
        v.into_iter()
            .collect_into(errs.entry(k).or_insert_with(Vec::new));
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
        let parameters: Box<[Result<TypeId, TypeId>]> =
            resolve_multiple(parameters.iter().map(|param| &param.ty), &name_resolver);
        let ret_ty = ty.as_ref().map(|ty| resolve_singular(ty, &name_resolver));
        for e in parameters
            .iter()
            .filter_map(|r| r.err())
            .chain(ret_ty.and_then(|r| r.err().to_owned()))
        {
            let e = name_resolver
                .type_registery
                .borrow()
                .get_as_unknown(e)
                .unwrap()
                .1
                 .1;
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
            let e = name_resolver
                .type_registery
                .borrow()
                .get_as_unknown(e)
                .unwrap()
                .1
                 .1;
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
    let project = &workspace
        .dependencies
        .get_dependency(dependency_id)
        .unwrap()
        .project;
    let workspace_read: &Workspace = workspace;
    let mut errors = HashMap::<FileId, Vec<SimpleSpan>>::new();
    let mut to_update = HashMap::<StructId, ResolvedStructOutline>::new();

    for (struct_id, strct) in project.pool.structs.iter() {
        let name_resolver = workspace_read.resolver_by_file(dependency_id, strct.file_id);
        let (Struct { fields, .. }, _) = &strct.decl.ast;
        let fields = resolve_multiple(fields.iter().map(|field| &field.ty), &name_resolver);
        for e in fields.iter().filter_map(|r| r.err()) {
            let e = name_resolver
                .type_registery
                .borrow()
                .get_as_unknown(e)
                .unwrap()
                .1
                 .1;
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
) -> Result<TypeId, TypeId> {
    name_resolver.resolve_type(&ty.0)
}

fn resolve_multiple<'a, I: Iterator<Item = &'a Spanned<ParsedType>>>(
    fields: I,
    name_resolver: &FileScopedNameResolver,
) -> Box<[Result<TypeId, TypeId>]> {
    fields
        .map(|ty| name_resolver.resolve_type(&ty.0))
        .collect_vec()
        .into_boxed_slice()
}
