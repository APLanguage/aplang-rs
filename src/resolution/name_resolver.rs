use std::collections::HashMap;

use either::Either::{Left, Right};
use itertools::Itertools;
use lasso::Rodeo;
use logos::Source;

use crate::parsing::ast::declarations::FlatUseDeclaration;

use crate::project::{
    ModuleId, ResolvedUses, UseTarget, UseTargetSingle, UseTargetStar, Workspace,
};

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
