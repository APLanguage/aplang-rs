use std::collections::HashMap;

use either::Either::{Left, Right};
use itertools::Itertools;
use lasso::Rodeo;
use logos::Source;

use super::{ModuleId, ResolvedUses, Workspace};

fn resolve_uses(rodeo: &mut Rodeo, workspace: &mut Workspace) {
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
            let Some(dependency_id) = workspace.dependencies.get_dependency_by_name(
                &u.scope
                    .iter()
                    .map(|s| {
                        rodeo.get_or_intern(
                            src.slice(s.into_range())
                                .expect("The span should be correct"),
                        )
                    })
                    .collect_vec(),
            ) else {
                panic!("the dependency should exist 🤨 (well, no, there is no error handling yet)")
            };

            let scopes = workspace
                .dependencies
                .get_dependency_scope(dependency_id)
                .unwrap();
            let root = scopes.root_id();

            for (flat_spans, star) in u.flatten_tree() {
                match scopes.scope_child_by_path(
                    root,
                    &flat_spans
                        .iter()
                        .map(|s| rodeo.get_or_intern(src.slice(s.into_range()).unwrap()))
                        .collect_vec(),
                ) {
                    Ok(scope_id) => {
                        resolved.add(dependency_id, scope_id, star);
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
