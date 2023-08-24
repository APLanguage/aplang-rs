use lasso::Spur;

use crate::project::scopes::ScopeId;
use crate::project::{Dependencies, DependencyId, FileId, ResolvedUses, TypeRegistry};
use crate::typing::TypeId;

pub mod name_resolution;

pub struct FileScopedNameResolver<'a> {
    pub dependencies: &'a Dependencies,
    pub type_registery: &'a TypeRegistry,
    pub dependency_id: DependencyId,
    pub scope_id: ScopeId,
    pub file_id: FileId,
    pub resolved_uses: &'a ResolvedUses,
}
impl<'a> FileScopedNameResolver<'a> {
    pub fn new_with_scope(
        deps: &'a Dependencies,
        types: &'a TypeRegistry,
        dependency_id: DependencyId,
        scope_id: ScopeId,
    ) -> FileScopedNameResolver<'a> {
        let file_id = deps
            .get_dependency_scopes(dependency_id)
            .unwrap()
            .scope_as_file(scope_id)
            .unwrap();
        let resolved_uses = deps
            .get_dependency(dependency_id)
            .unwrap()
            .project
            .src
            .get_module_by_file(file_id)
            .unwrap()
            .1
            .imports
            .as_ref()
            .expect_right("should have been resolved");
        Self {
            dependencies: deps,
            type_registery: types,
            dependency_id,
            scope_id,
            file_id,
            resolved_uses,
        }
    }

    pub fn new_with_file(
        deps: &'a Dependencies,
        types: &'a TypeRegistry,
        dependency_id: DependencyId,
        file_id: FileId,
    ) -> FileScopedNameResolver<'a> {
        let scope_id = deps
            .get_dependency_scopes(dependency_id)
            .unwrap()
            .scope_of_file(file_id)
            .unwrap();
        let resolved_uses = deps
            .get_dependency(dependency_id)
            .unwrap()
            .project
            .src
            .get_module_by_file(file_id)
            .unwrap()
            .1
            .imports
            .as_ref()
            .expect_right("should have been resolved");
        Self {
            dependencies: deps,
            type_registery: types,
            dependency_id,
            scope_id,
            file_id,
            resolved_uses,
        }
    }

    pub fn resolve_type(&self, name_for_search: Spur) -> Option<TypeId> {
        let dependency_id = self.dependency_id;
        let scopes = self.dependencies.get_dependency_scopes(dependency_id)?;
        let scope = self.scope_id;

        scopes
            .scope_children_by_name(scope, name_for_search)
            .into_iter()
            .flatten()
            .filter_map(|scope_id| {
                scopes
                    .scope_as_declaration(scope_id)
                    .and_then(|(_name, dec_id)| {
                        self.dependencies
                            .get_dependency(dependency_id)?
                            .project
                            .pool
                            .type_id_of_declaration(dec_id)
                    })
            })
            .chain(
                self.resolved_uses
                    .find_struct_in_single(self.dependencies, name_for_search)
                    .map(|(.., type_id)| type_id)
                    .chain(self.find_in_primitives(name_for_search))
                    .chain(
                        self.resolved_uses
                            .find_struct_in_stars(self.dependencies, name_for_search)
                            .map(|(.., type_id)| type_id),
                    ),
            )
            .next()
    }

    fn find_in_primitives(&self, name_for_search: Spur) -> Option<TypeId> {
        self.type_registery
            .primitive_by_spur(name_for_search)
            .map(|(id, _)| id)
    }
}
