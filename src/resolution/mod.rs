use std::cell::RefCell;
use std::rc::Rc;

use either::Either::{self, Left, Right};
use lasso::{Rodeo, Spur};

use crate::parsing::ast::ParsedType;
use crate::parsing::Spanned;
use crate::project::scopes::ScopeId;
use crate::project::{
    FileId, FunctionId, ProjectLink, ResolvedUses, StructId,
    TypeRegistry, Workspace,
};
use crate::typing::{PrimitiveType, Type, TypeId};

pub mod fir;
pub mod name_resolution;
pub mod type_resolution;

pub struct FileScopedNameResolver<'a> {
    pub workspace: &'a Workspace,
    pub type_registery: Rc<RefCell<TypeRegistry>>,
    pub project_link: ProjectLink,
    pub scope_id: ScopeId,
    pub file_id: FileId,
    pub resolved_uses: &'a ResolvedUses,
    pub rodeo: &'a Rodeo,
}
impl<'a> FileScopedNameResolver<'a> {
    pub fn new_with_scope(
        workspace: &'a Workspace,
        types: Rc<RefCell<TypeRegistry>>,
        project_link: ProjectLink,
        scope_id: ScopeId,
        rodeo: &'a Rodeo,
    ) -> FileScopedNameResolver<'a> {
        let file_id = workspace
            .get_scopes(project_link)
            .scope_as_file(scope_id)
            .unwrap();
        let resolved_uses = workspace
            .get_project(project_link)
            .src
            .get_module_by_file(file_id)
            .unwrap()
            .1
            .imports
            .as_ref()
            .expect_right("should have been resolved");
        Self {
            workspace,
            type_registery: types,
            project_link,
            scope_id,
            file_id,
            resolved_uses,
            rodeo,
        }
    }

    pub fn new_with_file(
        workspace: &'a Workspace,
        types: Rc<RefCell<TypeRegistry>>,
        project_link: ProjectLink,
        file_id: FileId,
        rodeo: &'a Rodeo,
    ) -> FileScopedNameResolver<'a> {
        let scope_id = workspace
            .get_scopes(project_link)
            .scope_of_file(file_id)
            .unwrap();
        let resolved_uses = workspace
            .get_project(project_link)
            .src
            .get_module_by_file(file_id)
            .unwrap()
            .1
            .imports
            .as_ref()
            .expect_right("should have been resolved");
        Self {
            workspace,
            type_registery: types,
            project_link,
            scope_id,
            file_id,
            resolved_uses,
            rodeo,
        }
    }

    pub fn resolve_type(&self, parsed_type: &ParsedType) -> Result<TypeId, TypeId> {
        let scopes = self.workspace.get_scopes(self.project_link);
        let scope = self.scope_id;

        let (name_for_search, _span) = match parsed_type {
            ParsedType::Data(Spanned(name_for_search, span)) => (*name_for_search, *span),
            ParsedType::Array(t) => {
                return Ok(self.type_registery.borrow_mut().register_type(Type::Array {
                    ty: self.resolve_type(t)?,
                    size: None,
                }))
            }
        };

        scopes
            .scope_children_by_name(scope, name_for_search)
            .into_iter()
            .flatten()
            .filter_map(|scope_id| {
                scopes
                    .scope_as_declaration(scope_id)
                    .and_then(|(_name, dec_id)| {
                        self.workspace
                            .get_project(self.project_link)
                            .pool
                            .type_id_of_declaration(dec_id)
                    })
            })
            .chain(
                self.resolved_uses
                    .find_struct_in_single(self.workspace, name_for_search)
                    .map(|(.., type_id)| type_id)
                    .chain(self.find_in_primitives(name_for_search))
                    .chain(
                        self.resolved_uses
                            .find_struct_in_stars(self.workspace, name_for_search)
                            .map(|(.., type_id)| type_id),
                    ),
            )
            .next()
            .ok_or_else(|| {
                let ty_reg = &mut self.type_registery.borrow_mut();
                ty_reg
                    .primitive_by_spur(name_for_search)
                    .map(|(ty, _)| ty)
                    .unwrap_or_else(|| {
                        ty_reg.register_type(Type::Error)
                    })
            })
    }

    fn find_in_primitives(&self, name_for_search: Spur) -> Option<TypeId> {
        self.type_registery
            .borrow()
            .primitive_by_spur(name_for_search)
            .map(|(id, _)| id)
    }

    fn resolve_primitive(&self, primitive: PrimitiveType) -> TypeId {
        self.type_registery
            .borrow()
            .get_by_primitive_type(primitive)
    }

    fn register_type(&self, ty: Type) -> TypeId {
        self.type_registery.borrow_mut().register_type(ty)
    }

    fn resolve_struct(
        &'a self,
        identifier: Spur,
    ) -> impl Iterator<Item = (ProjectLink, StructId)> + 'a {
        self.resolve_struct_in_file(identifier)
            .chain(self.resolve_struct_in_uses(identifier))
    }

    fn resolve_func(
        &'a self,
        identifier: Spur,
    ) -> impl Iterator<Item = (ProjectLink, FunctionId)> + 'a {
        self.resolve_func_in_file(identifier)
            .chain(self.resolve_func_in_uses(identifier))
    }

    fn resolve_struct_in_file(
        &'a self,
        identifier: Spur,
    ) -> impl Iterator<Item = (ProjectLink, StructId)> + 'a {
        let scopes = self.workspace.get_scopes(self.project_link);
        scopes
            .scope_children_by_name(self.scope_id, identifier)
            .into_iter()
            .flatten()
            .filter_map(|scope_id| {
                scopes
                    .scope_as_declaration(scope_id)
                    .and_then(|(_name, dec_id)| {
                        self.workspace
                            .get_project(self.project_link)
                            .pool
                            .declaration_id_as_struct(dec_id)
                            .map(|strct_id| (self.project_link, strct_id))
                    })
            })
    }

    fn resolve_struct_in_uses(
        &'a self,
        identifier: Spur,
    ) -> impl Iterator<Item = (ProjectLink, StructId)> + 'a {
        self.resolved_uses
            .find(identifier)
            .filter_map(|(project_link, sp_id)| {
                let project = &self.workspace.get_project(project_link);
                Some((
                    project_link,
                    project
                        .pool
                        .declaration_id_as_struct(project.scopes.scope_as_declaration(sp_id)?.1)?,
                ))
            })
    }

    fn resolve_func_in_file(
        &'a self,
        identifier: Spur,
    ) -> impl Iterator<Item = (ProjectLink, FunctionId)> + 'a {
        let scopes = self.workspace.get_scopes(self.project_link);
        scopes
            .scope_children_by_name(self.scope_id, identifier)
            .into_iter()
            .flatten()
            .filter_map(|scope_id| {
                scopes
                    .scope_as_declaration(scope_id)
                    .and_then(|(_name, dec_id)| {
                        self.workspace
                            .get_project(self.project_link)
                            .pool
                            .declaration_id_as_func(dec_id)
                            .map(|func_id| (self.project_link, func_id))
                    })
            })
    }

    fn resolve_func_in_uses(
        &'a self,
        identifier: Spur,
    ) -> impl Iterator<Item = (ProjectLink, FunctionId)> + 'a {
        self.resolved_uses
            .find(identifier)
            .filter_map(|(project_link, sp_id)| {
                let project = self.workspace.get_project(project_link);
                Some((
                    project_link,
                    project
                        .pool
                        .declaration_id_as_func(project.scopes.scope_as_declaration(sp_id)?.1)?,
                ))
            })
    }

    fn resolve_callable(
        &'a self,
        identifier: Spur,
    ) -> impl Iterator<Item = (ProjectLink, Either<FunctionId, StructId>)> + 'a {
        let scopes = self.workspace.get_scopes(self.project_link);
        scopes
            .scope_children_by_name(self.scope_id, identifier)
            .into_iter()
            .flatten()
            .filter_map(|scope_id| {
                scopes
                    .scope_as_declaration(scope_id)
                    .and_then(|(_name, dec_id)| {
                        let declaration_pool = &self.workspace.get_project(self.project_link).pool;
                        declaration_pool
                            .declaration_id_as_func(dec_id)
                            .map(|func_id| (self.project_link, Left(func_id)))
                            .or_else(|| {
                                declaration_pool
                                    .declaration_id_as_struct(dec_id)
                                    .map(|strct_id| (self.project_link, Right(strct_id)))
                            })
                    })
            })
            .chain(
                self.resolved_uses
                    .find(identifier)
                    .filter_map(|(project_link, sp_id)| {
                        let project = &self.workspace.get_project(project_link);
                        Some((project_link, {
                            let dec_id = project.scopes.scope_as_declaration(sp_id)?.1;
                            project
                                .pool
                                .declaration_id_as_func(dec_id)
                                .map(Left)
                                .or_else(|| {
                                    project.pool.declaration_id_as_struct(dec_id).map(Right)
                                })?
                        }))
                    }),
            )
    }
}
