use aplang_parser::ast::ParsedType;

use crate::ty::TypeId;

pub struct ContextfulTypeResolver;

impl ContextfulTypeResolver {

    fn type_id_of(&self, ty: ParsedType) -> TypeId {
        todo!("ContextfulTypeResolver::type_id_of")
    }
}