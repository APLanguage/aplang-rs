use std::num::NonZeroUsize;

use aplang_parser::parsers::number::LiteralWidth;
use slotmap::new_key_type;
use strum_macros::Display;

new_key_type! { pub struct TypeId; }

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Type {
    PrimitiveType(PrimitiveType),
    Array {
        ty: TypeId,
        size: Option<NonZeroUsize>,
    },
    Ref(TypeId),
    Error,
}

#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash, Display)]
pub enum PrimitiveType {
    String,
    Integer(bool, LiteralWidth),
    Float(LiteralWidth),
    Boolean,
    Unit,
    Nothing,
}