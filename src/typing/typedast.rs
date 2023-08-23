use super::TypeId;

#[derive(Debug)]
pub struct NameTypeTuple {
    name: String,
    ty: TypeId,
}

#[derive(Debug)]
pub struct Struct {
    pub name: String,
    pub fields: Box<[NameTypeTuple]>,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub parameters: Box<[NameTypeTuple]>,
    pub ty: Option<TypeId>,
}

#[derive(Debug)]
pub struct Variable {
    pub name: String,
    pub ty: TypeId,
}
