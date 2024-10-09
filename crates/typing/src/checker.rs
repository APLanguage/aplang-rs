use crate::{ty::TypeId, reg::TypeRegistry};

pub enum UnifyResult {
    Success
}

fn unify(reg: TypeRegistry, a: TypeId, b: TypeId) -> UnifyResult {
    todo!("unify");
}