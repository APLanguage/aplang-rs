#![allow(dead_code)]
#![allow(unused)]
#![allow(clippy::needless_pass_by_ref_mut)]
// TODO: remove all 3
use std::{collections::HashMap, ops::ControlFlow, vec::Vec};

use chumsky::span::{SimpleSpan, Span};
use itertools::Itertools;
use lasso::Spur;
use slotmap::SlotMap;

use crate::{
    parsing::{
        ast::{self, declarations::Parameter, expressions::CallKind},
        parsers,
        tokenizer::{Operation, OperationGroup},
        Infoed, Spanned,
    },
    project::{
        DeclarationInfo, DeclarationResolutionStage, DependencyId, FileId, FunctionId, StructId,
        Workspace,
    },
    resolution::fir::AssignableTarget,
    typing::{self, typedast::Variable, PrimitiveType, Type, TypeId},
};

use super::{
    fir::{self, LocalVarId, VariableType},
    name_resolution::ResolvedFunctionOutline,
    FileScopedNameResolver,
};

pub fn resolve_and_typecheck_functions(
    workspace: &mut Workspace,
    dependency_id: DependencyId,
) -> HashMap<FileId, Vec<Spanned<TypeResError>>> {
    let workspace_read: &Workspace = workspace;
    let mut errors = HashMap::new();
    for (func_id, func_info) in workspace_read
        .dependencies
        .get_dependency(dependency_id)
        .unwrap()
        .project
        .pool
        .functions
        .iter()
    {
        let (func, errs) = resolve_function(
            workspace_read.resolver_by_file(dependency_id, func_info.file_id),
            func_id,
            func_info,
        );
        errs.into_iter()
            .collect_into(errors.entry(func_info.file_id).or_insert_with(Vec::new));
    }
    errors
}

fn resolve_function(
    resolver: FileScopedNameResolver,
    func_id: FunctionId,
    func_info: &DeclarationInfo<ast::declarations::Function, ResolvedFunctionOutline>,
) -> (fir::Function, Vec<Spanned<TypeResError>>) {
    let mut env = ResolutionEnv::new(resolver, func_id, func_info);
    let statements = func_info
        .decl
        .ast
        .statements
        .iter()
        .map(|stmt| env.resolve_statement(stmt))
        .collect_vec()
        .into_boxed_slice();
    let ResolutionEnv { locals, errors, .. } = env;
    (fir::Function { locals, statements }, errors)
}

#[derive(Debug)]
pub enum TypeResError {
    FunctionNotFound(Spanned<Spur>, Vec<Spanned<TypeId>>),
    VariableNotFound(Spanned<Spur>),
    TypesAreNotMatching(Spanned<TypeId>, Spanned<TypeId>),
}

struct ResolutionEnv<'a> {
    locals: SlotMap<LocalVarId, fir::Variable>,
    var_scopes: Vec<Vec<(Spur, LocalVarId)>>,
    resolver: FileScopedNameResolver<'a>,
    func_id: FunctionId,
    func_info: &'a DeclarationInfo<ast::declarations::Function, ResolvedFunctionOutline>,
    func_info_outline: &'a ResolvedFunctionOutline,
    errors: Vec<Spanned<TypeResError>>,
}

impl<'a> ResolutionEnv<'a> {
    fn new(
        resolver: FileScopedNameResolver<'a>,
        func_id: FunctionId,
        func_info: &'a DeclarationInfo<ast::declarations::Function, ResolvedFunctionOutline>,
    ) -> Self {
        Self {
            locals: SlotMap::with_key(),
            var_scopes: vec![vec![]],
            resolver,
            func_id,
            func_info,
            func_info_outline: func_info
                .decl
                .outline
                .as_ref()
                .expect("The outline should have been resolved."),
            errors: vec![],
        }
    }

    pub fn resolve_primitive(&self, primitive: PrimitiveType) -> TypeId {
        self.resolver.resolve_primitive(primitive)
    }

    fn resolve_type(&self, span: Type) -> TypeId {
        self.resolver.register_type(span)
    }

    fn resolve_statement(
        &mut self,
        spanned_stmt: &Spanned<ast::statements::Statement>,
    ) -> fir::Statement {
        let Spanned(stmt, span) = spanned_stmt;
        match stmt {
            ast::statements::Statement::Expression(expr) => {
                fir::Statement::Expression(self.resolve_expression(None, expr, *span))
            }
            ast::statements::Statement::ControlFlow(_) => todo!("Statement::ControlFlow"),
            ast::statements::Statement::Declaration(delr) => self.resolve_declaration(delr),
            ast::statements::Statement::None => todo!("Statement::None"),
        }
    }

    fn resolve_expression(
        &mut self,
        try_to_be: Option<TypeId>,
        expr: &ast::expressions::Expression,
        span: SimpleSpan,
    ) -> Infoed<fir::Expression> {
        let (ty, expr) = match expr {
            ast::expressions::Expression::StringLiteral(l) => (
                self.resolve_primitive(PrimitiveType::String),
                fir::Expression::StringLiteral(*l),
            ),
            ast::expressions::Expression::Number(rslt) => self.resolve_number_literal(rslt, span),
            ast::expressions::Expression::Call(call_kind) => {
                self.resolve_call_kind(span, None, call_kind)
            }

            ast::expressions::Expression::Unary { ops, expression } => todo!("Expression::Unary"),
            ast::expressions::Expression::If {
                condition,
                then,
                other,
            } => todo!("Expression::If"),
            ast::expressions::Expression::CallChain { expression, calls } => {
                self.resolve_call_chain(expression, calls)
            }
            ast::expressions::Expression::OperationChain {
                base,
                continuation,
                group,
            } => self.resolve_operation_chain(base, continuation, *group),
            ast::expressions::Expression::Assignement {
                call,
                op,
                expression,
            } => self.resolve_assignement(call, *op, expression),
            ast::expressions::Expression::Binary {
                lhs,
                op,
                rhs,
                group,
            } => self.resolve_binary(lhs.as_ref(), op.to_owned(), rhs.as_ref(), *group),
            // _ => todo!(),
        };
        Infoed {
            inner: expr,
            info: ty,
            span,
        }
    }

    fn resolve_call_chain(
        &mut self,
        expression: &Spanned<ast::expressions::Expression>,
        calls: &[Spanned<CallKind>],
    ) -> (TypeId, fir::Expression) {
        let Spanned(base, span) = expression;
        let base = self.resolve_expression(None, base, *span);
        let mut this = base.info;
        let mut the_calls = vec![];
        for call in calls.iter() {
            let Some((dep, struct_id)) = self.resolver.type_registery.borrow().get_as_struct(this)
            else {
                todo!("Expression::CallChain/handle errors")
            };
            let Spanned(CallKind::Identifier(Spanned(name, _)), span) = call else {
                todo!("Expression::CallChain/methods")
            };
            let Some(fid) = self.find_field_in_struct(dep, struct_id, *name) else {
                todo!("Expression::CallChain/handle not founds")
            };
            this = self.resolver
            .dependencies
            .get_dependency(dep)
            .and_then(|dep| {
                Some(
                    dep.project
                        .pool
                        .structs
                        .get(struct_id)?
                        .decl
                        .outline
                        .as_ref()?
                        .fields
                        .get(fid)?
                        .unwrap_or_else(|t| t),
                )
            }).expect("should have been resolved and the the siue of both ast and outline should match");
            the_calls.push((
                Spanned(fir::CallKind::StructField(dep, struct_id, fid), *span),
                this,
            ));
        }
        (
            the_calls.last().expect("should have at least one call").1,
            fir::Expression::CallChain {
                expression: Box::new(base),
                calls: the_calls
                    .into_iter()
                    .map(|(ck, _)| ck)
                    .collect_vec()
                    .into_boxed_slice(),
            },
        )
    }

    fn find_field_in_struct(
        &mut self,
        dep: DependencyId,
        struct_id: StructId,
        name: Spur,
    ) -> Option<usize> {
        self.resolver
            .dependencies
            .get_dependency(dep)
            .unwrap()
            .project
            .pool
            .structs
            .get(struct_id)
            .unwrap()
            .decl
            .ast
            .0
            .fields
            .iter()
            .enumerate()
            .find(|(_, f)| f.name.0 == name)
            .map(|t| t.0)
    }

    fn resolve_call_kind(
        &mut self,
        span: SimpleSpan,
        try_to_be: Option<TypeId>,
        call_kind: &ast::expressions::CallKind,
    ) -> (TypeId, fir::Expression) {
        match call_kind {
            ast::expressions::CallKind::Identifier(name) => {
                if let Some((ty, target)) = self.find_var(name.0) {
                    (
                        ty,
                        fir::Expression::Call(Spanned(fir::CallKind::Variable(target), span)),
                    )
                } else {
                    self.add_error(TypeResError::VariableNotFound(name.to_owned()), span);
                    (
                        self.resolver
                            .type_registery
                            .borrow_mut()
                            .register_type(Type::Error(
                                self.func_info.file_id,
                                span,
                                "unresolved variable",
                            )),
                        fir::Expression::Call(Spanned(fir::CallKind::Unresolved, span)),
                    )
                }
            }
            ast::expressions::CallKind::Call {
                identifier,
                parameters,
            } => {
                let parameters = parameters
                    .iter()
                    .map(|Spanned(expr, span)| self.resolve_expression(None, expr, *span))
                    .collect_vec()
                    .into_boxed_slice();
                let Some((dep, f_id, ret_ty)) = self
                    .find_fn(
                        identifier.0,
                        &parameters.iter().map(|i| i.info).collect_vec(),
                    )
                    .next()
                else {
                    self.add_error(
                        TypeResError::FunctionNotFound(
                            identifier.to_owned(),
                            parameters
                                .iter()
                                .map(|t| t.as_spanned_info().into())
                                .collect_vec(),
                        ),
                        span,
                    );
                    return (
                        self.resolver
                            .type_registery
                            .borrow_mut()
                            .register_type(Type::Error(
                                self.func_info.file_id,
                                span,
                                "unresolved function",
                            )),
                        fir::Expression::Call(Spanned(fir::CallKind::Unresolved, span)),
                    );
                };

                (
                    ret_ty.map(|r| r.unwrap_or_else(|t| t)).unwrap_or_else(|| {
                        self.resolver
                            .type_registery
                            .borrow_mut()
                            .get_by_primitive_type(PrimitiveType::Unit)
                    }),
                    fir::Expression::Call(Spanned(
                        fir::CallKind::Function {
                            dependency_id: dep,
                            fn_id: f_id,
                            parameters,
                        },
                        span,
                    )),
                )
            }
        }
    }

    fn resolve_number_literal(
        &mut self,
        rslt: &Result<parsers::number::NumberLiteral, parsers::number::NumberLiteralError>,
        span: SimpleSpan,
    ) -> (TypeId, fir::Expression) {
        match rslt {
            Ok(lit) => self.resolve_number_literal_ok(lit),
            Err(_) => (
                self.resolve_type(Type::Error(
                    self.func_info.file_id,
                    span,
                    "error resolving broken number literal",
                )),
                fir::Expression::Number(rslt.clone()),
            ),
        }
    }

    fn resolve_number_literal_ok(
        &mut self,
        lit: &parsers::number::NumberLiteral,
    ) -> (TypeId, fir::Expression) {
        let expr = fir::Expression::Number(Ok(lit.clone()));
        let ty = match lit {
            parsers::number::NumberLiteral::Unsigned(_, w) => {
                self.resolve_primitive(PrimitiveType::Integer(false, (*w).into()))
            }
            parsers::number::NumberLiteral::Signed(_, w) => {
                self.resolve_primitive(PrimitiveType::Integer(true, (*w).into()))
            }
            parsers::number::NumberLiteral::Float(_, w) => {
                self.resolve_primitive(PrimitiveType::Float((*w).into()))
            }
            parsers::number::NumberLiteral::Inferred {
                unsigned,
                signed,
                float,
            } => todo!("NumberLiteral::Inferred"),
        };
        (ty, expr)
    }

    fn resolve_operation_chain(
        &mut self,
        base: &Spanned<ast::expressions::Expression>,
        continuation: &[(Spanned<Operation>, Spanned<ast::expressions::Expression>)],
        group: OperationGroup,
    ) -> (TypeId, fir::Expression) {
        let (is, try_to_be, base) = match group {
            OperationGroup::Equality | OperationGroup::Comparison => {
                let Spanned(expr, span) = base;
                (
                    self.resolver.resolve_primitive(PrimitiveType::Boolean),
                    None,
                    self.resolve_expression(None, expr, *span),
                )
            }
            _ => unreachable!(),
        };
        let continuation = continuation
            .iter()
            .map(|(op, Spanned(expr, span))| (*op, self.resolve_expression(try_to_be, expr, *span)))
            .collect_vec()
            .into_boxed_slice();
        (
            base.info,
            fir::Expression::OperationChain {
                base: base.into(),
                continuation,
                group,
            },
        )
    }

    fn find_fn<'b: 'a>(
        &'a self,
        identifier: lasso::Spur,
        parameters: &'b [TypeId],
    ) -> impl Iterator<Item = (DependencyId, FunctionId, Option<Result<TypeId, TypeId>>)> + 'a {
        let iter = self
            .resolver
            .resolve_fn(identifier)
            .filter_map(move |(dep, f_id)| {
                let DeclarationInfo {
                    decl: DeclarationResolutionStage { outline, .. },
                    file_id,
                } = self
                    .resolver
                    .dependencies
                    .get_dependency(dep)?
                    .project
                    .pool
                    .functions
                    .get(f_id)?;
                let Some(ResolvedFunctionOutline {
                    parameters: resolved_parameters,
                    ret_ty,
                }) = outline
                else {
                    return None;
                };
                // println!(
                //     "testing {:?}: ({}) vs ({})",
                //     identifier.to_owned(),
                //     parameters
                //         .iter()
                //         .map(|ty| self.resolver.type_registery.borrow().display_type(*ty))
                //         .join(", "),
                //     resolved_parameters
                //         .iter()
                //         .map(|ty| self
                //             .resolver
                //             .type_registery
                //             .borrow()
                //             .display_type(ty.unwrap_or_else(|t| t)))
                //         .join(", ")
                // );
                if parameters.len() != resolved_parameters.len() {
                    // println!("len not matched");
                    return None;
                }
                if parameters
                    .iter()
                    .zip_eq(resolved_parameters.iter())
                    .any(|(a, b)| match b.to_owned() {
                        Ok(t) => *a != t,
                        Err(_) => true,
                    })
                {
                    // println!("types not matching");
                    return None;
                }
                Some((dep, f_id, ret_ty.to_owned()))
            });
        if parameters.iter().any(|type_id| {
            matches!(
                self.resolver.type_registery.borrow().get(*type_id),
                Some(Type::Unknown(_, _) | Type::Error(_, _, _)) | None
            )
        }) {
            iter.take(0)
        } else {
            iter.take(usize::MAX)
        }
    }

    fn resolve_binary(
        &mut self,
        lhs: &Spanned<ast::expressions::Expression>,
        op: Spanned<Operation>,
        rhs: &Spanned<ast::expressions::Expression>,
        group: OperationGroup,
    ) -> (TypeId, fir::Expression) {
        let Spanned(lhs, lhs_span) = lhs;
        let Spanned(rhs, rhs_span) = rhs;
        let lhs = self.resolve_expression(None, lhs, *lhs_span);
        let rhs = self.resolve_expression(None, rhs, *rhs_span);
        let lhs_get_as_primitive = self
            .resolver
            .type_registery
            .borrow()
            .get_as_primitive(lhs.info);
        let rhs_get_as_primitive = self
            .resolver
            .type_registery
            .borrow()
            .get_as_primitive(rhs.info);
        use OperationGroup::*;
        let ty = match (group, op.0, (lhs_get_as_primitive, rhs_get_as_primitive)) {
            (_, _, (None, None)) => todo!("report being non-primitive"),
            (_, _, (None, Some(_))) => todo!("report being rhs non-primitive"),
            (_, _, (Some(_), None)) => todo!("report being lhs non-primitive"),
            (
                Term,
                Operation::Addition,
                (Some(PrimitiveType::String), _) | (_, Some(PrimitiveType::String)),
            ) => self.resolve_primitive(PrimitiveType::String),
            (
                Term,
                _,
                (Some(PrimitiveType::Integer(true, w1)), Some(PrimitiveType::Integer(true, w2))),
            ) => self.resolve_primitive(PrimitiveType::Integer(true, w1.max(w2))),
            (
                Term,
                _,
                (Some(PrimitiveType::Integer(false, w1)), Some(PrimitiveType::Integer(false, w2))),
            ) => self.resolve_primitive(PrimitiveType::Integer(false, w1.max(w2))),
            _ => todo!("resolve_binary/other ops"),
        };
        (
            ty,
            fir::Expression::Binary {
                lhs: lhs.into(),
                op,
                rhs: rhs.into(),
                group,
            },
        )
    }

    fn find_local_var(&self, name: Spur) -> Option<LocalVarId> {
        for scope in self.var_scopes.iter().rev() {
            for (var_name, id) in scope.iter().rev() {
                if *var_name == name {
                    return Some(*id);
                }
            }
        }
        None
    }

    fn find_param(&self, name: Spur) -> Option<usize> {
        for (
            x,
            Parameter {
                name: Spanned(param_name, _),
                ..
            },
        ) in self.func_info.decl.ast.parameters.iter().enumerate()
        {
            if *param_name == name {
                return Some(x);
            }
        }
        None
    }

    fn find_var(&self, name: Spur) -> Option<(TypeId, VariableType)> {
        self.find_local_var(name)
            .and_then(|id| self.locals.get(id).map(|v| (v.ty, VariableType::Local(id))))
            .or_else(|| {
                self.find_param(name).and_then(|id| {
                    self.func_info_outline
                        .parameters
                        .get(id)
                        .map(|ty_res| (ty_res.unwrap_or_else(|ty| ty), VariableType::Parameter(id)))
                })
            })
    }

    fn add_error(&mut self, error: TypeResError, span: SimpleSpan) {
        self.errors.push(Spanned(error, span))
    }

    fn resolve_declaration(&mut self, delr: &ast::declarations::Declaration) -> fir::Statement {
        match delr {
            ast::declarations::Declaration::Variable(var) => {
                let try_to_be = var
                    .ty
                    .as_ref()
                    .map(|Spanned(t, span)| self.resolver.resolve_type(t).unwrap_or_else(|t| t));
                let Spanned(expr, expr_span) = &var.expression;
                let expr = self.resolve_expression(try_to_be, expr, *expr_span);
                let var_id = self.locals.insert(fir::Variable {
                    reassignable: var.reassignable,
                    name: var.name,
                    ty: expr.info,
                });
                self.var_scopes
                    .last_mut()
                    .expect("at least one scope should exist")
                    .push((var.name.0, var_id));
                fir::Statement::VariableDeclaration { var_id, expr }
            }
            _ => todo!("resolve_declaration/other declarations"),
        }
    }
    fn resolve_assignement(
        &mut self,
        call: &Spanned<ast::expressions::Expression>,
        op: Spanned<Operation>,
        expression: &Spanned<ast::expressions::Expression>,
    ) -> (TypeId, fir::Expression) {
        let Spanned(call, call_span) = call;
        let Spanned(op, op_span) = op;
        let Spanned(expr, expr_span) = expression;
        match call {
            ast::expressions::Expression::CallChain { expression, calls } => {
                todo!("Expression::Assignement/Expression::CallChain")
            }
            ast::expressions::Expression::Call(CallKind::Identifier(Spanned(name, var_span))) => {
                let Some((ty, var_ty)) = self.find_var(*name) else {
                    todo!("Expression::Assignement/Expression::Call/not found")
                };
                let Infoed {
                    inner: fir_expr,
                    info: expr_ty,
                    ..
                } = self.resolve_expression(Some(ty), expr, *expr_span);
                if ty != expr_ty {
                    self.add_error(
                        TypeResError::TypesAreNotMatching(
                            Spanned(ty, *var_span),
                            Spanned(expr_ty, *expr_span),
                        ),
                        (call_span.start..expr_span.end).into(),
                    );
                    (
                        ty,
                        fir::Expression::Assignement {
                            call: Spanned(AssignableTarget::Var(ty, var_ty), *call_span),
                            op: Spanned(op, op_span),
                            expression: Box::new(Infoed {
                                inner: fir_expr,
                                info: expr_ty,
                                span: *expr_span,
                            }),
                        },
                    )
                } else {
                    (
                        expr_ty,
                        fir::Expression::Assignement {
                            call: Spanned(AssignableTarget::Var(ty, var_ty), *call_span),
                            op: Spanned(op, op_span),
                            expression: Box::new(Infoed {
                                inner: fir_expr,
                                info: expr_ty,
                                span: *expr_span,
                            }),
                        },
                    )
                }
            }
            _ => todo!("Expression::Assignement/handle wrong lhs"),
        }
    }
}
