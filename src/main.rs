#![allow(dead_code)]

use std::collections::HashMap;
use parsing::literals::number::NumberLiteral;

enum Type {
    I32,
    String,
    Struct(String),
    Array(Box<Type>),
}

enum BinaryOperation {
    Addition,
    Substraction,
    Multiplication,
    Division,
}

enum UnaryOperation {
    Negation,
}

enum Declaration {
    Data {
        name: String,
        fields: HashMap<String, Type>,
    },
    Function {
        name: String,
        return_type: Type,
        parameters: HashMap<String, Type>,
    },
    Var {
        name: String,
        expression: Option<Box<Expression>>,
    },
}


enum Expression {
    Number(NumberLiteral),
    String(String),
    FunctionCall {
        name: String,
        parameters: Vec<Expression>,
    },
    Binary {
        lhs: Box<Expression>,
        op: BinaryOperation,
        rhs: Box<Expression>,
    },
    Unary {
        op: UnaryOperation,
        expr: Box<Expression>,
    },
    Assignement {
        lhs: Option<Box<Expression>>,
        name: String,
        rhs: Box<Expression>,
    },
    If {
        condition: Box<Expression>,
        then: Box<Expression>,
        or: Box<Expression>,
    },
}

enum Statement {
    While {
        condition: Expression,
        statement: Option<Box<Statement>>,
    },
    If {
        condition: Expression,
        statement: Option<Box<Statement>>,
    },
    ExpressionStatement(Expression),
}



pub mod parsing;
fn main() {
    println!("Hello, world!");
}
