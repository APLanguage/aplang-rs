use chumsky::{
    primitive::{choice, group, just},
    recursive::recursive,
    select,
    span::SimpleSpan,
    Parser,
};

use crate::parsing::{
    ast::expressions::{CallKind, Expression},
    parsers::number::parse_complex_number,
    parsers::{CollectBoxedSliceExt, ParserState, TokenInput, TokenParser},
    tokenizer::Identifier,
    tokenizer::{ident, keyword, Operation, OperationGroup, Token},
    Spanned,
};

macro_rules! ops_parser {
    ($($variant:ident), *) => {
        choice((
            $(
                just(Token::$variant).map(Into::<Operation>::into),
            )*
        ))
    };
}

pub fn call<'a, EP, I: TokenInput<'a>>(
    expr_parser: EP,
) -> impl TokenParser<'a, I, CallKind> + Clone
where EP: TokenParser<'a, I, Expression> + Clone + 'a {
    ident()
        .spur()
        .map_with_span(Spanned)
        .then(
            expr_parser
                .spanned()
                .paddedln()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .at_least(0)
                .collect_boxed_slice()
                .boxed()
                .delimited_by(just(Token::ParenOpen), just(Token::ParenClosed))
                .or_not()
                .boxed(),
        )
        .map(|(identifier, params)| match params {
            Some(parameters) => CallKind::Call {
                identifier,
                parameters,
            },
            None => CallKind::Identifier(identifier),
        })
        .labelled("call")
}

pub fn assignment_parser<'a, EP, I: TokenInput<'a>>(
    expr_parser: EP,
) -> impl TokenParser<'a, I, Expression> + Clone
where EP: TokenParser<'a, I, Expression> + Clone + 'a {
    group((
        atom_parser(expr_parser.clone())
            .spanned()
            .labelled("assignement_left")
            .map_into(),
        ops_parser!(
            Equal,
            PlusEqual,
            MinusEqual,
            SlashSlashEqual,
            SlashEqual,
            AsteriskAsteriskEqual,
            AsteriskEqual,
            PercentEqual,
            AmpersandEqual,
            BarEqual,
            CircumflexEqual,
            GreaterGreaterGreaterEqual,
            GreaterGreaterEqual,
            LessLessEqual
        )
        .boxed()
        .map_with_span(Spanned)
        .labelled("assignement_operator")
        .paddedln(),
        expr_parser
            .spanned()
            .labelled("assignement_right")
            .map_into(),
    ))
    .map(|(call, op, expression)| Expression::Assignement {
        call,
        op,
        expression,
    })
    .boxed()
    .labelled("assignement")
}

pub fn if_expr_parser<'a, EP, I: TokenInput<'a>>(
    expr_parser: EP,
) -> impl TokenParser<'a, I, Expression> + Clone
where EP: TokenParser<'a, I, Expression> + Clone + 'a {
    keyword(Identifier::If)
        .ignore_then(group((
            expr_parser
                .clone()
                .spanned()
                .paddedln()
                .delimited_by(just(Token::ParenOpen), just(Token::ParenClosed))
                .boxed()
                .map_into(),
            expr_parser.clone().spanned().paddedln().map_into(),
            keyword(Identifier::Else)
                .paddedln()
                .ignore_then(expr_parser.spanned())
                .map_into(),
        )))
        .map(|(condition, then, other)| Expression::If {
            condition,
            then,
            other,
        })
        .boxed()
}

pub fn unary_parser<'a, EP, I: TokenInput<'a>>(
    expr_parser: EP,
) -> impl TokenParser<'a, I, Expression> + Clone
where EP: TokenParser<'a, I, Expression> + Clone + 'a {
    ops_parser!(Minus, Bang, Tilde)
        .map_with_span(Spanned)
        .repeated()
        .at_least(1)
        .collect_boxed_slice()
        .or_not()
        .labelled("unary-ops")
        .boxed()
        .then(atom_parser(expr_parser).spanned())
        .map(
            |(ops, expression): (Option<Box<[Spanned<Operation>]>>, _)| match ops {
                Some(unary_ops) => Expression::Unary {
                    ops: unary_ops,
                    expression: Box::new(expression),
                },
                None => expression.0,
            },
        )
        .labelled("unary")
        .boxed()
}

pub fn atom_parser<'a, EP, I: TokenInput<'a>>(
    expr_parser: EP,
) -> impl TokenParser<'a, I, Expression> + Clone
where EP: TokenParser<'a, I, Expression> + Clone + 'a {
    choice((
        just(Token::Number)
            .map_with_state(|_, span: SimpleSpan, input: &mut ParserState<'a>| {
                parse_complex_number(input.rodeo, input.slice(span.into()).unwrap())
            })
            .map(Expression::Number)
            .boxed(),
        select! { Token::String(literal_type) => literal_type }
            .map_with_state(
                |literal_type, span: SimpleSpan, input: &mut ParserState<'a>| {
                    string_parser(literal_type)
                        .parse(input.slice(span.into()).unwrap())
                        .into_output()
                        .unwrap()
                        .into_lassoed(input.rodeo)
                },
            )
            .map(Expression::StringLiteral),
        keyword(Identifier::True).map(|_| Expression::Bool(true)),
        keyword(Identifier::False).map(|_| Expression::Bool(false)),
        call(expr_parser.clone()).map(Expression::Call),
        expr_parser
            .clone()
            .delimited_by(just(Token::ParenOpen), just(Token::ParenClosed)),
    ))
    .spanned()
    .boxed()
    .then(
        just(Token::Dot)
            .ignore_then(call(expr_parser).spanned())
            .repeated()
            .collect_boxed_slice()
            .boxed(),
    )
    .map(|(expression, calls)| {
        if calls.is_empty() {
            expression.0
        } else {
            Expression::CallChain {
                expression: Box::new(expression),
                calls,
            }
        }
    })
    .labelled("atom")
    .boxed()
}

pub fn expression_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Expression> + Clone {
    recursive(|p| {
        choice((
            if_expr_parser(p.clone()),
            assignment_parser(p.clone()),
            logic_parser(p),
        ))
    })
    .labelled("expression")
    .boxed()
}

use paste::paste;

use super::{string::string_parser, TokenParserExt};
macro_rules! binary_parser {
    ($what:ident, $name: ident, $next_name: ident, $ops:expr) => {
        paste! {
            pub fn  [< $name _parser >] <'a, EP, I: TokenInput<'a>>(expr_parser: EP) -> impl TokenParser<'a, I, Expression> + Clone
            where
                EP: TokenParser<'a, I, Expression> + Clone + 'a,
            {
                [< $what _parser>](
                    [< $next_name _parser >](expr_parser.clone()),
                    $ops.labelled(stringify!([< $name _operator >])),
                    OperationGroup:: [< $name:camel >]
                )
                .labelled(stringify!($name))
            }
        }
    };
}
// TODO: make some parser a chain for less recursion
#[rustfmt::skip] binary_parser!(binary, logic, equality, ops_parser!(BarBar, AmpersandAmpersand));
#[rustfmt::skip] binary_parser!(operation_chain, equality, comparison, ops_parser!(BangEqual, EqualEqual));
#[rustfmt::skip] binary_parser!(operation_chain, comparison, term, ops_parser!(GreaterEqual, LessEqual, Greater, Less));
#[rustfmt::skip] binary_parser!(binary, term, factor, ops_parser!(Plus, Minus)); // TODO: Newline split
#[rustfmt::skip] binary_parser!(binary, factor, bitops, ops_parser!(Asterisk, SlashSlash, Slash, Percent));
#[rustfmt::skip] binary_parser!(binary, bitops, unary, ops_parser!(Bar, Ampersand, GreaterGreater, LessLess, GreaterGreaterGreater));

pub fn operation_chain_parser<'a, NP, OP, I: TokenInput<'a>>(
    next_parser: NP,
    operator_parser: OP,
    group: OperationGroup,
) -> impl TokenParser<'a, I, Expression> + Clone
where
    NP: TokenParser<'a, I, Expression> + Clone + 'a,
    OP: TokenParser<'a, I, Operation> + Clone + 'a,
{
    next_parser
        .clone()
        .spanned()
        .then(
            operator_parser
                .spanned()
                .paddedln()
                .then(next_parser.spanned().paddedln())
                .repeated()
                .at_least(1)
                .collect_boxed_slice()
                .boxed()
                .or_not(),
        )
        .map(move |(atom, chain)| match chain {
            None => atom.0,
            Some(chain) => Expression::OperationChain {
                base: Box::new(atom),
                continuation: chain,
                group,
            },
        })
        .labelled("math")
        .boxed()
}

pub fn binary_parser<'a, NP, OP, I: TokenInput<'a>>(
    next_parser: NP,
    operator_parser: OP,
    group: OperationGroup,
) -> impl TokenParser<'a, I, Expression> + Clone
where
    NP: TokenParser<'a, I, Expression> + Clone + 'a,
    OP: TokenParser<'a, I, Operation> + Clone + 'a,
{
    next_parser
        .clone()
        .spanned()
        .foldl(
            operator_parser
                .spanned()
                .paddedln()
                // TODO fix new line consuming on last foldl run
                .then(next_parser.spanned().paddedln())
                .repeated(),
            move |lhs, (op, rhs)| {
                let span = SimpleSpan::from(lhs.1.start..rhs.1.end);
                Spanned(
                    Expression::Binary {
                        lhs: lhs.into(),
                        op,
                        rhs: rhs.into(),
                        group,
                    },
                    span,
                )
            },
        )
        .map(|Spanned(t, _)| t)
        .labelled("math")
        .boxed()
}
