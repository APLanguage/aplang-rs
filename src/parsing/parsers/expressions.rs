use chumsky::{
    primitive::{choice, just},
    recursive::recursive,
    span::SimpleSpan,
    IterParser, Parser,
};

use crate::parsing::{
    tokenizer::{ident, keyword, Operation, Token},
    ast::expressions::{Call, Expression},
    literals::number::parse_complex_number,
    tokenizer::Identifier,
    utilities::Spanned,
    ParserState, TokenInput, TokenParser,
};

macro_rules! ops_parser {
    ($($variant:ident), *) => {
        choice((
            $(
                just(Token::$variant).map(|tk| Into::<Operation>::into(tk)),
            )*
        ))
    };
}

fn call<'a, EP, I: TokenInput<'a>>(expr_parser: EP) -> impl TokenParser<'a, I, Call> + Clone
where EP: TokenParser<'a, I, Expression> + Clone + 'a {
    ident()
        .map_with_span(Spanned)
        .then(
            expr_parser
                .map_with_span(Spanned)
                .paddedln()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .at_least(0)
                .collect()
                .map(Vec::into_boxed_slice)
                .delimited_by(just(Token::ParenOpen), just(Token::ParenClosed))
                .or_not()
                .boxed(),
        )
        .map(|(identifier, params)| match params {
            Some(parameters) => Call::Call {
                identifier,
                parameters,
            },
            None => Call::Identifier(identifier),
        })
        .labelled("call")
}

fn assignment<'a, EP, I: TokenInput<'a>>(
    expr_parser: EP,
) -> impl TokenParser<'a, I, Expression> + Clone
where EP: TokenParser<'a, I, Expression> + Clone + 'a {
    call(expr_parser.clone())
        .map_with_span(Spanned)
        .labelled("assignement_left")
        .then(
            ops_parser!(
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
            .paddedln()
            .then(
                expr_parser
                    .map_with_span(Spanned)
                    .labelled("assignement_right"),
            ),
        )
        .map(|(call, (op, expression))| Expression::Assignement {
            call,
            op,
            expression: Box::new(expression),
        })
        .boxed()
        .labelled("assignement")
}

fn if_expr_parser<'a, EP, I: TokenInput<'a>>(
    expr_parser: EP,
) -> impl TokenParser<'a, I, Expression> + Clone
where EP: TokenParser<'a, I, Expression> + Clone + 'a {
    keyword(Identifier::If)
        .ignore_then(
            expr_parser
                .clone()
                .map_with_span(Spanned)
                .paddedln()
                .delimited_by(just(Token::ParenOpen), just(Token::ParenClosed)),
        )
        .then(expr_parser.clone().map_with_span(Spanned).paddedln())
        .then(
            keyword(Identifier::Else)
                .paddedln()
                .ignore_then(expr_parser.map_with_span(Spanned)),
        )
        .map(|((condition, then), other)| Expression::If {
            condition: Box::new(condition),
            then: Box::new(then),
            other: Box::new(other),
        })
        .boxed()
}

pub fn unary_parser<'a, EP, I: TokenInput<'a>>(
    expr_parser: EP,
) -> impl TokenParser<'a, I, Expression> + Clone
where EP: TokenParser<'a, I, Expression> + Clone + 'a {
    ops_parser!(Minus, Bang)
        .map_with_span(Spanned)
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .map(|v| v.into_boxed_slice())
        .or_not()
        .labelled("unary-ops")
        .then(atom_parser(expr_parser))
        .map(
            |(ops, expression): (Option<Box<[Spanned<Operation>]>>, _)| match ops {
                Some(unary_ops) => Expression::Unary {
                    ops: unary_ops,
                    expression: Box::new(expression),
                },
                None => expression,
            },
        )
        .labelled("unary")
}

pub fn atom_parser<'a, EP, I: TokenInput<'a>>(
    expr_parser: EP,
) -> impl TokenParser<'a, I, Expression> + Clone
where EP: TokenParser<'a, I, Expression> + Clone + 'a {
    choice((
        just(Token::Number)
            .map_with_state(|_, span: SimpleSpan, input: &mut ParserState| {
                parse_complex_number(input.slice(span.into()))
            })
            .map(Expression::Number),
        call(expr_parser.clone()).map(Expression::Call),
        expr_parser
            .clone()
            .delimited_by(just(Token::ParenOpen), just(Token::ParenClosed)),
    ))
    .map_with_span(Spanned)
    .then(
        just(Token::Dot)
            .ignore_then(call(expr_parser).map_with_span(Spanned))
            .repeated()
            .collect::<Vec<_>>()
            .map(Vec::into_boxed_slice)
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
    .boxed()
    .labelled("atom")
}

pub fn expression_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Expression> + Clone {
    recursive(|p| {
        choice((
            if_expr_parser(p.clone()),
            assignment(p.clone()),
            logic_parser(p),
        ))
    })
    .boxed()
    .labelled("expression")
}

use paste::paste;
macro_rules! binary_parser {
    ($name: ident, $next_name: ident, $ops:expr) => {
        paste! {
            fn  [< $name _parser >] <'a, EP, I: TokenInput<'a>>(expr_parser: EP) -> impl TokenParser<'a, I, Expression> + Clone
            where
                EP: TokenParser<'a, I, Expression> + Clone + 'a,
            {
                math_parser(
                    [< $next_name _parser >](expr_parser.clone()),
                    $ops.labelled(stringify!([< $name _operator >]))
                )
                .labelled(stringify!($name))
            }
        }
    };
}

binary_parser!(logic, equality, ops_parser!(BarBar, AmpersandAmpersand));
binary_parser!(equality, comparison, ops_parser!(BangEqual, EqualEqual));
binary_parser!(
    comparison,
    term,
    ops_parser!(GreaterEqual, LessEqual, Greater, Less)
);
binary_parser!(term, factor, ops_parser!(Plus, Minus)); // TODO: Newline split
binary_parser!(
    factor,
    bitops,
    ops_parser!(Asterisk, SlashSlash, Percent, Slash)
);
binary_parser!(
    bitops,
    unary,
    ops_parser!(
        Bar,
        Ampersand,
        GreaterGreater,
        LessLess,
        GreaterGreaterGreater
    )
);

pub fn math_parser<'a, NP, OP, I: TokenInput<'a>>(
    next_parser: NP,
    operator_parser: OP,
) -> impl TokenParser<'a, I, Expression> + Clone
where
    NP: TokenParser<'a, I, Expression> + Clone + 'a,
    OP: TokenParser<'a, I, Operation> + Clone + 'a,
{
    next_parser
        .clone()
        .map_with_span(Spanned)
        .then(
            operator_parser
                .map_with_span(Spanned)
                .paddedln()
                .then(next_parser.map_with_span(Spanned).paddedln())
                .repeated()
                .collect::<Vec<_>>()
                .map(Vec::into_boxed_slice),
        )
        .map(|(atom, chain)| {
            if chain.is_empty() {
                atom.0 // should think of a better way to handle this
            } else {
                Expression::Operation {
                    base: Box::new(atom),
                    continuation: chain,
                }
            }
        })
        .boxed()
        .labelled("math")
}
