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
    tokenizer::{ident, keyword, Operation, Token},
    utilities::Spanned,
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

pub fn call<'a, EP, I: TokenInput<'a>>(expr_parser: EP) -> impl TokenParser<'a, I, CallKind> + Clone
where EP: TokenParser<'a, I, Expression> + Clone + 'a {
    ident()
        .spur()
        .map_with_span(Spanned)
        .then(
            expr_parser
                .infoed()
                .paddedln()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .at_least(0)
                .collect_boxed_slice()
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

pub fn assignment<'a, EP, I: TokenInput<'a>>(
    expr_parser: EP,
) -> impl TokenParser<'a, I, Expression> + Clone
where EP: TokenParser<'a, I, Expression> + Clone + 'a {
    group((
        call(expr_parser.clone())
            .infoed()
            .labelled("assignement_left"),
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
        expr_parser.infoed().labelled("assignement_right"),
    ))
    .map(|(call, op, expression)| Expression::Assignement {
        call,
        op,
        expression: Box::new(expression),
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
                .infoed()
                .paddedln()
                .delimited_by(just(Token::ParenOpen), just(Token::ParenClosed))
                .boxed(),
            expr_parser.clone().infoed().paddedln(),
            keyword(Identifier::Else)
                .paddedln()
                .ignore_then(expr_parser.infoed()),
        )))
        .map(|(condition, then, other)| Expression::If {
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
        .collect_boxed_slice()
        .or_not()
        .labelled("unary-ops")
        .boxed()
        .then(atom_parser(expr_parser).infoed())
        .map(
            |(ops, expression): (Option<Box<[Spanned<Operation>]>>, _)| match ops {
                Some(unary_ops) => Expression::Unary {
                    ops: unary_ops,
                    expression: Box::new(expression),
                },
                None => expression.inner,
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
                parse_complex_number(input.slice(span.into()).unwrap())
            })
            .map(Expression::Number)
            .boxed(),
        call(expr_parser.clone()).map(Expression::Call),
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
        expr_parser
            .clone()
            .delimited_by(just(Token::ParenOpen), just(Token::ParenClosed)),
    ))
    .infoed()
    .boxed()
    .then(
        just(Token::Dot)
            .ignore_then(call(expr_parser).infoed())
            .repeated()
            .collect_boxed_slice()
            .boxed(),
    )
    .map(|(expression, calls)| {
        if calls.is_empty() {
            expression.inner
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
            assignment(p.clone()),
            logic_parser(p),
        ))
    })
    .labelled("expression")
    .boxed()
}

use paste::paste;

use super::{string::string_parser, TokenParserExt};
macro_rules! binary_parser {
    ($name: ident, $next_name: ident, $ops:expr) => {
        paste! {
            pub fn  [< $name _parser >] <'a, EP, I: TokenInput<'a>>(expr_parser: EP) -> impl TokenParser<'a, I, Expression> + Clone
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
        .infoed()
        .then(
            operator_parser
                .map_with_span(Spanned)
                .paddedln()
                .then(next_parser.infoed().paddedln())
                .repeated()
                .at_least(1)
                .collect_boxed_slice()
                .boxed()
                .or_not(),
        )
        .map(|(atom, chain)| match chain {
            None => atom.inner,
            Some(chain) => Expression::Operation {
                base: Box::new(atom),
                continuation: chain,
            },
        })
        .labelled("math")
        .boxed()
}
