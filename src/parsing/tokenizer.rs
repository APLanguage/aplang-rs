use chumsky::{
    prelude::*,
    Parser,
};
use super::{TokenParser, TokenInput};
use logos::Logos;

#[derive(Logos, Debug, PartialEq, Eq, Clone, Hash)]
pub enum Identifier {
    #[token("fn")]
    Fn,
    #[token("for")]
    For,
    #[token("while")]
    While,
    #[token("return")]
    Return,
    #[token("break")]
    Break,
    #[token("struct")]
    Struct,
    #[token("var")]
    Var,
    #[token("val")]
    Val,
    #[token("if")]
    If,
    #[token("else")]
    Else,

    #[regex(".+")]
    Custom,

    #[error]
    Error,
}

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    #[token("->")]
    ArrowRight,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,

    #[token("/")]
    Slash,
    #[token("//")]
    SlashSlash,
    #[token("*")]
    Asterisk,
    #[token("**")]
    AsteriskAsterisk,
    #[token("%")]
    Percent,

    #[token("&")]
    Ampersand,
    #[token("|")]
    Bar,
    #[token("^")]
    Circumflex,

    #[token(">>")]
    GreaterGreater,
    #[token("<<")]
    LessLess,
    #[token(">>>")]
    GreaterGreaterGreater,

    #[token("&&")]
    AmpersandAmpersand,
    #[token("||")]
    BarBar,

    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEqual,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEqual,

    #[token("==")]
    EqualEqual,
    #[token("!=")]
    BangEqual,

    #[token("!")]
    Bang,

    #[token("=")]
    Equal,

    #[token("+=")]
    PlusEqual,
    #[token("-=")]
    MinusEqual,

    #[token("/=")]
    SlashEqual,
    #[token("//=")]
    SlashSlashEqual,
    #[token("*=")]
    AsteriskEqual,
    #[token("**=")]
    AsteriskAsteriskEqual,
    #[token("%=")]
    PercentEqual,

    #[token("&=")]
    AmpersandEqual,
    #[token("|=")]
    BarEqual,
    #[token("^=")]
    CircumflexEqual,

    #[token(">>=")]
    GreaterGreaterEqual,
    #[token("<<=")]
    LessLessEqual,
    #[token(">>>=")]
    GreaterGreaterGreaterEqual,

    #[token("(")]
    ParenOpen,
    #[token(")")]
    ParenClosed,

    #[token("{")]
    BraceOpen,
    #[token("}")]
    BraceClosed,

    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClosed,

    #[token(".")]
    Dot,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,

    #[regex(r"[_a-zA-Z]\w*", |lex| Identifier::lexer(lex.slice()).next().unwrap())]
    Identifier(Identifier),

    #[regex(r"\d[_\w]*(\.\d[_\w]*)?")]
    Number,

    #[regex("\"[^\"]+\"")]
    String,

    #[token("\n")]
    NewLine,

    #[regex(r"[ \t\f\r]+", logos::skip)]
    #[error]
    Error,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Operation {
    Addition,
    Substraction,

    Division,
    DivisionInteger,
    Multiplication,
    Power,

    And,
    Or,

    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Equal,
    NotEqual,

    Remainder,

    AndBitwise,
    OrBitwise,
    XOR,

    ShiftRight,
    ShiftLeft,
    ShiftRightUnsigned,

    Not,
    Unknown
}

impl From<Token> for Operation {
    fn from(value: Token) -> Self {
        use Token::*;
        use Operation::*;
        match value {
            Plus => Addition,
            Minus => Substraction,
            Slash => Division,
            SlashSlash => DivisionInteger,
            Asterisk => Multiplication,
            AsteriskAsterisk => Power,
            Percent => Remainder,
            Ampersand => AndBitwise,
            Bar => OrBitwise,
            Circumflex => XOR,
            GreaterGreater => ShiftRight,
            LessLess => ShiftLeft,
            GreaterGreaterGreater => ShiftRightUnsigned,
            AmpersandAmpersand => And,
            BarBar => Or,
            Token::Greater => Operation::Greater,
            Token::GreaterEqual => Operation::GreaterEqual,
            Token::Less => Operation::Less,
            Token::LessEqual => Operation::LessEqual,
            EqualEqual => Operation::Equal,
            BangEqual => NotEqual,
            PlusEqual => Addition,
            MinusEqual => Substraction,
            SlashEqual => Division,
            SlashSlashEqual => DivisionInteger,
            AsteriskEqual => Multiplication,
            AsteriskAsteriskEqual => Power,
            PercentEqual => Remainder,
            AmpersandEqual => AndBitwise,
            BarEqual => OrBitwise,
            CircumflexEqual => XOR,
            GreaterGreaterEqual => ShiftRight,
            LessLessEqual => ShiftLeft,
            GreaterGreaterGreaterEqual => ShiftRightUnsigned,
            Bang => Not,
            _ => Unknown
        }
    }
}

pub fn ident<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Identifier> + Clone {
    select! { Token::Identifier(id) => id }
}

#[inline]
pub fn keyword<'a, I: TokenInput<'a>>(id: Identifier) -> impl TokenParser<'a, I, ()> + Clone {
    just(Token::Identifier(id)).ignored()
}

#[inline]
pub fn newline<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, ()> + Clone {
    any().filter(|t| matches!(t, Token::NewLine)).ignored()
}