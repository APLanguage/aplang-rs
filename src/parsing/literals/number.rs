use std::num::{ParseFloatError, ParseIntError};

use chumsky::{
    prelude::Simple,
    primitive::just,
    text::{ident, TextParser, digits},
    Parser,
};
use lazy_static::lazy_static;
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{cast::FromPrimitive, Num, PrimInt};
use regex::Regex;

#[derive(Debug)]
pub enum LiteralWidthError {
    NotSupported(u64),
    TooBig(ParseIntError, String),
    TooBigForLiteral(u64),
}

#[derive(Debug)]
pub enum NumberLiteral {
    Unsigned(u64, LiteralWidth),
    Signed(i64, LiteralWidth),
    Float(f64, LiteralWidth),
    Inferred {
        unsigned: Option<u64>,
        signed: Option<i64>,
        float: Option<f64>,
    },
}
impl NumberLiteral {
    fn inferred_of_u64(input: u64) -> NumberLiteral {
        NumberLiteral::Unsigned(input, LiteralWidth::Inferred)
    }
    fn inferred_of_i64(input: i64) -> NumberLiteral {
        NumberLiteral::Signed(input, LiteralWidth::Inferred)
    }
    fn inferred_of_f64(input: f64) -> NumberLiteral {
        NumberLiteral::Float(input, LiteralWidth::Inferred)
    }
}

#[derive(Debug)]
pub enum LiteralType {
    Unsigned,
    Signed,
    Float,
}

impl LiteralType {
    fn width_error(&self) -> fn(LiteralWidthError) -> NumberLiteralError {
        match self {
            LiteralType::Signed => NumberLiteralError::SignedIntWidthError,
            LiteralType::Unsigned => NumberLiteralError::UnsignedIntWidthError,
            LiteralType::Float => NumberLiteralError::FloatWidthError,
        }
    }
}

#[derive(Clone, Debug, FromPrimitive, ToPrimitive)]
pub enum LiteralWidth {
    Inferred = 0,
    _8 = 8,
    _16 = 16,
    _32 = 32,
    _64 = 64,
}
pub type LiteralWidthResult = Result<LiteralWidth, LiteralWidthError>;
pub type NumberLiteralResult = Result<NumberLiteral, NumberLiteralError>;

#[derive(Debug)]
pub enum NumberLiteralError {
    ParseInt(ParseIntError, Option<(LiteralType, LiteralWidth)>),
    ParseFloat(ParseFloatError, Option<(LiteralType, LiteralWidth)>),
    UnsignedIntWidthError(LiteralWidthError),
    SignedIntWidthError(LiteralWidthError),
    FloatWidthError(LiteralWidthError),
    FloatHasRadix,
    CannotInfer,
    UnknownSuffix(String),
}

#[inline]
fn parse_number<T>(literal: &str, radix: u32) -> Result<T, <T as Num>::FromStrRadixErr>
where
    T: PrimInt,
{
    T::from_str_radix(literal, radix)
}

pub fn number_parser() -> impl Parser<char, String, Error = Simple<char>> + Clone {
    // https://github.com/zesterer/chumsky/issues/184
    digits(10)
        .then(ident().or_not())
        .map(|(tail, head): (String, Option<String>)| match head {
            Some(head) => tail + &head,
            None => tail,
        })
}

pub fn complex_number_parser() -> impl Parser<char, NumberLiteralResult, Error = Simple<char>>  + Clone{
    lazy_static! {
        static ref NUMBER_REGEX: Regex =
            Regex::new(r"^(0x|0b|0o)?(\w+?(?:\.\w+?)*)?(?:([uif])(\d+))?$").unwrap();
    }

    let nb_psr = number_parser()
        .separated_by(just("."))
        .at_least(1)
        .map(|v| v.join("."));
    just("-")
        .repeated()
        .map(|v| (v.len() & 1 == 1, !v.is_empty()))
        .then(nb_psr.map(|value| {
            let Some(captures) = NUMBER_REGEX.captures(&value) else {
                return (10, "wtf".to_owned(), None);
            };
            let value = captures.get(2).map(|m| m.as_str().to_owned());
            let radix = captures
                .get(1)
                .map(|m| match m.as_str() {
                    "0x" => 16,
                    "0b" => 2,
                    "0o" => 8,
                    _ => unimplemented!(),
                })
                .unwrap_or(10);
            let value = value.unwrap_or_else(|| captures.get(1).unwrap().as_str().to_owned());
            (
                radix,
                value,
                captures
                    .get(3)
                    .map(|m| m.as_str())
                    .map(|nt| match nt {
                        "u" => LiteralType::Unsigned,
                        "i" => LiteralType::Signed,
                        "f" => LiteralType::Float,
                        _ => unreachable!(),
                    })
                    .map(|lt| {
                        (
                            lt,
                            captures
                                .get(4)
                                .map(|m| m.as_str())
                                .map(|w| w.parse::<u64>().unwrap())
                                .unwrap(),
                        )
                    }),
            )
        }))
        .map(
            |((is_negative, has_minus), (radix, value, num_type_width))| {
                let mut number_part = value.replace('_', "");
                if is_negative {
                    number_part = "-".to_owned() + &number_part
                }
                if num_type_width.is_none() {
                    let unsigned = if !has_minus {
                        parse_number::<u64>(&number_part, radix).ok()
                    } else {
                        None
                    };
                    let signed = parse_number::<i64>(&number_part, radix).ok();
                    let float = if radix == 10 {
                        number_part.parse::<f64>().ok()
                    } else {
                        None
                    };
                    return match (unsigned.is_some() as u8)
                        + (signed.is_some() as u8)
                        + (float.is_some() as u8)
                    {
                        0 => Err(NumberLiteralError::CannotInfer),
                        1 => Ok(unsigned
                            .map(NumberLiteral::inferred_of_u64)
                            .or_else(|| signed.map(NumberLiteral::inferred_of_i64))
                            .or_else(|| float.map(NumberLiteral::inferred_of_f64))
                            .unwrap()),
                        _ => Ok(NumberLiteral::Inferred {
                            unsigned,
                            signed,
                            float,
                        }),
                    };
                };
                let (num_type, width) = num_type_width.unwrap();
                let width = match LiteralWidth::from_u64(width).ok_or(width) {
                    Ok(w) => w,
                    Err(w) => {
                        return Err(num_type.width_error()(LiteralWidthError::NotSupported(w)))
                    }
                };
                use LiteralType::*;
                match num_type {
                    Unsigned => match parse_number(&number_part, radix) {
                        Ok(v) => Ok(NumberLiteral::Unsigned(v, width)),
                        Err(e) => Err(NumberLiteralError::ParseInt(e, Some((Unsigned, width)))),
                    },
                    Signed => match parse_number(&number_part, radix) {
                        Ok(v) => Ok(NumberLiteral::Signed(v, width)),
                        Err(e) => Err(NumberLiteralError::ParseInt(e, Some((Signed, width)))),
                    },
                    Float => {
                        if radix != 10 {
                            Err(NumberLiteralError::FloatHasRadix)
                        } else {
                            match number_part.parse::<f64>() {
                                Ok(v) => Ok(NumberLiteral::Float(v, width)),
                                Err(e) => {
                                    Err(NumberLiteralError::ParseFloat(e, Some((Float, width))))
                                }
                            }
                        }
                    }
                }
            },
        )
        .padded()
}

#[cfg(test)]
mod tests {
    use super::complex_number_parser;
    use chumsky::Parser;

    #[test]
    fn testoo() {
        let parser = complex_number_parser();
        let v: Vec<&str> = vec![
            "_123i32",
            "a__auab",
            "0b",
            "0x",
            "0X",
            "0xi16",
            "",
            "132i64",
            "0x123u64",
            "0babcdefi64",
            "0b01010u64",
            "0xabc_defu32",
            "4141i8",
            "-1i32",
            "0i16",
            "0",
            "-1",
            "00",
            "1",
            "-0012",
            "--12",
            "01",
            "12",
            "0x0",
            "1.0",
            "13.0f64",
            "564f64",
            "1.6_54_6",
            "-0x2130",
            "9223372036854775808i64",
            "-9223372036854775808i64",
            "123.0213",
            "13.0123.32"
        ];
        for ele in v {
            println!("{} -> {:?}", ele, parser.parse(ele))
        }
    }
}
