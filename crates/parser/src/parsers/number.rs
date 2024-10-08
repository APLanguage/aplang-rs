
use std::{
    num::{NonZeroU16, ParseFloatError, ParseIntError},
    str::FromStr,
    sync::OnceLock,
};

use bigdecimal::{num_bigint::ParseBigIntError, BigDecimal, ParseBigDecimalError};
use either::Either;
use num::BigInt;
use num_traits::{Num, PrimInt};
use regex::Regex;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct LiteralWidth(NonZeroU16);

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralWidthError {
    NotSupported(u64),
    TooBig(ParseIntError, String),
    TooBigForLiteral(u64),
}

#[derive(Debug, PartialEq, Clone)]
pub enum NumberLiteral {
    Unsigned(u64, LiteralWidth),
    Signed(i64, LiteralWidth),
    Float(f64, LiteralWidth),
    Inferred(Either<BigInt, BigDecimal>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum LiteralType {
    Unsigned,
    Signed,
    Float,
}

impl LiteralType {
    fn width_error(self, we: LiteralWidthError) -> NumberLiteralError {
        return NumberLiteralError::WidthError(self, we);
    }
}

pub type NumberLiteralResult = Result<NumberLiteral, NumberLiteralError>;

#[derive(Debug, PartialEq, Clone)]
pub enum NumberLiteralError {
    ParseInt(ParseIntError, Option<(LiteralType, LiteralWidth)>),
    ParseFloat(ParseFloatError, Option<(LiteralType, LiteralWidth)>),
    ParseBigInt(ParseBigIntError, Option<(LiteralType, LiteralWidth)>),
    ParseBigFloat(ParseBigDecimalError, Option<(LiteralType, LiteralWidth)>),
    WidthError(LiteralType, LiteralWidthError),
    FloatHasRadix,
    CannotInfer,
    UnknownSuffix(String),
    Error,
}

#[inline]
fn parse_number<T>(literal: &str, radix: u32) -> Result<T, <T as Num>::FromStrRadixErr>
where T: PrimInt {
    T::from_str_radix(literal, radix)
}

static NUMBER_REGEX: OnceLock<Regex> = OnceLock::new();

pub fn parse_complex_number(input: &str) -> NumberLiteralResult {
    let Some(captures) = NUMBER_REGEX
        .get_or_init(|| Regex::new(r"^(0x|0b|0o)?(\w+?(?:\.\w+?)*)?(?:([uif])(\d+))?$").unwrap())
        .captures(input)
    else {
        return Err(NumberLiteralError::Error);
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
    let num_type_width = captures
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
        });
    // ((is_negative, has_minus), (radix, value, num_type_width))
    // TODO: for the time being, the negative part could be handled with this parser later
    let is_negative = false;
    // let has_minus = false;

    let mut number_part = value.replace('_', "");
    if is_negative {
        number_part = "-".to_owned() + &number_part
    }
    let Some((num_type, width)) = num_type_width else {
        return if number_part.contains('.') {
            BigDecimal::from_str(&number_part)
                .map(Either::Right)
                .map_err(|err| NumberLiteralError::ParseBigFloat(err, None))
        } else {
            BigInt::from_str_radix(&number_part, radix)
                .map(Either::Left)
                .map_err(|err| NumberLiteralError::ParseBigInt(err, None))
        }
        .map(NumberLiteral::Inferred);
    };
    let width = match (num_type, TryInto::<u16>::try_into(width)) {
        (t, Err(_)) => Err(NumberLiteralError::WidthError(t, LiteralWidthError::TooBigForLiteral(width))),
        (LiteralType::Signed | LiteralType::Unsigned, Ok(w @ (8 | 16 | 32 | 64))) => Ok(LiteralWidth(NonZeroU16::try_from(w).expect("Must be 8, 16, 32 or 64"))),
        (LiteralType::Float, Ok(w @ (32 | 64))) => Ok(LiteralWidth(NonZeroU16::try_from(w).expect("Must be 32 or 64"))),
        (t, Ok(w)) => Err(t.width_error(LiteralWidthError::NotSupported(w as u64)))
    }?;
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
                    Err(e) => Err(NumberLiteralError::ParseFloat(e, Some((Float, width)))),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_postive() {
        assert_eq!(1, 1);
    }
}
