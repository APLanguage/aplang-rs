use std::{
    num::{ParseFloatError, ParseIntError},
    sync::OnceLock,
};

use either::Either;
use lasso::Rodeo;
use num::BigInt;
use num_traits::{Num, PrimInt};
use regex::Regex;

use crate::typing::{FloatWidth, IntegerWidth};

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
    Inferred(Either<BigInt, ()>),
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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(u8)]
pub enum LiteralWidth {
    Inferred = 0,
    _8 = 8,
    _16 = 16,
    _32 = 32,
    _64 = 64,
}

impl TryFrom<u8> for LiteralWidth {
    type Error = LiteralWidthError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        LiteralWidth::try_from(Into::<u64>::into(value))
    }
}

impl TryFrom<u64> for LiteralWidth {
    type Error = LiteralWidthError;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        Ok(match value {
            0 => LiteralWidth::Inferred,
            8 => LiteralWidth::_8,
            16 => LiteralWidth::_16,
            32 => LiteralWidth::_32,
            64 => LiteralWidth::_64,
            other => return Err(LiteralWidthError::NotSupported(other)),
        })
    }
}

impl From<IntegerWidth> for LiteralWidth {
    fn from(value: IntegerWidth) -> Self {
        match value {
            IntegerWidth::_8 => LiteralWidth::_8,
            IntegerWidth::_16 => LiteralWidth::_16,
            IntegerWidth::_32 => LiteralWidth::_32,
            IntegerWidth::_64 => LiteralWidth::_64,
        }
    }
}

impl From<FloatWidth> for LiteralWidth {
    fn from(value: FloatWidth) -> Self {
        match value {
            FloatWidth::_32 => LiteralWidth::_32,
            FloatWidth::_64 => LiteralWidth::_64,
        }
    }
}

pub type LiteralWidthResult = Result<LiteralWidth, LiteralWidthError>;
pub type NumberLiteralResult = Result<NumberLiteral, NumberLiteralError>;

#[derive(Debug, PartialEq, Clone)]
pub enum NumberLiteralError {
    ParseInt(ParseIntError, Option<(LiteralType, LiteralWidth)>),
    ParseFloat(ParseFloatError, Option<(LiteralType, LiteralWidth)>),
    UnsignedIntWidthError(LiteralWidthError),
    SignedIntWidthError(LiteralWidthError),
    FloatWidthError(LiteralWidthError),
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

pub fn parse_complex_number(_rodeo: &mut Rodeo, input: &str) -> NumberLiteralResult {
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
    if num_type_width.is_none() {
        return Ok(NumberLiteral::Inferred(
            BigInt::parse_bytes(number_part.as_bytes(), radix)
                .map(Either::Left)
                .unwrap_or(Either::Right(())),
        ));
    };
    let (num_type, width) = num_type_width.unwrap();
    let width = LiteralWidth::try_from(width).map_err(num_type.width_error())?;
    let width = match (num_type.clone(), width) {
        (_, LiteralWidth::Inferred) => width,
        (LiteralType::Signed | LiteralType::Unsigned, LiteralWidth::_8) => width,
        (LiteralType::Signed | LiteralType::Unsigned, LiteralWidth::_16) => width,
        (_, LiteralWidth::_32) => width,
        (_, LiteralWidth::_64) => width,
        (_, LiteralWidth::_8) => {
            return Err((num_type.width_error())(LiteralWidthError::NotSupported(8)))
        }
        (_, LiteralWidth::_16) => {
            return Err((num_type.width_error())(LiteralWidthError::NotSupported(
                16,
            )))
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
                    Err(e) => Err(NumberLiteralError::ParseFloat(e, Some((Float, width)))),
                }
            }
        }
    }
}
