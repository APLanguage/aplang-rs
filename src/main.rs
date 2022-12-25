#![allow(dead_code, unused_imports)]

use chumsky::{
    primitive::end,
    text::{newline, TextParser},
    Parser,
};

use crate::parsing::{data::data_parser, file::file_parser, expression::expression_parser};

pub mod parsing;
fn main() {
    let input = "2 * 5 + 1 *3";
    println!(
        "{:?}",
        expression_parser().padded().then_ignore(end()).parse(input)
    );
}
