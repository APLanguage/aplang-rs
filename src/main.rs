#![allow(dead_code, unused_imports)]

use chumsky::{Parser, text::{TextParser, newline}, primitive::end};


use crate::parsing::{data::data_parser, file::file_parser};



pub mod parsing;
fn main() {
    let input = "
    data SomeDude{
        name: String, age:u32
    }

    fn test(dude: SomeDude) {
        1 + 2.smth()
        println(\"Dude is \" + dude.age + \" years old.\")
    }
    ";
    println!("{:?}", file_parser().padded().then_ignore(end()).parse(input));
}
