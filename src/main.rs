#![allow(dead_code)]
#![feature(min_specialization)]
#![feature(iter_intersperse)]
#![feature(associated_type_defaults)]
#![feature(once_cell)]
#![feature(trait_alias)]

use chumsky::{
    input::{Input, Stream},
    Parser,
};
use logos::Logos;

use crate::parsing::{file::file_parser, tokenizer::Token, ParserState};

pub mod parsing;

fn main() {
    let input = r#"
      struct Person {
        age: u8,
        name: String
      }
      
      fn test(person: Person) {
        print(person.age + 0)
        print(person.name)
      }
    "#;
    let s = Stream::from_iter(
        Token::lexer(input)
            .spanned()
            .map(|(tok, span)| (tok, span.into())),
    )
    .spanned((input.len()..input.len()).into());
    let mut state = ParserState::new(input);
    println!("{:#?}", file_parser().parse_with_state(s, &mut state));
    println!("{:?}", Token::lexer(input).collect::<Vec<Token>>());
}
