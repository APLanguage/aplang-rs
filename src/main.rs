#![allow(dead_code)]
#![feature(min_specialization)]
#![feature(iter_intersperse)]
#![feature(associated_type_defaults)]
#![feature(once_cell)]
#![feature(trait_alias)]
#![feature(return_position_impl_trait_in_trait)]

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
        print(person.age + (if(true) -0 else 0))
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
