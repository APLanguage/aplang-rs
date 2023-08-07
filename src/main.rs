#![allow(dead_code)]
#![allow(incomplete_features)]
#![feature(trait_alias)]
#![feature(return_position_impl_trait_in_trait)]

use chumsky::{
    error::RichReason,
    input::{Input, Stream},
    prelude::Rich,
    primitive::end,
    ParseResult, Parser,
};
use crate::parsing::parsers::file::File;
use indextree::{Arena as IndexArena, NodeId};
use itertools::Itertools;
use lasso::{Rodeo, Spur};
use logos::Logos;
use parsing::parsers::TokenInput;
use source::VirtualFile;

use crate::{
    parsing::{
        ast::declarations::UseDeclaration,
        parsers::{file::file_parser, ParserState, TokenParserExt},
        tokenizer::Token,
    },
    source::SourceFile,
};

pub mod parsing;
pub mod source;
pub mod typing;

#[derive(Debug)]
enum PathPartType {
    Module,
    File,
}

fn main() {
    let input: String = r#"
fn sum(a: i32, b: i32) -> int = a + b

fn main() {
  var result = sum(1, 2)
  print(result)
}
    "#
    .to_owned();
    let mut rodeo = Rodeo::new();
    let file = &VirtualFile::new(input);
    let result = parse_file(&mut rodeo, file);

    println!("{:#?}", result);

    print_errors(&result, file);
    let mut module_tree: IndexArena<Spur> = IndexArena::new();
    // module-tree -> package-tree -> files -> declaration tree
    let mut package_tree: IndexArena<(Spur, Option<NodeId>)> = IndexArena::new();
    if let Some(file_ast) = result.into_output() {
        // print_uses(uses, &file);
    }

    println!(
        "{:?}",
        Token::lexer(file.whole_file())
            .map(|tok| tok.unwrap_or(Token::Error))
            .collect::<Vec<Token>>()
    );
}

fn parse_file<'a, S: SourceFile>(
    rodeo: &'a mut Rodeo,
    input: &'a S,
) -> ParseResult<File, Rich<'a, Token>> {
    let mut state = ParserState::new(rodeo, input);
    return file_parser()
        .then_ignore(end())
        .parse_with_state(tokenize(input.whole_file()), &mut state);
}

fn print_uses(uses: Box<[UseDeclaration]>, file: &VirtualFile) {
    uses.iter()
        .flat_map(UseDeclaration::flatten_tree)
        .for_each(|(path, is_star)| {
            println!(
                "{}{}",
                path.iter()
                    .map(|s| file.read_range(s.into_range()).unwrap())
                    .join("::"),
                if is_star { "::*" } else { "" }
            )
        });
}

fn tokenize(input: &str) -> impl TokenInput<'_> {
    Stream::from_iter(
        Token::lexer(input)
            .spanned()
            .map(|(tok, span)| (tok.unwrap_or(Token::Error), span.into())),
    )
    .spanned((input.len()..input.len()).into())
}

fn print_errors<R>(result: &ParseResult<R, Rich<Token>>, file: &VirtualFile) {
    use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};

    let mut colors = ColorGenerator::new();
    // Generate & choose some colours for each of our elements
    let input_name = "virtual_input.aplang";
    for error in result.errors() {
        Report::build(
            ReportKind::Error,
            "virtual_input.aplang",
            error.span().into_range().start,
        )
        .with_message(match error.reason() {
            RichReason::ExpectedFound { .. } => "Unexpected",
            RichReason::Custom(_) => "Custom",
            RichReason::Many(_) => "Mnay",
        })
        .with_label(
            Label::new((input_name, error.span().into_range()))
                .with_message(format!("{error:?}"))
                .with_color(colors.next()),
        )
        .finish()
        .print(("virtual_input.aplang", Source::from(file.whole_file())))
        .unwrap();
    }
}
