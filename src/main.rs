#![allow(dead_code)]
#![allow(incomplete_features)]
#![allow(unused_macros)]
#![feature(trait_alias)]
#![feature(return_position_impl_trait_in_trait)]

use std::path::Path;

use crate::{
    parsing::parsers::{expressions::expression_parser, file::File},
    project::{
        readers::{read_workspace, ReadWorkspaceError, ReadWorkspaceResult},
        Workspace,
    },
    resolution::name_resolution::resolve_workspace_outlines,
};
use chumsky::{error::RichReason, prelude::Rich, primitive::end, ParseResult, Parser};
use itertools::Itertools;
use lasso::Rodeo;
use parsing::{
    ast::{declarations::FlatUseDeclaration, expressions::Expression},
    tokenizer::tokenize,
};
use source::VirtualFile;
use thiserror::__private::PathAsDisplay;

use crate::{
    parsing::{
        ast::declarations::UseDeclaration,
        parsers::{file::file_parser, ParserState},
        tokenizer::Token,
    },
    source::SourceFile,
};

pub mod parsing;
pub mod project;
pub mod resolution;
pub mod source;
pub mod typing;
pub mod utils;

#[derive(Debug)]
enum PathPartType {
    Module,
    File,
}

macro_rules! parse_and_print {
    ($input: literal, $parser_name:ident) => {{
        let input: String = $input.to_owned();
        let mut rodeo = Rodeo::new();
        let file = &VirtualFile::new(input.clone());
        let mut state = ParserState::new(&mut rodeo, file);
        let result = $parser_name()
            .then_ignore(end())
            .parse_with_state(tokenize(file.whole_file()), &mut state);
        println!("{:#?}", result);
        print_errors(&result, file);
        println!("{:?}", Token::lexer(&input).collect::<Vec<_>>());
    }};
}

fn main() {
    let mut rodeo = Rodeo::new();
    // let file = &VirtualFile::new(input);
    // let result = parse_file(&mut rodeo, file);

    // println!("{:#?}", result);

    // print_errors(&result, file);
    println!("Reading workspace...");
    if let Some(mut workspace) =
        read_workspace_and_report(&mut rodeo, Path::new("./tests/test-projects/001"))
    {
        let mut is_errors = false;
        println!("Resolving...");
        let dependency_id = workspace.project_dep_id();
        for (file_id, errs) in resolve_workspace_outlines(&mut rodeo, &mut workspace, dependency_id).iter() {
            is_errors = true;
            let file = workspace.project().files.file_by_id(*file_id).unwrap();
            let mut colors = ariadne::ColorGenerator::new();
            // Generate & choose some colours for each of our elements
            let input_name = file.path().as_display().to_string();
            for err in errs {
                ariadne::Report::build(ariadne::ReportKind::Error, &input_name, err.start)
                    .with_message("Not found somthing.")
                    .with_label(
                        ariadne::Label::new((&input_name, err.into_iter()))
                            .with_message("↑ where 🍌?")
                            .with_color(colors.next()),
                    )
                    .finish()
                    .print((&input_name, ariadne::Source::from(file.src())))
                    .unwrap();
            }
        }
        if is_errors {
            println!("Errors found, cannot go further.");
            // for (spur, s) in rodeo.into_iter() {
            //     println!("  {spur:>3?}: {s}")
            // }
            return;
        }
        println!("No use resolution errors found.")
    }

    // let mut depth = 0;
    // for (action, entry) in WalkDir::new("./src")
    //     .min_depth(2)
    //     .into_iter()
    //     .filter_map(|e| e.ok())
    // {
    //     let path = entry.into_path();
    //     let name = path.file_name().and_then(|s| s.to_str()).unwrap_or("?");
    //     match action {
    //         WalkAction::EnterDir => {
    //             depth += 1;
    //             println!("{:indent$}\\ {}", "", name, indent = depth);
    //         }
    //         WalkAction::ListFile => println!("{:indent$} | {}", "", name, indent = depth),
    //         WalkAction::ExitDir => {
    //             println!("{:indent$}/ {}", "", name, indent = depth);
    //             depth -= 1;
    //         }
    //     }
    // }

    // println!(
    //     "{:?}",
    //     Token::lexer(file.whole_file())
    //         .map(|tok| tok.unwrap_or(Token::Error))
    //         .collect::<Vec<Token>>()
    // );
    // println!("----------------------------------------");
    // fn test_parser<'a, I: TokenInput<'a>>() -> impl TokenParser<'a, I, Expression> {
    //     logic_parser(expression_parser())
    // }
    // parse_and_print!(r###"r"Hello " + ", age " + str(person.age)"""###, test_parser)
}

fn read_workspace_and_report(rodeo: &mut Rodeo, path: &Path) -> Option<Workspace> {
    match read_workspace(rodeo, path) {
        ReadWorkspaceResult::ErrFile(e) => {
            println!("{:#?}", e);
            None
        }
        ReadWorkspaceResult::ErrProject(e, files) => {
            if let Some(project_error) = e.downcast_ref::<ReadWorkspaceError>() {
                match project_error {
                    ReadWorkspaceError::NoWorkspaceFile => println!("No aplang.toml found!"),
                    ReadWorkspaceError::ParseErrors(errs) => {
                        for (file_id, error) in errs {
                            let mut colors = ariadne::ColorGenerator::new();
                            // Generate & choose some colours for each of our elements
                            let file = files.file_by_id(*file_id).unwrap();
                            let input_name = file.path().as_display().to_string();
                            ariadne::Report::build(
                                ariadne::ReportKind::Error,
                                &input_name,
                                error.span().into_range().start,
                            )
                            .with_message(match error.reason() {
                                RichReason::ExpectedFound { .. } => "Unexpected",
                                RichReason::Custom(_) => "Custom",
                                RichReason::Many(_) => "Many",
                            })
                            .with_label(
                                ariadne::Label::new((&input_name, error.span().into_range()))
                                    .with_message(format!("{error:?}"))
                                    .with_color(colors.next()),
                            )
                            .finish()
                            .print((&input_name, ariadne::Source::from(file.src())))
                            .unwrap();
                        }
                    }
                }
            } else {
                println!("{:#?}", e);
            };
            None
        }
        ReadWorkspaceResult::Ok(workspace) => Some(workspace),
    }
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

fn parse_expression<'a, S: SourceFile>(
    rodeo: &'a mut Rodeo,
    input: &'a S,
) -> ParseResult<Expression, Rich<'a, Token>> {
    let mut state = ParserState::new(rodeo, input);
    return expression_parser()
        .then_ignore(end())
        .parse_with_state(tokenize(input.whole_file()), &mut state);
}

fn print_uses(uses: &[UseDeclaration], file: &VirtualFile) {
    uses.iter().flat_map(UseDeclaration::flatten_tree).for_each(
        |FlatUseDeclaration {
             path,
             star,
             single_alias,
         }| {
            println!(
                "{}{}",
                path.iter()
                    .map(|s| file.read_range(s.into_range()).unwrap())
                    .join("::"),
                if star {
                    "::*"
                } else {
                    single_alias
                        .and_then(|s| file.read_range(s.1.into_range()))
                        .unwrap_or("")
                }
            )
        },
    );
}

fn print_errors<R>(result: &ParseResult<R, Rich<Token>>, file: &dyn SourceFile) {
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
