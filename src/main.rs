#![allow(dead_code)]
#![allow(incomplete_features)]
#![feature(trait_alias)]
#![feature(return_position_impl_trait_in_trait)]

use std::{cell::RefCell, path::Path, rc::Rc};

use crate::{
    parsing::parsers::{expressions::expression_parser, file::File, TokenParser, TokenInput},
    project::{read_workspace, ReadWorkspaceError},
    source::RefVirtualFile,
};
use chumsky::{
    error::RichReason,
    prelude::Rich,
    primitive::end,
    ParseResult, Parser,
};
use indextree::{Arena as IndexArena, NodeId};
use itertools::Itertools;
use lasso::{Rodeo, Spur};
use parsing::{
    ast::expressions::Expression,
    tokenizer::tokenize,
};
use project::ReadWorkspaceResult;
use slotmap::{new_key_type, SlotMap};
use source::VirtualFile;
use thiserror::__private::PathAsDisplay;
use logos::Logos;

use crate::{
    parsing::{
        ast::declarations::UseDeclaration,
        parsers::{file::file_parser, ParserState, expressions::*},
        tokenizer::Token,
    },
    source::SourceFile,
};

pub mod parsing;
pub mod project;
pub mod source;
pub mod typing;

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

struct FileTree {}

new_key_type! { struct ModuleSlotMapId; }

struct ModuleInfo {
    name: Spur,
    files: FileTree,
}

struct ModuleTree {
    modules: SlotMap<ModuleSlotMapId, ModuleInfo>,
    tree: IndexArena<ModuleSlotMapId>,
}

enum ModuleId {
    SlotMapId(ModuleSlotMapId),
    IndexArenaId(NodeId),
}

enum ThingId {
    Module(ModuleId),
}

impl ModuleTree {
    fn get_module(&self, module_id: ModuleId) -> Option<&ModuleInfo> {
        match module_id {
            ModuleId::SlotMapId(id) => self.modules.get(id),
            ModuleId::IndexArenaId(id) => {
                self.tree.get(id).and_then(|id| self.modules.get(*id.get()))
            }
        }
    }

    fn get_module_mut(&mut self, module_id: ModuleId) -> Option<&mut ModuleInfo> {
        match module_id {
            ModuleId::SlotMapId(id) => self.modules.get_mut(id),
            ModuleId::IndexArenaId(id) => self
                .tree
                .get_mut(id)
                .and_then(|id| self.modules.get_mut(*id.get())),
        }
    }
}

fn main() {
    let mut rodeo = Rodeo::new();
    // let file = &VirtualFile::new(input);
    // let result = parse_file(&mut rodeo, file);

    // println!("{:#?}", result);

    // print_errors(&result, file);

    match read_workspace(&mut rodeo, Path::new("./tests/test-projects/001")) {
        ReadWorkspaceResult::ErrFile(e) => println!("{:#?}", e),
        ReadWorkspaceResult::ErrProject(e, files) => {
            if let Ok(project_error) = e.downcast::<ReadWorkspaceError>() {
                match project_error {
                    ReadWorkspaceError::NoWorkspaceFile => println!("No aplang.toml found!"),
                    ReadWorkspaceError::ParseErrors(errs) => {
                        for (file_id, error) in errs {
                            let mut colors = ariadne::ColorGenerator::new();
                            // Generate & choose some colours for each of our elements
                            let file = files.file_by_id(file_id).unwrap();
                            let input_name = file.path().as_display().to_string();
                            ariadne::Report::build(
                                ariadne::ReportKind::Error,
                                &input_name,
                                error.span().into_range().start,
                            )
                            .with_message(match error.reason() {
                                RichReason::ExpectedFound { .. } => "Unexpected",
                                RichReason::Custom(_) => "Custom",
                                RichReason::Many(_) => "Mnay",
                            })
                            .with_label(
                                ariadne::Label::new((&input_name, error.span().into_range()))
                                    .with_message(format!("{error:?}"))
                                    .with_color(colors.next()),
                            )
                            .finish()
                            .print((
                                &input_name,
                                ariadne::Source::from(file.src()),
                            ))
                            .unwrap();
                        }
                    }
                }
            }
        }
        ReadWorkspaceResult::Ok(_) => println!("Yay"),
    }

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
