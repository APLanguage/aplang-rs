#![allow(dead_code)]
#![allow(incomplete_features)]
#![allow(unused_macros)]
#![feature(trait_alias)]
#![feature(iter_collect_into)]
// #![feature(iter_map_windows)]

use aplang_parser::{ast::declarations::{FlatUseDeclaration, UseDeclaration}, tokenizer::Token};
use aplang_source::{SourceFile, VirtualFile};
use chumsky::{error::{Rich, RichReason}, ParseResult};

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
    // let file = &VirtualFile::new(input);
    // let result = parse_file(&mut rodeo, file);

    // println!("{:#?}", result);

    // print_errors(&result, file);
    println!("Reading workspace...");

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

fn print_uses(uses: &[UseDeclaration], file: &VirtualFile) {
    use itertools::Itertools;

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
