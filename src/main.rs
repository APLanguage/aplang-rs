#![allow(dead_code)]
#![allow(incomplete_features)]
#![allow(unused_macros)]
#![feature(trait_alias)]
#![feature(return_position_impl_trait_in_trait)]
#![feature(iter_collect_into)]

use std::{ops::Range, path::Path};

use crate::{
    parsing::{
        parsers::{expressions::expression_parser, file::File},
        tokenizer::Operation,
        Spanned,
    },
    project::{
        display_integer_type,
        readers::{read_workspace, ReadWorkspaceError, ReadWorkspaceResult},
        Workspace,
    },
    resolution::{
        name_resolution::resolve_workspace_outlines,
        type_resolution::{resolve_and_typecheck_functions, TypeResError},
    },
};
use ariadne::{Fmt, ReportBuilder};
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
        for (file_id, errs) in resolve_workspace_outlines(&mut rodeo, &mut workspace, dependency_id)
        {
            is_errors = true;
            let file = workspace.project().files.file_by_id(file_id).unwrap();
            let mut colors = ariadne::ColorGenerator::new();
            // Generate & choose some colours for each of our elements
            let input_name = file.path().as_display().to_string();
            for err in errs {
                ariadne::Report::build(ariadne::ReportKind::Error, &input_name, err.start)
                    .with_message("Not found something.")
                    .with_label(
                        ariadne::Label::new((&input_name, err.into_range()))
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
        for (file_id, errs) in resolve_and_typecheck_functions(&rodeo, &mut workspace, dependency_id) {
            is_errors = true;
            let file = workspace.project().files.file_by_id(file_id).unwrap();
            // Generate & choose some colours for each of our elements
            let input_name = file.path().as_display().to_string();
            for Spanned(err, span) in errs {
                let rep: ReportBuilder<'_, _> = ariadne::Report::<(&'_ str, Range<usize>)>::build(
                    ariadne::ReportKind::Error,
                    &input_name as &str,
                    span.start,
                );
                use TypeResError::*;
                let mut colors = ariadne::ColorGenerator::new();
                match err {
                    FunctionNotFound(Spanned(name, span), params) => {
                        let params = params
                            .into_iter()
                            .map(|ty_id| {
                                ty_id.map_new(|ty_id| {
                                    workspace.display_type(&rodeo, *ty_id)
                                })
                            })
                            .collect_vec();
                        let fn_color = ariadne::Color::Red;
                        let mut rep = rep
                            .with_message(format!("Couldn't find function `{}` matching parameter types", rodeo.resolve(&name).fg(fn_color)))
                            .with_label(
                                ariadne::Label::new((&input_name as &str, span.into_range()))
                                    .with_color(fn_color),
                            );
                        for param in params {
                            let param_color = colors.next();
                            rep.add_label(
                                ariadne::Label::new((&input_name as &str, param.1.into_range()))
                                    .with_color(param_color)
                                    .with_message(format!("this is of type: {}", param.0.fg(param_color))),
                            )
                        }
                        rep
                    }
                    VariableNotFound(Spanned(name, span), try_to_be) => {
                        let color = colors.next();
                        let mut rep = rep.with_message(format!("Variable `{}` cannot be found in scope", rodeo.resolve(&name).fg(color))).with_label(
                            ariadne::Label::new((&input_name as &str, span.into_range()))
                                .with_color(color),
                        );
                        if let Some(ty) = try_to_be {
                            rep.add_note(format!("The type of the variable needs to be a `{}`.", workspace.display_type(&rodeo, ty).fg(color)))
                        }
                        rep
                    }
                    TypesAreNotMatching(context, a, b) => {
                        use resolution::type_resolution::TypesAreNotMatchingContext::*;
                        let ty_a_color = colors.next();
                        let ty_a_str = workspace.display_type(&rodeo, a.0).fg(ty_a_color);
                        let ty_b_color = ariadne::Color::Red;
                        let ty_b_str = workspace.display_type(&rodeo, b.0).fg(ty_b_color);
                        rep
                            .with_message(match context {
                                If => format!("Ifs arms des not have matching types, both must be of type `{ty_a_str}`, but second arm is `{ty_b_str}`"),
                                Assignment => format!("Assignments right-hand's type does not match left-hand's type. Expected `{ty_a_str}`, got `{ty_b_str}`."),
                                FuncRet => format!("Return's expression's type does not match functions return type: function returns `{ty_a_str}` but the return's expression is of type `{ty_b_str}`."),
                            })
                            .with_label(
                                ariadne::Label::new((&input_name as &str, a.1.into_range()))
                                    .with_color(ty_a_color)
                                    .with_message(format!("this is of type: {ty_a_str}"))
                            )
                            .with_label(
                                ariadne::Label::new((&input_name as &str, b.1.into_range()))
                                    .with_color(ty_b_color)
                                    .with_message(format!("this is of type: {ty_b_str}"))
                            )
                    },
                    FieldNotFound(
                        Spanned((dep, struct_id), base_span),
                        Spanned(name, name_span),
                    ) => {
                        let dep = workspace.dependencies.get_dependency(dep).unwrap();
                        let project_name = rodeo.resolve(&dep.name);
                        let struct_path = &dep
                            .project
                            .struct_path(struct_id)
                            .map(|s| rodeo.resolve(&s))
                            .join("::");
                        let struct_color = colors.next();
                        rep.with_message(format!(
                            "No field `{}` found for struct ({}) {}",
                            rodeo.resolve(&name).fg(ariadne::Color::Red),
                            project_name.fg(struct_color),
                            struct_path.fg(struct_color),
                        ))
                        .with_label({
                            ariadne::Label::new((&input_name as &str, base_span.into_range()))
                                .with_color(struct_color)
                                .with_message(format!(
                                    "this returns struct: ({}) {}",
                                    project_name.fg(struct_color), struct_path.fg(struct_color),
                                ))
                        })
                        .with_label(
                            ariadne::Label::new((&input_name as &str, name_span.into_range()))
                                .with_color(ariadne::Color::Red)
                                .with_message("Cannot find this field"),
                        )
                    }
                    SignNotMatching(Spanned((s1, w1), span1),group, op, Spanned((s2, w2), span2)) => {
                        let int1_color = colors.next();
                        let int2_color = colors.next();
                        let op_color = colors.next();

                        rep
                        .with_message(format!(
                            "Mismatching signedness of ints: {} ({}) on {} and {} cannot be computed.",
                            op.0.fg(op_color),
                            group.fg(op_color),
                            display_integer_type(s1, w1).fg(int1_color),
                            display_integer_type(s2, w2).fg(int2_color)
                        ))
                        .with_note("Operations work only on the same number type (int or float),\nfor ints there is an additional requirement of them needing to have the same signedness.")
                        .with_label(
                            ariadne::Label::new((&input_name as &str, span1.into_range()))
                                .with_color(int1_color)
                                .with_message(format!(
                                    "This is of type {}",
                                    display_integer_type(s1, w1).fg(int1_color)
                                ))
                        )
                        .with_label(
                            ariadne::Label::new((&input_name as &str, span2.into_range()))
                                .with_color(int2_color)
                                .with_message(format!(
                                    "This is of type {}",
                                    display_integer_type(s2, w2).fg(int2_color)
                                ))
                        ).with_label(ariadne::Label::new((&input_name as &str, op.1.into_range()))
                        .with_color(op_color))
                    },
                    UnaryUnapplicable(Spanned(op, op_span), Spanned(ty, ty_span)) => {
                        let op_color = colors.next();
                        let ty_color = colors.next();
                        let ty_str = workspace.display_type(&rodeo, ty).fg(ty_color);
                        let op_str = match op {
                            Operation::Addition => "Plus",
                            Operation::Substraction | Operation::Not |Operation::NotBitwise => "Negation",
                            _ => Into::<&'static str>::into(op)
                        }.fg(op_color);
                        rep
                        .with_message(format!("{op_str} is not applicable for type `{ty_str}`"))
                        .with_label(
                            ariadne::Label::new((&input_name as &str, op_span.into_range()))
                                .with_color(op_color)
                                .with_message(format!("{op_str} of"))
                        )
                        .with_label(
                            ariadne::Label::new((&input_name as &str, ty_span.into_range()))
                                .with_color(ty_color)
                                .with_message(format!("type `{ty_str}`"))
                        )
                    },
                    FunctionReturnProblem(problem, Spanned(ty, ty_span)) => {
                        use resolution::type_resolution::FunctionReturnProblem::*;
                        let ty_color = colors.next();
                        let unit_ty_color = colors.next();
                        let ret_color = ariadne::Color::Red;
                        let unit_ty_str = "unit".fg(unit_ty_color);
                        match problem {
                            ExpectedEmptyReturn => {
                                let ty_str = workspace.display_type(&rodeo, ty).fg(ariadne::Color::Red);
                                rep.with_message(format!("Expected no expression for the return statement as the function returns `{unit_ty_str}`, but got an expression returning `{ty_str}`" ))
                                    .with_label(ariadne::Label::new((&input_name as &str, ty_span.into_range()))
                                        .with_color(ty_color)
                                        .with_message(format!("this expression returns a `{ty_str}`"))
                                    ).with_note(format!("Either remove the expression, or make the function return `{ty_str}`."))
                                },
                            ExpectedAReturnExpr => {
                                let ty_str = workspace.display_type(&rodeo, ty).fg(ty_color);
                                rep.with_message(format!("Expected an expression for the return statement as the function returns `{ty_str}`."))
                                    .with_label(ariadne::Label::new((&input_name as &str, span.into_range()))
                                        .with_color(ret_color)
                                        .with_message("this return has no expression")
                                    ).with_label(ariadne::Label::new((&input_name as &str, ty_span.into_range()))
                                        .with_color(ty_color)
                                        .with_message(format!("this function returns `{ty_str}`"))
                                    ).with_note(format!("Either add an expression, or make the function return `{unit_ty_str}`."))
                                },
                        }
                    },
                    ConditionNotBool(Spanned(ty, ty_span)) => {
                        let ty_str = workspace.display_type(&rodeo, ty).fg(ariadne::Color::Red);
                        let bool_ty_color = colors.next();
                        let bool_ty_str = "bool".fg(bool_ty_color);
                        
                        rep
                            .with_message(format!("Expected condition of type `{bool_ty_str}` but got `{ty_str}`."))
                            .with_label(ariadne::Label::new((&input_name as &str, ty_span.into_range()))
                                .with_color(ariadne::Color::Red)
                                .with_message(format!("this condition is of type `{ty_str}`"))
                            )
                    },
                    BinaryHandsNotPrimitive(group, Spanned(op, op_span), _error, Spanned(lhs_ty, lhs_span), Spanned(rhs_ty, rhs_span)) => {
                        let lhs_color = colors.next();
                        let rhs_color = colors.next();
                        let op_color = colors.next();
                        let lhs_ty_str = workspace.display_type(&rodeo, lhs_ty).fg(lhs_color);
                        let rhs_ty_str = workspace.display_type(&rodeo, rhs_ty).fg(rhs_color);
                        let op_str = Into::<&'static str>::into(op).fg(op_color);
                        let group_str =  Into::<&'static str>::into(group).fg(op_color);

                        rep.with_message(format!("Cannot compute {op_str} ({group_str}) for types `{lhs_ty_str}` and `{rhs_ty_str}`"))
                        .with_label(
                            ariadne::Label::new((&input_name as &str, lhs_span.into_range()))
                                .with_color(lhs_color)
                                .with_message(format!("This is of type {lhs_ty_str}"))
                        )
                        .with_label(
                            ariadne::Label::new((&input_name as &str, rhs_span.into_range()))
                                .with_color(rhs_color)
                                .with_message(format!("This is of type {rhs_ty_str}"))
                        ).with_label(
                            ariadne::Label::new((&input_name as &str, op_span.into_range()))
                                .with_color(op_color)
                                .with_message(format!("Using binary operation {op_str} ({group_str})"))
                        )
                    },
                    ExpectedStructForCallChain(ty, span) => {
                        let ty_color = ariadne::Color::Red;
                        let ty_str = workspace.display_type(&rodeo, ty).fg(ty_color);
                        rep.with_message(format!("Expected a struct but got `{ty_str}`."))
                            .with_label(ariadne::Label::new(
                                (&input_name as &str, span.into_range())
                            ).with_message(format!("This should have been a struct, but got `{ty_str}`"))
                        .with_color(ty_color))
                    },
                    VariableExpectedForAssignment(span) => rep
                        .with_message("Cannot set a non-field for assignment's lhs.")
                        .with_label(ariadne::Label::new(
                            (&input_name as &str, span.into_range())
                        ).with_message("This is not a field.").with_color(ariadne::Color::Red)),
                }
                .finish()
                .print((&input_name as &str, ariadne::Source::from(file.src())))
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
