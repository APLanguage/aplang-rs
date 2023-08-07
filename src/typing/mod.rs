pub mod typedast;

use chumsky::span::SimpleSpan;
use either::Either::{Left, Right};
use itertools::Itertools;
use lasso::{Rodeo, Spur};
use std::{num::NonZeroUsize, ops::Range};

use crate::{parsing::{
    ast::ParsedType,
    utilities::Spanned,
    parsers::file::File,
}, source::DeclarationPath};

pub type TypeId = usize;

#[derive(Debug, PartialEq, Clone)]
pub enum OperationResult {
    If {
        condition: TypeId,
        then: TypeId,
        other: Option<TypeId>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Data(DeclarationPath),
    Array {
        ty: TypeId,
        size: Option<NonZeroUsize>,
    },
    Function {
        parameters: Box<[TypeId]>,
        retty: Option<TypeId>,
    },
    Trait(DeclarationPath),
    Union(Box<[TypeId]>),
    Intersection(Box<[TypeId]>),
    InternalUnion(Box<[TypeId]>),
    Ref(TypeId),
    OperationResult(OperationResult),
    Unknown,
    Unit,
    Nothing,
}

pub enum PrimitiveType {
    String(bool),
}

trait Context<'a> {
    fn rodeo(&mut self) -> &mut Rodeo;

    fn read_source(&self, range: Range<usize>) -> &'a str;

    fn resolve_single(&self, module_id: usize, path: &SpurPath) -> Result<DeclarationPath, usize>;
    fn resolve_single_spanned(
        &self,
        module_id: usize,
        path: &SpannedSpurPath,
    ) -> Result<DeclarationPath, SimpleSpan>;

    fn resolve_multiple(&self, module_id: usize, path: DeclarationPath) -> Box<[DeclarationPath]>;

    fn resolve_module(&self, path: &SpurPath) -> Result<usize, usize>;
    fn resolve_module_spanned(&self, path: &SpannedSpurPath) -> Result<usize, SimpleSpan>;

    fn resolve_type(&mut self, ty: &ParsedType) -> Option<TypeId>;
    fn register_type(&mut self, ty: Type) -> TypeId;
    fn get_type(&self, id: TypeId) -> &Type;
    fn get_type_primitive(&self, primitive_type: PrimitiveType) -> TypeId;

    fn report_error(&mut self, error: ResolutionError);

    fn push_import_scope(&mut self, imports: Box<[DeclarationImportType]>);
    fn pop_import_scope(&mut self);

    fn spurpath_of_spans(&mut self, spans: &[SimpleSpan]) -> SpurPath {
        spans
            .iter()
            .map(|s| {
                let src = self.read_source(s.into_range());
                self.rodeo().get_or_intern(src)
            })
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }

    fn spannedspurpath_of_spans(&mut self, spans: &[SimpleSpan]) -> SpannedSpurPath {
        spans
            .iter()
            .map(|s| {
                let src = self.read_source(s.into_range());
                Spanned(self.rodeo().get_or_intern(src), *s)
            })
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }
}

#[derive(Debug)]
enum DeclarationImportType {
    Single(DeclarationPath),
    Multiple(DeclarationPath, Box<[DeclarationPath]>),
}

#[derive(Debug)]
enum ResolutionError {
    ModuleNotFound(SimpleSpan),
    PathNotFound(SimpleSpan),
    Cummulative(Box<[Result<DeclarationImportType, ResolutionError>]>),
    CouldNotResolveType(SimpleSpan),
}

type SpannedSpurPath = Box<[Spanned<Spur>]>;
type SpurPath = Box<[Spur]>;

#[derive(Debug)]
struct InternedDeclaration {
    module_path: SpannedSpurPath,
    import_path: SpannedSpurPath,
    star: bool,
}

struct ResolutionContext {
    single: Box<[DeclarationPath]>,
    multiple: Box<[(DeclarationPath, Box<[DeclarationPath]>)]>,
}

fn visit_file<'a, C: Context<'a>>(context: &mut C, file: &mut File) {
    let mut imports: Vec<InternedDeclaration> = vec![];
    for value in file.0.iter_mut() {
        imports.extend(
            value
                .flatten_tree()
                .map(|(span_path, star)| InternedDeclaration {
                    module_path: context.spannedspurpath_of_spans(&value.0),
                    import_path: context.spannedspurpath_of_spans(&span_path),
                    star,
                }),
        );
    }
    let (imports, errors): (Vec<_>, Vec<_>) = imports
        .iter_mut()
        .map(|interned| {
            let module_id = context
                .resolve_module_spanned(&interned.module_path)
                .map_err(ResolutionError::ModuleNotFound)?;
            let declr_path = context
                .resolve_single_spanned(module_id, &interned.import_path)
                .map_err(ResolutionError::PathNotFound)?;
            if interned.star {
                Ok::<_, ResolutionError>(DeclarationImportType::Multiple(
                    declr_path,
                    context.resolve_multiple(module_id, declr_path),
                ))
            } else {
                Ok::<_, ResolutionError>(DeclarationImportType::Single(declr_path))
            }
        })
        .partition_map(|res| match res {
            Ok(success) => Left(success),
            Err(failure) => Right(failure),
        });
    if !errors.is_empty() {
        for ele in errors {
            context.report_error(ele)
        }
    }
    context.push_import_scope(imports.into_boxed_slice());
}
