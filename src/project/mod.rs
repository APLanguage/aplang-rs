pub mod files;
pub mod readers;
pub mod scopes;
pub mod std_lib;

use std::{cell::RefCell, collections::HashMap, path::Path, rc::Rc, slice::Iter};

use crate::{
    parsing::{ast::declarations::Variable, Spanned},
    resolution::{
        name_resolution::{
            ResolvedFunctionOutline, ResolvedStructOutline, ResolvedVariableOutline,
        },
        FileScopedNameResolver,
    },
    typing::{FloatWidth, IntegerWidth, PrimitiveType, Type, TypeId},
};
use chumsky::span::SimpleSpan;
use either::Either;
use lasso::Spur;
use slotmap::{new_key_type, SlotMap};

use crate::parsing::ast::declarations::{Function, Struct, UseDeclaration};

use self::{
    files::APLangWorkspaceFile,
    scopes::{ScopeId, ScopeType, Scopes},
};

new_key_type! { pub struct FunctionId; }
new_key_type! { pub struct StructId; }
new_key_type! { pub struct VariableId; }
new_key_type! { pub struct FileId; }
new_key_type! { pub struct ModuleId; }
new_key_type! { pub struct DeclarationId; }
new_key_type! { pub struct DependencyId; }

#[derive(Default)]
pub struct TypeRegistry {
    types: SlotMap<TypeId, Type>,
    primitives_by_spur: HashMap<Spur, (TypeId, PrimitiveType)>,
    primitives_by_name: HashMap<&'static str, (TypeId, PrimitiveType)>,
    type_id_by_type: HashMap<Type, TypeId>,
}

impl TypeRegistry {
    pub fn new_by_spur_supplier<F: FnMut(&'static str) -> Spur>(mut supplier: F) -> Self {
        let mut types = SlotMap::with_key();
        let mut primitives_by_spur = HashMap::new();
        let mut primitives_by_name = HashMap::new();
        let mut type_id_by_type = HashMap::new();

        macro_rules! insert_into {
            ($s:literal, $v:expr) => {
                let pt = $v;
                let ty = Type::PrimitiveType(pt);
                let ty_id = types.insert(ty.clone());
                primitives_by_name.insert($s, (ty_id, pt));
                primitives_by_spur.insert(supplier($s), (ty_id, pt));
                type_id_by_type.insert(ty, ty_id);
            };
        }

        insert_into!("str", PrimitiveType::String);
        insert_into!("bool", PrimitiveType::Boolean);
        insert_into!("i8", PrimitiveType::Integer(true, IntegerWidth::_8));
        insert_into!("i16", PrimitiveType::Integer(true, IntegerWidth::_16));
        insert_into!("i32", PrimitiveType::Integer(true, IntegerWidth::_32));
        insert_into!("i64", PrimitiveType::Integer(true, IntegerWidth::_64));
        insert_into!("u8", PrimitiveType::Integer(false, IntegerWidth::_8));
        insert_into!("u16", PrimitiveType::Integer(false, IntegerWidth::_16));
        insert_into!("u32", PrimitiveType::Integer(false, IntegerWidth::_32));
        insert_into!("u64", PrimitiveType::Integer(false, IntegerWidth::_64));
        insert_into!("f32", PrimitiveType::Float(FloatWidth::_32));
        insert_into!("f64", PrimitiveType::Float(FloatWidth::_64));
        insert_into!("unit", PrimitiveType::Unit);

        Self {
            types,
            primitives_by_spur,
            primitives_by_name,
            type_id_by_type,
        }
    }

    fn create_struct(&mut self, dependency_id: DependencyId, struct_id: StructId) -> TypeId {
        self.types.insert(Type::Data(dependency_id, struct_id))
    }

    pub fn get_declaration_from_type_id(
        &self,
        type_id: TypeId,
    ) -> Option<(DependencyId, StructId)> {
        match self.types.get(type_id)? {
            Type::Data(dep_id, struct_id) => Some((*dep_id, *struct_id)),
            _ => None,
        }
    }

    pub fn primitive_by_spur(&self, name_for_search: Spur) -> Option<(TypeId, PrimitiveType)> {
        self.primitives_by_spur.get(&name_for_search).cloned()
    }

    pub fn register_type(&mut self, ty: Type) -> TypeId {
        match self.type_id_by_type.get(&ty) {
            Some(ty) => *ty,
            None => self.types.insert(ty),
        }
    }

    pub fn get(&self, type_id: TypeId) -> Option<&Type> {
        self.types.get(type_id)
    }

    pub fn get_as_unknown(&self, type_id: TypeId) -> Option<(FileId, Spanned<Spur>)> {
        self.types.get(type_id).and_then(|t| match t {
            Type::Unknown(f, s) => Some((*f, *s)),
            _ => None,
        })
    }

    pub fn get_as_primitive(&self, type_id: TypeId) -> Option<PrimitiveType> {
        self.types.get(type_id).and_then(|t| match t {
            Type::PrimitiveType(t) => Some(*t),
            _ => None,
        })
    }

    pub fn get_as_struct(&self, type_id: TypeId) -> Option<(DependencyId, StructId)> {
        self.types.get(type_id).and_then(|t| match t {
            Type::Data(dep, struct_id) => Some((*dep, *struct_id)),
            _ => None,
        })
    }

    pub fn get_by_primitive_type(&self, primitive_type: PrimitiveType) -> TypeId {
        *self
            .type_id_by_type
            .get(&Type::PrimitiveType(primitive_type))
            .unwrap()
    }

    pub fn display_type(&self, type_id: TypeId) -> String {
        self.get(type_id).map_or_else(
            || "?".to_string(),
            |ty| match ty {
                Type::PrimitiveType(ty) => Self::display_primitive_type(ty).to_string(),
                Type::Data(_, _) => todo!("Type::Data"),
                Type::Array { ty: _, size: _ } => todo!("Type::Array"),
                Type::Function { parameters: _, retty: _ } => todo!("Type::Function"),
                Type::Trait(_) => todo!("Type::Trait"),
                Type::Union(_) => todo!("Type::Union"),
                Type::Intersection(_) => todo!("Type::Intersection"),
                Type::InternalUnion(_) => todo!("Type::InternalUnion"),
                Type::Ref(_) => todo!("Type::Ref"),
                Type::OperationResult(_) => todo!("Type::OperationResult"),
                Type::Unknown(_, _) => todo!("Type::Unknown"),
                Type::Error(_, _, e) => "!<".to_owned() + e + ">!",
                Type::Unit => todo!("Type::Unit"),
                Type::Nothing => todo!("Type::Nothing"),
            },
        ) + &format!("[{:?}]", type_id)
    }
    fn display_primitive_type(ty: &PrimitiveType) -> &str {
        use PrimitiveType::*;
        match ty {
            String => "str",
            Integer(signed, w) => match (signed, w) {
                (true, IntegerWidth::_8) => "i8",
                (true, IntegerWidth::_16) => "i16",
                (true, IntegerWidth::_32) => "i32",
                (true, IntegerWidth::_64) => "i64",
                (false, IntegerWidth::_8) => "u8",
                (false, IntegerWidth::_16) => "u16",
                (false, IntegerWidth::_32) => "u32",
                (false, IntegerWidth::_64) => "u64",
            },
            Float(w) => match w {
                FloatWidth::_32 => "f32",
                FloatWidth::_64 => "f64",
            },
            Boolean => "bool",
            Unit => "unit",
        }
    }
}


pub struct Workspace {
    pub aplang_file: APLangWorkspaceFile,
    pub dependencies: Dependencies,
    pub type_registery: Rc<RefCell<TypeRegistry>>,
    _project: DependencyId,
}

impl Workspace {
    pub fn project(&self) -> &Project {
        &self
            .dependencies
            .get_dependency(self._project)
            .unwrap()
            .project
    }

    pub fn project_mut(&mut self) -> &mut Project {
        &mut self
            .dependencies
            .get_dependency_mut(self._project)
            .unwrap()
            .project
    }

    pub fn project_dep_id(&self) -> DependencyId {
        self._project
    }

    pub fn resolver_by_scope(
        &self,
        dependency_id: DependencyId,
        scope_id: ScopeId,
    ) -> FileScopedNameResolver<'_> {
        FileScopedNameResolver::new_with_scope(
            &self.dependencies,
            self.type_registery.clone(),
            dependency_id,
            scope_id,
        )
    }

    pub fn resolver_by_file(
        &self,
        dependency_id: DependencyId,
        file_id: FileId,
    ) -> FileScopedNameResolver<'_> {
        FileScopedNameResolver::new_with_file(
            &self.dependencies,
            self.type_registery.clone(),
            dependency_id,
            file_id,
        )
    }
}

pub struct DeclarationResolutionStage<D, D2, D3> {
    pub ast: D,
    pub outline: Option<D2>,
    pub full: Option<D3>,
}

pub struct DeclarationInfo<D, D2 = (), D3 = ()> {
    pub decl: DeclarationResolutionStage<D, D2, D3>,
    pub file_id: FileId,
}

#[derive(Debug)]
pub enum DeclarationDelegate {
    Function(FunctionId),
    Struct(StructId),
    Variable(VariableId),
}

pub struct DeclarationPool {
    pub functions: SlotMap<FunctionId, DeclarationInfo<Function, ResolvedFunctionOutline>>,
    pub structs: SlotMap<StructId, DeclarationInfo<(Struct, TypeId), ResolvedStructOutline>>,
    pub variables: SlotMap<VariableId, DeclarationInfo<Variable, ResolvedVariableOutline>>,
    pub declarations: SlotMap<DeclarationId, DeclarationDelegate>,
}

impl DeclarationPool {
    pub fn declaration_id_of_func(&self, func_id: FunctionId) -> Option<DeclarationId> {
        for declr in self.declarations.iter() {
            let (id, DeclarationDelegate::Function(f_id)) = declr else {
                continue;
            };
            if func_id == *f_id {
                return Some(id);
            }
        }
        None
    }

    pub fn declaration_id_of_var(&self, var_id: VariableId) -> Option<DeclarationId> {
        for declr in self.declarations.iter() {
            let (id, DeclarationDelegate::Variable(v_id)) = declr else {
                continue;
            };
            if var_id == *v_id {
                return Some(id);
            }
        }
        None
    }

    pub fn declaration_id_of_struct(&self, strct_id: StructId) -> Option<DeclarationId> {
        for declr in self.declarations.iter() {
            let (id, DeclarationDelegate::Struct(s_id)) = declr else {
                continue;
            };
            if strct_id == *s_id {
                return Some(id);
            }
        }
        None
    }

    pub fn type_id_of_declaration(&self, dec_id: DeclarationId) -> Option<TypeId> {
        match self.declarations.get(dec_id)? {
            DeclarationDelegate::Struct(id) => Some(self.structs.get(*id)?.decl.ast.1),
            _ => None,
        }
    }

    pub fn declaration_id_as_func(&self, dec_id: DeclarationId) -> Option<FunctionId> {
        match self.declarations.get(dec_id)? {
            DeclarationDelegate::Function(id) => Some(*id),
            _ => None,
        }
    }

    pub fn declaration_id_as_var(&self, dec_id: DeclarationId) -> Option<VariableId> {
        match self.declarations.get(dec_id)? {
            DeclarationDelegate::Variable(id) => Some(*id),
            _ => None,
        }
    }

    pub fn declaration_id_as_struct(&self, dec_id: DeclarationId) -> Option<StructId> {
        match self.declarations.get(dec_id)? {
            DeclarationDelegate::Struct(id) => Some(*id),
            _ => None,
        }
    }
}

pub trait File {
    fn src(&self) -> &str;
    fn path(&self) -> &Path;
}

impl File for VirtualFile {
    fn src(&self) -> &str {
        &self.src
    }

    fn path(&self) -> &Path {
        &self.path
    }
}

pub struct VirtualFile {
    src: String,
    path: Box<Path>,
}

pub struct Files {
    pub files: SlotMap<FileId, Box<dyn File>>,
}

impl Files {
    pub fn file_by_id(&self, file_id: FileId) -> Option<&dyn File> {
        self.files.get(file_id).map(Box::as_ref)
    }
}

pub struct Project {
    pub src: AstFiles,
    pub pool: DeclarationPool,
    pub files: Files,
    pub scopes: Scopes,
}

pub struct AstFiles {
    files: SlotMap<ModuleId, ModuleData>,
    by_file: HashMap<FileId, ModuleId>,
}

impl AstFiles {
    fn new(modules: SlotMap<ModuleId, ModuleData>) -> AstFiles {
        let mut map = HashMap::<FileId, ModuleId>::new();
        for (m_id, m) in modules.iter() {
            map.insert(m.file_id, m_id);
        }
        AstFiles {
            files: modules,
            by_file: map,
        }
    }

    pub fn get_module_by_file(&self, file_id: FileId) -> Option<(ModuleId, &ModuleData)> {
        let module_id = *self.by_file.get(&file_id)?;
        Some((module_id, self.files.get(module_id)?))
    }

    pub fn get_module_by_file_mut(
        &mut self,
        file_id: FileId,
    ) -> Option<(ModuleId, &mut ModuleData)> {
        let module_id = *self.by_file.get(&file_id)?;
        Some((module_id, self.files.get_mut(module_id)?))
    }

    pub fn get_module_by_module(&self, module_id: ModuleId) -> Option<(ModuleId, &ModuleData)> {
        Some((module_id, self.files.get(module_id)?))
    }

    pub fn get_module_by_module_mut(
        &mut self,
        module_id: ModuleId,
    ) -> Option<(ModuleId, &mut ModuleData)> {
        Some((module_id, self.files.get_mut(module_id)?))
    }

    pub fn iter(&self) -> impl Iterator<Item = (ModuleId, &ModuleData)> {
        return self.files.iter();
    }
}
#[derive(Debug)]
pub struct UseTargetSingle {
    pub alias_name: Option<Spanned<Spur>>,
    pub name: Spur,
    pub scope: ScopeId,
}

#[derive(Debug)]
pub struct UseTargetStar {
    pub scope: ScopeId,
    pub end_targets: Box<[(Spur, ScopeId)]>,
}

#[derive(Debug)]
pub enum UseTarget {
    UseTargetStar(UseTargetStar),
    UseTargetSingle(UseTargetSingle),
}

pub struct ResolvedUsesInDependency(Vec<UseTargetStar>, Vec<UseTargetSingle>);

pub struct DependencyInfo {
    pub name: Spur,
    pub project: Project,
}

pub struct Dependencies {
    deps: SlotMap<DependencyId, DependencyInfo>,
    by_name: HashMap<Spur, DependencyId>,
}

impl Dependencies {
    pub fn new_from_project(name: Spur, project: Project) -> (Self, DependencyId) {
        let mut deps = Dependencies {
            deps: SlotMap::with_key(),
            by_name: HashMap::new(),
        };
        let id = deps.deps.insert(DependencyInfo { name, project });
        deps.by_name.insert(name, id);
        (deps, id)
    }

    pub fn get_dependency(&self, id: DependencyId) -> Option<&DependencyInfo> {
        self.deps.get(id)
    }

    pub fn get_dependency_mut(&mut self, id: DependencyId) -> Option<&mut DependencyInfo> {
        self.deps.get_mut(id)
    }

    pub fn add_dependency(&mut self, info: DependencyInfo) -> DependencyId {
        let name = info.name;
        let id = self.deps.insert(info);
        self.by_name.insert(name, id);
        id
    }

    pub fn add_dependency_with_key<F>(&mut self, f: F) -> DependencyId
    where F: FnOnce(DependencyId) -> DependencyInfo {
        let id = self.deps.insert_with_key(f);
        let name = unsafe { self.deps.get_unchecked(id) }.name;
        self.by_name.insert(name, id);
        id
    }

    pub fn get_dependency_by_name(&self, name: &[Spur]) -> Result<DependencyId, usize> {
        if name.len() != 1 {
            todo!("Dependencies#get_dependency_by_name: multi project workspaces")
        }
        self.by_name
            .get(name.get(0).unwrap())
            .map(DependencyId::clone)
            .ok_or(0)
    }

    pub fn get_dependency_scopes(&self, dependency_id: DependencyId) -> Option<&Scopes> {
        self.deps.get(dependency_id).map(|d| &d.project.scopes)
    }
}

pub struct ResolvedUses {
    uses: HashMap<DependencyId, ResolvedUsesInDependency>,
    errors: Vec<SimpleSpan>,
}

impl std::fmt::Debug for ResolvedUses {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ResolvedUses")
            .field("uses_amount", &self.uses.len())
            .field("errors_amount", &self.errors.len())
            .finish()
    }
}
impl ResolvedUses {
    pub(crate) fn new() -> Self {
        ResolvedUses {
            uses: HashMap::new(),
            errors: vec![],
        }
    }

    pub(crate) fn add(&mut self, dependency_id: DependencyId, target: UseTarget) {
        match self.uses.get_mut(&dependency_id) {
            Some(ruid) => {
                match target {
                    UseTarget::UseTargetStar(t) => ruid.0.push(t),
                    UseTarget::UseTargetSingle(t) => ruid.1.push(t),
                };
            }
            None => {
                self.uses.insert(dependency_id, {
                    let (vec_star, vec_single) = match target {
                        UseTarget::UseTargetStar(t) => (vec![t], vec![]),
                        UseTarget::UseTargetSingle(t) => (vec![], vec![t]),
                    };
                    ResolvedUsesInDependency(vec_star, vec_single)
                });
            }
        }
    }

    pub(crate) fn add_error(&mut self, span: SimpleSpan) {
        if !self.errors.contains(&span) {
            self.errors.push(span)
        }
    }

    pub fn iter_err(&self) -> Iter<'_, SimpleSpan> {
        return self.errors.iter();
    }

    pub fn find(&self, name: Spur) -> impl Iterator<Item = (DependencyId, ScopeId)> + '_ {
        self.find_in_single(name).chain(self.find_in_stars(name))
    }

    pub fn find_in_single(&self, name: Spur) -> impl Iterator<Item = (DependencyId, ScopeId)> + '_ {
        self.uses
            .iter()
            .flat_map(move |(dep_id, ResolvedUsesInDependency(_, singles))| {
                singles
                    .iter()
                    .filter(move |t| t.alias_name.map(|n| n.0 == name).unwrap_or(t.name == name))
                    .map(|t| (*dep_id, t.scope))
            })
    }

    pub fn find_in_stars(&self, name: Spur) -> impl Iterator<Item = (DependencyId, ScopeId)> + '_ {
        self.uses
            .iter()
            .flat_map(move |(dep_id, ResolvedUsesInDependency(stars, _))| {
                stars
                    .iter()
                    .flat_map(|t| t.end_targets.iter())
                    .filter_map(move |(target_name, id)| (*target_name == name).then_some(id))
                    .map(|&id| (*dep_id, id))
            })
    }

    pub fn find_struct<'a>(
        &'a self,
        dependencies: &'a Dependencies,
        name: Spur,
    ) -> impl Iterator<Item = (DependencyId, ScopeId, DeclarationId, StructId, TypeId)> + '_ {
        self.find(name)
            .filter_map(|(dep_id, scope_id)| self.filter_finding(dependencies, dep_id, scope_id))
    }

    pub fn find_struct_in_single<'a>(
        &'a self,
        dependencies: &'a Dependencies,
        name: Spur,
    ) -> impl Iterator<Item = (DependencyId, ScopeId, DeclarationId, StructId, TypeId)> + '_ {
        self.find_in_single(name)
            .filter_map(|(dep_id, scope_id)| self.filter_finding(dependencies, dep_id, scope_id))
    }

    pub fn find_struct_in_stars<'a>(
        &'a self,
        dependencies: &'a Dependencies,
        name: Spur,
    ) -> impl Iterator<Item = (DependencyId, ScopeId, DeclarationId, StructId, TypeId)> + '_ {
        self.find_in_stars(name)
            .filter_map(|(dep_id, scope_id)| self.filter_finding(dependencies, dep_id, scope_id))
    }
    fn filter_finding(
        &self,
        dependencies: &Dependencies,
        dep_id: DependencyId,
        scope_id: ScopeId,
    ) -> Option<(DependencyId, ScopeId, DeclarationId, StructId, TypeId)> {
        let dep_info = dependencies.get_dependency(dep_id)?;
        let scopes = &dep_info.project.scopes;

        let ScopeType::Declaration(_, dec_id) = scopes.scope(scope_id)? else {
            return None;
        };

        let DeclarationDelegate::Struct(struct_id) =
            dep_info.project.pool.declarations.get(*dec_id)?
        else {
            return None;
        };
        let type_id = dep_info.project.pool.structs.get(*struct_id)?.decl.ast.1;
        Some((dep_id, scope_id, *dec_id, *struct_id, type_id))
    }
}

pub struct ModuleData {
    pub imports: Either<Box<[UseDeclaration]>, ResolvedUses>,
    pub functions: Vec<(Spur, FunctionId)>,
    pub structs: Vec<(Spur, StructId)>,
    pub variables: Vec<(Spur, VariableId)>,
    pub file_id: FileId,
}
