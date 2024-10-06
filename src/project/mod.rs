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
use itertools::Itertools;
use lasso::{Rodeo, Spur};
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ProjectLink {
    Dependency(DependencyId),
    Project,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StructLink {
    pub struct_id: StructId,
    pub project_link: ProjectLink,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FunctionLink {
    pub func_id: FunctionId,
    pub project_link: ProjectLink,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct VariableLink {
    pub var_id: VariableId,
    pub project_link: ProjectLink,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FieldLink {
    pub struct_link: StructLink,
    pub field: usize,
}

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

    fn create_struct(&mut self, struct_link: StructLink) -> TypeId {
        self.types.insert(Type::Data(struct_link))
    }

    pub fn get_declaration_from_type_id(&self, type_id: TypeId) -> Option<StructLink> {
        match self.types.get(type_id)? {
            Type::Data(link) => Some(*link),
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

    pub fn get_as_primitive(&self, type_id: TypeId) -> Option<PrimitiveType> {
        self.types.get(type_id).and_then(|t| match t {
            Type::PrimitiveType(t) => Some(*t),
            _ => None,
        })
    }

    pub fn get_as_struct(&self, type_id: TypeId) -> Option<StructLink> {
        self.types.get(type_id).and_then(|t| match t {
            Type::Data(link) => Some(*link),
            _ => None,
        })
    }

    pub fn get_by_primitive_type(&self, primitive_type: PrimitiveType) -> TypeId {
        *self
            .type_id_by_type
            .get(&Type::PrimitiveType(primitive_type))
            .unwrap()
    }

    pub fn display_type(&self, rodeo: &Rodeo, workspace: &Workspace, type_id: TypeId) -> String {
        self.get(type_id).map_or_else(
            || "?".to_string(),
            |ty| match ty {
                Type::PrimitiveType(ty) => Self::display_primitive_type(*ty).to_string(),
                Type::Data(StructLink {
                    struct_id,
                    project_link,
                }) => {
                    let project_name = rodeo.resolve(&workspace.get_dependency_name(*project_link));
                    let struct_path = workspace
                        .get_project(*project_link)
                        .struct_path(*struct_id)
                        .map(|s| rodeo.resolve(&s))
                        .join("::");
                    format!("({project_name}) {struct_path}")
                }
                Type::Error => format!("!<err [{:?}]>!", type_id),
                Type::Array { ty: _, size: _ } => todo!("Type::Array"),
                Type::Ref(_) => todo!("Type::Ref"),
                Type::Unit => todo!("Type::Unit"),
                Type::Nothing => todo!("Type::Nothing"),
            },
        )
    }

    pub fn display_primitive_type(ty: PrimitiveType) -> &'static str {
        use PrimitiveType::*;
        match ty {
            String => "str",
            Integer(signed, w) => display_integer_type(signed, w),
            Float(w) => match w {
                FloatWidth::_32 => "f32",
                FloatWidth::_64 => "f64",
            },
            Boolean => "bool",
            Unit => "unit",
        }
    }

    pub fn is_error(&self, ty: TypeId) -> bool {
        self.types
            .get(ty)
            .map(|t| matches!(t, Type::Error))
            .unwrap_or(false)
    }
}

pub fn display_integer_type(signed: bool, w: IntegerWidth) -> &'static str {
    match (signed, w) {
        (true, IntegerWidth::_8) => "i8",
        (true, IntegerWidth::_16) => "i16",
        (true, IntegerWidth::_32) => "i32",
        (true, IntegerWidth::_64) => "i64",
        (false, IntegerWidth::_8) => "u8",
        (false, IntegerWidth::_16) => "u16",
        (false, IntegerWidth::_32) => "u32",
        (false, IntegerWidth::_64) => "u64",
    }
}

pub struct Workspace {
    pub aplang_file: APLangWorkspaceFile,
    pub dependencies: Dependencies,
    pub type_registery: Rc<RefCell<TypeRegistry>>,
    project_name: Spur,
    project: Project,
}

impl Workspace {
    pub fn project(&self) -> &Project {
        &self.project
    }

    pub fn project_mut(&mut self) -> &mut Project {
        &mut self.project
    }

    pub fn resolver_by_scope<'a>(
        &'a self,
        rodeo: &'a Rodeo,
        project_link: ProjectLink,
        scope_id: ScopeId,
    ) -> FileScopedNameResolver<'a> {
        FileScopedNameResolver::new_with_scope(
            self,
            self.type_registery.clone(),
            project_link,
            scope_id,
            rodeo,
        )
    }

    pub fn resolver_by_file<'a>(
        &'a self,
        rodeo: &'a Rodeo,
        project_link: ProjectLink,
        file_id: FileId,
    ) -> FileScopedNameResolver<'a> {
        FileScopedNameResolver::new_with_file(
            self,
            self.type_registery.clone(),
            project_link,
            file_id,
            rodeo,
        )
    }

    pub fn display_type(&self, rodeo: &Rodeo, ty: TypeId) -> String {
        self.type_registery.borrow().display_type(rodeo, self, ty)
    }

    pub fn get_project(&self, project_link: ProjectLink) -> &Project {
        match project_link {
            ProjectLink::Dependency(dep_id) => {
                self.dependencies.deps.get(dep_id).map(|d| &d.project).expect("A dependency wasn't found by DependencyId, this means it is a dead id and is a bug.")
            }
            ProjectLink::Project => &self.project,
        }
    }

    pub fn get_project_mut(&mut self, project_link: ProjectLink) -> &mut Project {
        match project_link {
            ProjectLink::Dependency(dep_id) => self
                .dependencies
                .deps
                .get_mut(dep_id)
                .map(|d| &mut d.project).expect("A dependency wasn't found by DependencyId, this means it is a dead id and is a bug."),
            ProjectLink::Project => &mut self.project,
        }
    }

    pub fn get_dependency_name(&self, project_link: ProjectLink) -> Spur {
        match project_link {
            ProjectLink::Dependency(dep_id) => self.dependencies.deps.get(dep_id).unwrap().name,
            ProjectLink::Project => self.project_name,
        }
    }

    pub fn get_scopes(&self, project_link: ProjectLink) -> &Scopes {
        &self.get_project(project_link).scopes
    }

    pub fn get_project_by_name(&self, name: &[Spur]) -> Result<ProjectLink, usize> {
        if name.len() != 1 {
            todo!("Dependencies#get_dependency_by_name: multi project workspaces")
        }
        if *name.first().unwrap() == self.project_name {
            Ok(ProjectLink::Project)
        } else {
            self.dependencies.get_dependency_by_name(name)
        }
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
impl Project {
    pub fn struct_path(&self, struct_id: StructId) -> impl Iterator<Item = Spur> + '_ {
        let mut scope_id = self
            .scopes
            .scope_of_declaration(self.pool.declaration_id_of_struct(struct_id).unwrap())
            .unwrap();
        let root = self.scopes.root_id();
        let mut path = vec![scope_id];

        while let Some(s) = self.scopes.parent(scope_id) {
            scope_id = s;
            if scope_id == root {
                break;
            }
            path.push(scope_id);
        }

        path.into_iter()
            .rev()
            .map(|s| *match self.scopes.scope(s).unwrap() {
                ScopeType::Declaration(s, _) => s,
                ScopeType::File(s, _) => s,
                ScopeType::Package(s) => s,
                ScopeType::Root(_) => unreachable!(),
            })
    }
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

    fn get_dependency_by_name(&self, name: &[Spur]) -> Result<ProjectLink, usize> {
        if name.len() != 1 {
            todo!("Dependencies#get_dependency_by_name: multi project workspaces")
        }
        self.by_name
            .get(name.first().unwrap())
            .copied()
            .map(ProjectLink::Dependency)
            .ok_or(0)
    }

    pub fn get_dependency_scopes(&self, dependency_id: DependencyId) -> Option<&Scopes> {
        self.deps.get(dependency_id).map(|d| &d.project.scopes)
    }
}

pub struct ResolvedUses {
    uses: HashMap<ProjectLink, ResolvedUsesInDependency>,
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

    pub(crate) fn add(&mut self, project_link: ProjectLink, target: UseTarget) {
        match self.uses.get_mut(&project_link) {
            Some(ruid) => {
                match target {
                    UseTarget::UseTargetStar(t) => ruid.0.push(t),
                    UseTarget::UseTargetSingle(t) => ruid.1.push(t),
                };
            }
            None => {
                self.uses.insert(project_link, {
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

    pub fn find(&self, name: Spur) -> impl Iterator<Item = (ProjectLink, ScopeId)> + '_ {
        self.find_in_single(name).chain(self.find_in_stars(name))
    }

    pub fn find_in_single(&self, name: Spur) -> impl Iterator<Item = (ProjectLink, ScopeId)> + '_ {
        self.uses.iter().flat_map(
            move |(project_link, ResolvedUsesInDependency(_, singles))| {
                singles
                    .iter()
                    .filter(move |t| t.alias_name.map(|n| n.0 == name).unwrap_or(t.name == name))
                    .map(|t| (*project_link, t.scope))
            },
        )
    }

    pub fn find_in_stars(&self, name: Spur) -> impl Iterator<Item = (ProjectLink, ScopeId)> + '_ {
        self.uses
            .iter()
            .flat_map(move |(project_link, ResolvedUsesInDependency(stars, _))| {
                stars
                    .iter()
                    .flat_map(|t| t.end_targets.iter())
                    .filter_map(move |(target_name, id)| (*target_name == name).then_some(id))
                    .map(|&id| (*project_link, id))
            })
    }

    pub fn find_struct<'a>(
        &'a self,
        workspace: &'a Workspace,
        name: Spur,
    ) -> impl Iterator<Item = (ProjectLink, ScopeId, DeclarationId, StructId, TypeId)> + 'a {
        self.find(name).filter_map(|(project_link, scope_id)| {
            self.filter_finding(workspace, project_link, scope_id)
        })
    }

    pub fn find_struct_in_single<'a>(
        &'a self,
        workspace: &'a Workspace,
        name: Spur,
    ) -> impl Iterator<Item = (ProjectLink, ScopeId, DeclarationId, StructId, TypeId)> + 'a {
        self.find_in_single(name)
            .filter_map(|(project_link, scope_id)| {
                self.filter_finding(workspace, project_link, scope_id)
            })
    }

    pub fn find_struct_in_stars<'a>(
        &'a self,
        workspace: &'a Workspace,
        name: Spur,
    ) -> impl Iterator<Item = (ProjectLink, ScopeId, DeclarationId, StructId, TypeId)> + 'a {
        self.find_in_stars(name)
            .filter_map(|(project_link, scope_id)| {
                self.filter_finding(workspace, project_link, scope_id)
            })
    }
    fn filter_finding(
        &self,
        workspace: &Workspace,
        project_link: ProjectLink,
        scope_id: ScopeId,
    ) -> Option<(ProjectLink, ScopeId, DeclarationId, StructId, TypeId)> {
        let project = workspace.get_project(project_link);
        let scopes = &project.scopes;

        let ScopeType::Declaration(_, dec_id) = scopes.scope(scope_id)? else {
            return None;
        };

        let DeclarationDelegate::Struct(struct_id) = project.pool.declarations.get(*dec_id)? else {
            return None;
        };
        let type_id = project.pool.structs.get(*struct_id)?.decl.ast.1;
        Some((project_link, scope_id, *dec_id, *struct_id, type_id))
    }
}

pub struct ModuleData {
    pub imports: Either<Box<[UseDeclaration]>, ResolvedUses>,
    pub functions: Vec<(Spur, FunctionId)>,
    pub structs: Vec<(Spur, StructId)>,
    pub variables: Vec<(Spur, VariableId)>,
    pub file_id: FileId,
}
