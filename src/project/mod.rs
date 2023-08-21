pub mod files;
pub mod name_resolver;
pub mod readers;
pub mod scopes;

use std::{collections::HashMap, hash::Hash, path::Path};

use crate::parsing::ast::declarations::Variable;
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

pub struct Workspace {
    pub aplang_file: APLangWorkspaceFile,
    pub dependencies: Dependencies,
    _project: DependencyId,
}

impl Workspace {
    fn project(&self) -> &Project {
        &self
            .dependencies
            .get_dependency(self._project)
            .unwrap()
            .project
    }

    fn project_mut(&mut self) -> &mut Project {
        &mut self
            .dependencies
            .get_dependency_mut(self._project)
            .unwrap()
            .project
    }

    fn project_dep_id(&self) -> DependencyId {
        self._project.clone()
    }
}

pub struct DeclarationInfo<D> {
    decl: D,
    file_id: FileId,
}

pub enum DeclarationDelegate {
    Function(FunctionId),
    Struct(StructId),
    Variable(VariableId),
}

pub struct DeclarationPool {
    pub functions: SlotMap<FunctionId, DeclarationInfo<Function>>,
    pub structs: SlotMap<StructId, DeclarationInfo<Struct>>,
    pub variables: SlotMap<VariableId, DeclarationInfo<Variable>>,
    pub declarations: SlotMap<DeclarationId, DeclarationDelegate>,
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

enum FileOrFolder {
    File(FileId),
    Folder(Spur),
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
    pub files: SlotMap<ModuleId, ModuleData>,
}
#[derive(Debug)]
pub struct UseTargetSingle {
    pub name: Spur,
    pub scope: ScopeId,
}

#[derive(Debug)]
pub struct UseTargetStar {
    pub name: Spur,
    pub scope: ScopeId,
    pub end_targets: Box<[(Spur, ScopeId)]>,
}

#[derive(Debug)]
pub enum UseTarget {
    UseTargetStar(UseTargetStar),
    UseTargetSingle(UseTargetSingle),
}

pub type ResolvedUsesInDependency = (Vec<UseTargetStar>, Vec<UseTargetSingle>);

pub struct DependencyInfo {
    name: Spur,
    project: Project,
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
        self.deps.insert(info)
    }

    fn get_dependency_by_name(&self, name: &[Spur]) -> Option<DependencyId> {
        if name.len() != 1 {
            todo!("Dependencies#get_dependency_by_name: multi project workspaces")
        }
        self.by_name
            .get(name.get(0).unwrap())
            .map(DependencyId::clone)
    }

    fn get_dependency_scope(&self, dependency_id: DependencyId) -> Option<&Scopes> {
        self.deps.get(dependency_id).map(|d| &d.project.scopes)
    }
}

#[derive(Debug)]
pub struct ResolvedUses {
    uses: HashMap<DependencyId, ResolvedUsesInDependency>,
    errors: Vec<SimpleSpan>,
}
impl ResolvedUses {
    fn new() -> Self {
        todo!("ResolvedUses#new")
    }

    fn add(&mut self, dependency_id: DependencyId, scope_id: ScopeId, star: bool) {
        todo!("ResolvedUses#add")
    }

    fn add_error(&mut self, span: SimpleSpan) {
        self.errors.push(span)
    }
}

pub struct ModuleData {
    pub imports: Either<Box<[UseDeclaration]>, ResolvedUses>,
    pub functions: Vec<(Spur, FunctionId)>,
    pub structs: Vec<(Spur, StructId)>,
    pub variables: Vec<(Spur, VariableId)>,
    pub file_id: FileId,
}
