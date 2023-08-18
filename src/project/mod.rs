pub mod files;
pub mod readers;
pub mod scopes;

use std::path::Path;

use crate::parsing::ast::declarations::Variable;
use lasso::Spur;
use slotmap::{new_key_type, SlotMap};

use crate::parsing::ast::declarations::{Function, Struct, UseDeclaration};

use self::{files::APLangWorkspaceFile, scopes::Scopes};

new_key_type! { pub struct FunctionId; }
new_key_type! { pub struct StructId; }
new_key_type! { pub struct VariableId; }
new_key_type! { pub struct FileId; }
new_key_type! { pub struct ModuleId; }
new_key_type! { pub struct DeclarationId; }

pub struct Workspace {
    pub project: Project,
    pub aplang_file: APLangWorkspaceFile,
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

pub struct ModuleData {
    pub imports: Box<[UseDeclaration]>,
    pub functions: Vec<(Spur, FunctionId)>,
    pub structs: Vec<(Spur, StructId)>,
    pub variables: Vec<(Spur, VariableId)>,
    pub file_id: FileId,
}
