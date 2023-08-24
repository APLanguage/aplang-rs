use std::path::Path;

use lasso::Rodeo;
use slotmap::SlotMap;

use super::{
    readers::{read_project_from_files, ReadProjectResult},
    scopes::{Scopes, ScopeType},
    Files, Project, VirtualFile, DependencyId, TypeRegistry,
};

const STD_LIB_SRC: &str = r##"
fn println(input: str) {}
"##;

pub fn create_std_lib(dependency_id: DependencyId, rodeo: &mut Rodeo, types: &mut TypeRegistry) -> Project {
    let mut files = Files {
        files: SlotMap::with_key(),
    };
    let mut scopes = Scopes::new(Path::new("std"));
    let file_id = files.files.insert(Box::new(VirtualFile {
        src: STD_LIB_SRC.to_owned(),
        path: Path::new("std/std.aplang").into(),
    }));
    scopes.add(scopes.root_id(), ScopeType::File(rodeo.get_or_intern_static("std"), file_id));

    match read_project_from_files(dependency_id, rodeo, files, scopes, types) {
        ReadProjectResult::Err(_, _) => panic!("The std should compile!"),
        ReadProjectResult::Ok(p) => p,
    }
}
