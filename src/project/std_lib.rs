use std::path::Path;

use lasso::Rodeo;
use slotmap::SlotMap;

use super::{
    readers::{read_project_from_files, ReadProjectResult},
    scopes::{Scopes, ScopeType},
    Files, Project, VirtualFile,
};

const STD_LIB_SRC: &str = r##"
struct u8
struct u16
struct u32
struct u64
struct i8
struct i16
struct i32
struct i64
struct str

fn println(input: str) {}
"##;

pub fn create_std_lib(rodeo: &mut Rodeo) -> Project {
    let mut files = Files {
        files: SlotMap::with_key(),
    };
    let mut scopes = Scopes::new(Path::new("std"));
    let file_id = files.files.insert(Box::new(VirtualFile {
        src: STD_LIB_SRC.to_owned(),
        path: Path::new("std/std.aplang").into(),
    }));
    scopes.add(scopes.root_id(), ScopeType::File(rodeo.get_or_intern_static("std"), file_id));

    match read_project_from_files(rodeo, files, scopes) {
        ReadProjectResult::Err(_, _) => panic!("The std should compile!"),
        ReadProjectResult::Ok(p) => p,
    }
}
