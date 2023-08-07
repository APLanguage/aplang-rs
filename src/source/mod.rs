use std::ops::Range;

use indextree::NodeId;
use logos::Source;

pub trait SourceFile {
    fn read_range(&self, range: Range<usize>) -> Option<&str>;
    fn whole_file(&self) -> &str;
}

pub struct VirtualFile {
    source: String,
}

impl VirtualFile {
    pub fn new(source: String) -> Self {
        Self { source }
    }
}

impl SourceFile for VirtualFile {
    fn read_range(&self, range: Range<usize>) -> Option<&str> {
        self.source.slice(range)
    }

    fn whole_file(&self) -> &str {
        &self.source
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Debug, Hash)]
pub struct DeclarationPath {
    module_id: NodeId,
    declaration_id: NodeId,
}

