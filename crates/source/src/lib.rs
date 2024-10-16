use std::ops::Range;

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

pub struct RefVirtualFile<'a> {
    source: &'a str
}

impl <'a> RefVirtualFile<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source }
    }
}

impl <'a> SourceFile for RefVirtualFile<'a> {
    fn read_range(&self, range: Range<usize>) -> Option<&str> {
        self.source.slice(range)
    }

    fn whole_file(&self) -> &str {
        self.source
    }
}
