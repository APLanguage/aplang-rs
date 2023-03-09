use super::declarations::Declaration;

#[derive(Debug)]
pub struct File {
    declarations: Box<[Declaration]>
}

impl File {
    pub fn new(declarations: Box<[Declaration]>) -> Self {
        File { declarations }
    }
}