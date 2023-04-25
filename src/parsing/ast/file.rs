use super::declarations::{Declaration};

#[derive(Debug)]
pub struct File {
    pub declarations: Box<[Declaration]>
}