use std::path::Path;

use indextree::{Arena as IndexArena, Node, NodeId};
use lasso::Spur;
use slotmap::{new_key_type, SlotMap};

use super::{DeclarationId, FileId};

#[derive(Debug, Clone)]
pub enum ScopeType {
    Declaration(DeclarationId),
    File(FileId),
    Package(Spur),
    Root(Box<Path>),
}

pub struct Scopes {
    tree: IndexArena<ScopeType>,
    root: NodeId,
}

#[derive(Debug, Clone, Copy)]
pub struct ScopeId {
    node: NodeId,
}

impl Scopes {
    fn new(root_path: &Path) -> Self {
        let mut tree = IndexArena::new();
        let root = tree.new_node(ScopeType::Root(root_path.clone().into()));
        Scopes { tree, root }
    }

    fn root_id(&self) -> ScopeId {
        ScopeId { node: self.root }
    }

    fn scope(&self, id: ScopeId) -> Option<&ScopeType> {
        self.tree.get(id.node).map(Node::get)
    }

    fn root_path(&self) -> &Path {
        match self.scope(self.root_id()).unwrap() {
            ScopeType::Root(p) => p,
            _ => unreachable!(),
        }
    }

    fn add(&mut self, scope: ScopeId, scope_type: ScopeType) -> ScopeId {
        let new = self.tree.new_node(scope_type);
        scope.node.append(new, &mut self.tree);
        ScopeId { node: new }
    }
}
