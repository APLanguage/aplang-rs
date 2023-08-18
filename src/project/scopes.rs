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
    pub fn new(root_path: &Path) -> Self {
        let mut tree = IndexArena::new();
        let root = tree.new_node(ScopeType::Root(root_path.into()));
        Scopes { tree, root }
    }

    pub fn root_id(&self) -> ScopeId {
        ScopeId { node: self.root }
    }

    pub fn scope(&self, id: ScopeId) -> Option<&ScopeType> {
        self.tree.get(id.node).map(Node::get)
    }

    pub fn root_path(&self) -> &Path {
        match self.scope(self.root_id()).unwrap() {
            ScopeType::Root(p) => p,
            _ => unreachable!(),
        }
    }

    pub fn add(&mut self, scope: ScopeId, scope_type: ScopeType) -> ScopeId {
        let new = self.tree.new_node(scope_type);
        scope.node.append(new, &mut self.tree);
        ScopeId { node: new }
    }

    pub fn parent(&self, scope: ScopeId) -> Option<ScopeId> {
        self.tree
            .get(scope.node)
            .and_then(|node| node.parent().map(|node| ScopeId { node }))
    }
}
