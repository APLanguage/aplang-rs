use std::{collections::HashMap, path::Path};

use indextree::{Arena as IndexArena, Node, NodeId};
use lasso::Spur;

use super::{DeclarationId, FileId};

#[derive(Debug, Clone)]
pub enum ScopeType {
    Declaration(Spur, DeclarationId),
    File(Spur, FileId),
    Package(Spur),
    Root(Box<Path>),
}

pub struct Scopes {
    tree: IndexArena<ScopeType>,
    by_file: HashMap<FileId, NodeId>,
    by_declaration: HashMap<DeclarationId, NodeId>,
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
        Scopes {
            tree,
            root,
            by_file: HashMap::new(),
            by_declaration: HashMap::new(),
        }
    }

    pub fn root_id(&self) -> ScopeId {
        ScopeId { node: self.root }
    }

    pub fn scope(&self, id: ScopeId) -> Option<&ScopeType> {
        self.tree.get(id.node).map(Node::get)
    }

    pub fn scope_as_declaration(&self, id: ScopeId) -> Option<(Spur, DeclarationId)> {
        match self.tree.get(id.node).map(Node::get) {
            Some(ScopeType::Declaration(name, id)) => Some((*name, *id)),
            _ => None,
        }
    }

    pub fn scope_child_by_name(&self, id: ScopeId, name: Spur) -> Option<ScopeId> {
        let mut node_opt = self.tree.get(id.node)?.first_child();
        while let Some(node_id) = node_opt {
            let node = self.tree.get(node_id)?;
            if *match node.get() {
                ScopeType::Declaration(spur, _) => spur,
                ScopeType::File(spur, _) => spur,
                ScopeType::Package(spur) => spur,

                ScopeType::Root(_) => unreachable!(),
            } == name
            {
                break;
            }
            node_opt = node.next_sibling();
        }
        node_opt.map(|node| ScopeId { node })
    }

    pub fn scope_child_by_path(&self, id: ScopeId, path: &[Spur]) -> Result<ScopeId, usize> {
        let mut itr = path.iter();
        let mut i = 0;
        let mut path_frag_opt = itr.next().and_then(|&s| self.scope_child_by_name(id, s));
        while let Some(path_frag) = path_frag_opt {
            let Some(&next_frag) = itr.next() else {
                break;
            };
            i += 1;
            path_frag_opt = self.scope_child_by_name(path_frag, next_frag)
        }

        path_frag_opt.ok_or(i)
    }

    pub fn root_path(&self) -> &Path {
        match self.scope(self.root_id()).unwrap() {
            ScopeType::Root(p) => p,
            _ => unreachable!(),
        }
    }

    pub fn add(&mut self, scope: ScopeId, scope_type: ScopeType) -> ScopeId {
        let new = self.tree.new_node(scope_type.clone());
        scope.node.append(new, &mut self.tree);
        match scope_type {
            ScopeType::Declaration(_, id) => {
                self.by_declaration.insert(id, new);
            }
            ScopeType::File(_, id) => {
                self.by_file.insert(id, new);
            }
            _ => {}
        }
        ScopeId { node: new }
    }

    pub fn parent(&self, scope: ScopeId) -> Option<ScopeId> {
        self.tree
            .get(scope.node)
            .and_then(|node| node.parent().map(|node| ScopeId { node }))
    }

    pub fn scope_children(&self, scope_id: ScopeId) -> Option<Vec<ScopeId>> {
        let mut node_opt = self.tree.get(scope_id.node)?.first_child();
        let mut scope_ids = vec![];
        while let Some(node_id) = node_opt {
            let node = self.tree.get(node_id)?;
            scope_ids.push(ScopeId { node: node_id });
            node_opt = node.next_sibling();
        }
        Some(scope_ids)
    }

    pub fn scope_children_by_name(
        &self,
        scope_id: ScopeId,
        name_to_search: Spur,
    ) -> Option<Vec<ScopeId>> {
        let mut node_opt = self.tree.get(scope_id.node)?.first_child();
        let mut scope_ids = vec![];
        while let Some(node_id) = node_opt {
            let node = self.tree.get(node_id)?;
            let name = *match node.get() {
                ScopeType::Declaration(spur, _) => spur,
                ScopeType::File(spur, _) => spur,
                ScopeType::Package(spur) => spur,
                ScopeType::Root(_) => unreachable!(),
            };
            if name == name_to_search {
                scope_ids.push(ScopeId { node: node_id });
            }
            node_opt = node.next_sibling();
        }
        Some(scope_ids)
    }

    pub fn scope_of_file(&self, file_id: FileId) -> Option<ScopeId> {
        self.by_file.get(&file_id).map(|&node| ScopeId { node })
    }

    pub fn scope_of_declaration(&self, declaration_id: DeclarationId) -> Option<ScopeId> {
        self.by_declaration
            .get(&declaration_id)
            .map(|&node| ScopeId { node })
    }

    pub fn scope_name(&self, scope_id: ScopeId) -> Option<Spur> {
        self.tree.get(scope_id.node).map(|node| *match node.get() {
            ScopeType::Declaration(spur, _) => spur,
            ScopeType::File(spur, _) => spur,
            ScopeType::Package(spur) => spur,

            ScopeType::Root(_) => unreachable!(),
        })
    }
}
