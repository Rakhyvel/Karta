use std::{collections::HashMap, fmt::Display};

use crate::{ast::AstId, atom::AtomId};

pub(crate) struct SymbolTable {
    scopes: Vec<Scope>,
}

impl SymbolTable {
    pub(crate) fn new() -> SymbolTable {
        Self { scopes: vec![] }
    }

    pub(crate) fn new_scope(&mut self, parent: Option<ScopeId>) -> ScopeId {
        let retval = ScopeId::new(self.scopes.len());
        self.scopes.push(Scope::new(parent));
        retval
    }

    pub(crate) fn get_scope(&self, scope_id: ScopeId) -> &Scope {
        &self.scopes[scope_id.as_usize()]
    }

    pub(crate) fn get_mut_scope(&mut self, scope_id: ScopeId) -> &mut Scope {
        &mut self.scopes[scope_id.as_usize()]
    }

    pub(crate) fn insert(&mut self, scope_id: ScopeId, key: AtomId, value: AstId) {
        let scope_ref = self.get_mut_scope(scope_id);
        scope_ref.insert(key, value);
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
/// Unique identifier of an Ast expression in the file's vector of Asts
pub struct ScopeId(usize);

impl ScopeId {
    /// Create a new AstId
    pub(crate) fn new(id: usize) -> Self {
        ScopeId(id)
    }

    /// Convert an AstId to a usize
    pub(crate) fn as_usize(&self) -> usize {
        self.0
    }
}

impl Display for ScopeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AstId:{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Scope {
    pub(crate) bindings: HashMap<AtomId, AstId>,
    parent: Option<ScopeId>,
}

impl Scope {
    pub(crate) fn new(parent: Option<ScopeId>) -> Scope {
        Self {
            bindings: HashMap::new(),
            parent,
        }
    }

    pub(crate) fn insert(&mut self, key: AtomId, value: AstId) {
        self.bindings.insert(key, value);
    }

    pub(crate) fn get(&self, key: AtomId) -> Option<AstId> {
        self.bindings.get(&key).copied()
    }

    pub(crate) fn parent(&self) -> Option<ScopeId> {
        self.parent
    }
}
