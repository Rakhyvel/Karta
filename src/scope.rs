use std::collections::HashMap;

use crate::{ast::AstId, interner::SymbolId};

pub(crate) struct ScopeArena {
    scopes: Vec<Scope>,
}

impl ScopeArena {
    pub(crate) fn new() -> ScopeArena {
        Self { scopes: vec![] }
    }

    pub(crate) fn new_scope(&mut self, parent: Option<ScopeId>) -> ScopeId {
        let retval = ScopeId::new(self.scopes.len() as u32);
        self.scopes.push(Scope::new(parent));
        retval
    }

    pub(crate) fn get_scope(&self, scope_id: ScopeId) -> &Scope {
        &self.scopes[scope_id.as_u32() as usize]
    }

    pub(crate) fn insert(&mut self, scope_id: ScopeId, key: SymbolId, def: DefId) {
        self.scopes[scope_id.as_u32() as usize].insert(key, def);
    }

    pub(crate) fn lookup_ident(&self, key: SymbolId, scope: ScopeId) -> Option<DefId> {
        let mut curr_scope: Option<ScopeId> = Some(scope);

        while let Some(some_curr_scope) = curr_scope {
            let scope = self.get_scope(some_curr_scope);

            if let Some(def) = scope.get_def(key) {
                return Some(*def);
            }

            curr_scope = scope.parent();
        }

        None
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
/// Unique identifier of an Ast expression in the symbol table
pub struct ScopeId(u32);

impl ScopeId {
    /// Create a new AstId
    pub(crate) fn new(id: u32) -> Self {
        ScopeId(id)
    }

    /// Convert an AstId to a u32
    pub(crate) fn as_u32(&self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Scope {
    /// Binds variable names to their definition table entry
    defs: HashMap<SymbolId, DefId>,

    /// The parent Scope node
    parent: Option<ScopeId>,
}

impl Scope {
    fn new(parent: Option<ScopeId>) -> Self {
        Self {
            parent,
            defs: HashMap::new(),
        }
    }

    fn insert(&mut self, sym: SymbolId, def: DefId) {
        self.defs.insert(sym, def);
    }

    fn get_def(&self, sym: SymbolId) -> Option<&DefId> {
        self.defs.get(&sym)
    }

    fn parent(&self) -> Option<ScopeId> {
        self.parent
    }
}

pub struct DefArena {
    defs: Vec<Definition>,
}

impl DefArena {
    pub(crate) fn new() -> DefArena {
        Self { defs: vec![] }
    }

    pub(crate) fn create_def(&mut self, arity: u32, kind: DefKind, rhs: Option<AstId>) -> DefId {
        let retval = DefId::new(self.defs.len() as u32);
        self.defs.push(Definition::new(arity, kind, rhs));
        retval
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
/// Unique identifier of a definition
pub struct DefId(u32);

impl DefId {
    /// Create a new DefId
    pub(crate) fn new(id: u32) -> Self {
        DefId(id)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Definition {
    arity: u32,
    kind: DefKind,
    rhs: Option<AstId>,
}

#[derive(Debug, Clone)]
pub enum DefKind {
    Function,
    Parameter,
}

impl Definition {
    pub fn new(arity: u32, kind: DefKind, rhs: Option<AstId>) -> Self {
        Self { arity, kind, rhs }
    }
}
