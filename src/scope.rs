use std::collections::HashMap;

use crate::{
    ast::AstId,
    interner::{AtomId, AtomTable, SymbolId},
};

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

    pub(crate) fn get_mut_scope(&mut self, scope_id: ScopeId) -> &mut Scope {
        &mut self.scopes[scope_id.as_u32() as usize]
    }

    pub(crate) fn insert(&mut self, scope_id: ScopeId, key: SymbolId, def: DefId) {
        let scope_ref = self.get_mut_scope(scope_id);
        scope_ref.insert(key, def);
    }

    pub(crate) fn lookup_ident(&self, key: SymbolId, scope: ScopeId) -> Option<(DefId, ScopeId)> {
        let mut curr_scope: Option<ScopeId> = Some(scope);
        loop {
            if let Some(some_curr_scope) = curr_scope {
                let scope_ref = self.get_scope(some_curr_scope);

                if let Some(def) = scope_ref.get_def(key) {
                    return Some((*def, some_curr_scope));
                } else {
                    curr_scope = scope_ref.parent();
                }
            } else {
                return None;
            }
        }
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
    bindings: HashMap<SymbolId, DefId>,

    /// The parent Scope node
    parent: Option<ScopeId>,
}

impl Scope {
    fn new(parent: Option<ScopeId>) -> Self {
        Self {
            parent,
            bindings: HashMap::new(),
        }
    }

    fn insert(&mut self, sym: SymbolId, def: DefId) {
        self.bindings.insert(sym, def);
    }

    fn get_def(&self, sym: SymbolId) -> Option<&DefId> {
        self.bindings.get(&sym)
    }

    fn parent(&self) -> Option<ScopeId> {
        self.parent
    }
}

#[derive(Copy, Clone, Debug)]
/// Unique identifier of a definition
pub struct DefId(u32);

impl DefId {
    /// Create a new DefId
    pub(crate) fn new(id: u32) -> Self {
        DefId(id)
    }

    /// Convert an DefId to a u32
    pub(crate) fn as_u32(&self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Definition {
    arity: u32,
    kind: DefKind,
    def: AstId,
}

#[derive(Debug, Clone)]
pub enum DefKind {
    Function,
    Parameter,
}

impl Definition {
    pub fn new(arity: u32, kind: DefKind, def: AstId) -> Self {
        Self { arity, kind, def }
    }

    pub(crate) fn arity(&self) -> u32 {
        self.arity
    }

    pub(crate) fn def(&self) -> AstId {
        self.def
    }
}
