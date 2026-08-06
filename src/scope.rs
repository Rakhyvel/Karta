use std::collections::HashMap;

use crate::{ast::AstId, interner::SymbolId};

#[derive(Debug)]
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

    pub(crate) fn lookup_ident_local(&self, key: SymbolId, scope: ScopeId) -> Option<DefId> {
        let scope = self.get_scope(scope);

        if let Some(def) = scope.get_def(key) {
            return Some(*def);
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

#[derive(Debug)]
pub struct DefArena {
    defs: Vec<Definition>,
}

impl DefArena {
    pub(crate) fn new() -> DefArena {
        Self { defs: vec![] }
    }

    pub(crate) fn create_def(&mut self, arity: u32, kind: DefKind, clause: Option<AstId>) -> DefId {
        let param_defs = (0..arity)
            .map(|_| self.create_def(0, DefKind::AnonymousParameter, None))
            .collect();

        let retval = DefId::new(self.defs.len() as u32);

        self.defs
            .push(Definition::new(arity, kind, clause, param_defs));
        retval
    }

    pub(crate) fn get(&self, def: DefId) -> &Definition {
        &self.defs[def.0 as usize]
    }

    pub(crate) fn get_mut(&mut self, def: DefId) -> &mut Definition {
        &mut self.defs[def.0 as usize]
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
    /// vec of the bindings if this def is from a binding, empty is from a parameter
    clauses: Vec<AstId>,
    /// synthetic defs for the params, so the captures resolve normally
    param_defs: Vec<DefId>,
}

#[derive(Debug, Clone)]
pub enum DefKind {
    Function,
    Parameter,
    AnonymousParameter,
}

impl Definition {
    pub fn new(arity: u32, kind: DefKind, clause: Option<AstId>, param_defs: Vec<DefId>) -> Self {
        Self {
            arity,
            kind,
            clauses: clause.into_iter().collect(),
            param_defs,
        }
    }

    pub fn arity(&self) -> u32 {
        self.arity
    }

    pub fn kind(&self) -> &DefKind {
        &self.kind
    }

    pub fn clauses(&self) -> &[AstId] {
        &self.clauses
    }

    pub fn push_clause(&mut self, id: AstId) {
        self.clauses.push(id);
    }

    pub fn param_defs(&self) -> &[DefId] {
        &self.param_defs
    }
}
