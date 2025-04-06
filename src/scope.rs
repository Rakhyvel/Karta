use std::{collections::HashMap, fmt::Display};

use crate::{
    ast::AstId,
    atom::{AtomId, AtomMap},
};

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

    pub(crate) fn insert(&mut self, scope_id: ScopeId, key: AtomId, arity: usize, def: AstId) {
        let scope_ref = self.get_mut_scope(scope_id);
        scope_ref.insert(key, arity, def);
    }

    pub(crate) fn insert_all(&mut self, dest_scope_id: ScopeId, src_scope_id: ScopeId) {
        let src_bindings = self.get_scope(src_scope_id).bindings.clone();
        let dest_bindings = &mut self.get_mut_scope(dest_scope_id).bindings;
        dest_bindings.extend(src_bindings);
    }

    pub(crate) fn print_scope(&self, scope: ScopeId, atoms: &AtomMap) {
        let mut curr_scope: Option<ScopeId> = Some(scope);
        loop {
            if let Some(some_curr_scope) = curr_scope {
                let scope_ref = self.get_scope(some_curr_scope);
                for key in scope_ref.bindings.keys() {
                    println!("{:?}", atoms.string_from_atom(*key))
                }
                curr_scope = scope_ref.parent();
            } else {
                return;
            }
        }
    }

    pub(crate) fn lookup_ident(
        &self,
        key: AtomId,
        scope: ScopeId,
        atoms: &mut AtomMap,
    ) -> Result<(AstId, ScopeId), String> {
        let mut curr_scope: Option<ScopeId> = Some(scope);
        loop {
            if let Some(some_curr_scope) = curr_scope {
                let scope_ref = self.get_scope(some_curr_scope);

                if let Some(def) = scope_ref.get_def(key) {
                    return Ok((def, some_curr_scope));
                } else {
                    curr_scope = scope_ref.parent();
                }
            } else {
                panic!("hehre");
                return Err(format!(
                    "use of undefined identifier `{}`",
                    atoms.string_from_atom(key).unwrap()
                ));
            }
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
/// Unique identifier of an Ast expression in the symbol table
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
    /// Binds variable names to their definition table entry
    bindings: HashMap<AtomId, Definition>,

    /// The parent Scope node
    parent: Option<ScopeId>,
}

impl Scope {
    pub(crate) fn new(parent: Option<ScopeId>) -> Scope {
        Self {
            bindings: HashMap::new(),
            parent,
        }
    }

    pub(crate) fn insert(&mut self, key: AtomId, arity: usize, def: AstId) {
        self.bindings.insert(key, Definition::new(arity, def));
    }

    pub(crate) fn get_def(&self, key: AtomId) -> Option<AstId> {
        self.get(key).and_then(|d| Some(d.def))
    }

    pub(crate) fn parent(&self) -> Option<ScopeId> {
        self.parent
    }

    pub(crate) fn get(&self, key: AtomId) -> Option<&Definition> {
        self.bindings.get(&key)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Definition {
    arity: usize,
    def: AstId,
}

impl Definition {
    fn new(arity: usize, def: AstId) -> Self {
        Self { arity, def }
    }

    pub(crate) fn arity(&self) -> usize {
        self.arity
    }

    pub(crate) fn def(&self) -> AstId {
        self.def
    }
}
