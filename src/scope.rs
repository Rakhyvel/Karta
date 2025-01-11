use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use crate::{ast::AstId, atom::AtomId};

#[derive(Debug, Clone)]
pub(crate) struct Scope {
    bindings: HashMap<AtomId, AstId>,
    parent: Option<Arc<Mutex<Scope>>>,
}

impl Scope {
    pub(crate) fn new(parent: Option<Arc<Mutex<Scope>>>) -> Arc<Mutex<Scope>> {
        Arc::new(Mutex::new(Self {
            bindings: HashMap::new(),
            parent,
        }))
    }

    pub(crate) fn insert(&mut self, key: AtomId, value: AstId) {
        self.bindings.insert(key, value);
    }

    pub(crate) fn get(&self, key: AtomId) -> Option<AstId> {
        self.bindings.get(&key).copied()
    }

    pub(crate) fn parent(&self) -> Option<&Arc<Mutex<Scope>>> {
        self.parent.as_ref()
    }
}
