use std::{collections::HashMap, fmt::Display};

pub(crate) struct AtomMap {
    atoms: HashMap<String, AtomId>,
}

impl AtomMap {
    pub(crate) fn new() -> Self {
        Self {
            atoms: HashMap::new(),
        }
    }

    pub(crate) fn put_atoms_in_set(&mut self, atom: &str) -> AtomId {
        let atom_string = String::from(atom);
        if let Some(the_atom) = self.atoms.get(&atom_string) {
            return *the_atom;
        } else {
            let the_atom = AtomId::new(self.atoms.len());
            self.atoms.insert(atom_string, the_atom);
            the_atom
        }
    }

    pub(crate) fn get(&self, key: &str) -> Option<&AtomId> {
        self.atoms.get(&String::from(key))
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
/// Unique identifier of an Atom in the file's vector of Atoms
pub struct AtomId(usize);

impl AtomId {
    /// Create a new AtomId
    pub(crate) fn new(id: usize) -> Self {
        AtomId(id)
    }

    /// Convert an AtomId to a usize
    pub(crate) fn as_usize(&self) -> usize {
        self.0
    }
}

impl Display for AtomId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AtomId:{}", self.0)
    }
}
