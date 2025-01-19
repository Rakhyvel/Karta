use std::{collections::HashMap, fmt::Display};

pub(crate) struct AtomMap {
    atoms: HashMap<AtomKind, AtomId>,
}

impl AtomMap {
    pub(crate) fn new() -> Self {
        Self {
            atoms: HashMap::new(),
        }
    }

    pub(crate) fn put_atoms_in_set(&mut self, atom: AtomKind) -> AtomId {
        if let Some(the_atom) = self.atoms.get(&atom) {
            return *the_atom;
        } else {
            let the_atom = AtomId::new(self.atoms.len());
            self.atoms.insert(atom, the_atom);
            the_atom
        }
    }

    pub(crate) fn get(&self, key: AtomKind) -> Option<&AtomId> {
        self.atoms.get(&key)
    }

    pub(crate) fn string_from_atom(&self, atom_id: AtomId) -> Option<String> {
        for (k, v) in &self.atoms {
            match k {
                AtomKind::NamedAtom(name) => {
                    if (*v) == atom_id {
                        return Some(name.clone());
                    }
                }
                _ => {}
            }
        }
        None
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

#[derive(Eq, Hash, PartialEq)]
pub enum AtomKind {
    Int(i64),
    Char(char),
    NamedAtom(String),
}
