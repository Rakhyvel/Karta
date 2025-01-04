use std::fmt::Display;

// TODO: Add AtomMap which maps Strings to AtomIds

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
