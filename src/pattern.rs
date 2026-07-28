use crate::interner::SymbolId;

/// Contains the ASTs used in a Karta context
pub(crate) struct PatternHeap {
    patterns: Vec<Pattern>,
}

impl PatternHeap {
    /// Create a new Pattern Heap
    pub(crate) fn new() -> Self {
        Self { patterns: vec![] }
    }

    /// Inserts a new Pattern into the heap, and returns its ID
    fn insert(&mut self, ast: Pattern) -> PatternId {
        let retval = PatternId::new(self.patterns.len());
        self.patterns.push(ast);
        retval
    }

    pub(crate) fn create_identifier(&mut self, identifier: SymbolId) -> PatternId {
        self.insert(Pattern::Identifier(identifier))
    }
}

#[derive(Copy, Clone, Debug)]
/// Unique identifier of an Pattern expression in the context's vector of Asts
pub struct PatternId(usize);

impl PatternId {
    /// Create a new PatternId
    pub(crate) fn new(id: usize) -> Self {
        PatternId(id)
    }

    /// Convert an PatternId to a usize
    pub(crate) fn as_usize(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Identifier(SymbolId),
    // TODO: Add more!
}
