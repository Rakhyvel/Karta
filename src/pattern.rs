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
        let retval = PatternId::new(self.patterns.len() as u32);
        self.patterns.push(ast);
        retval
    }

    pub(crate) fn create_identifier(&mut self, identifier: SymbolId) -> PatternId {
        self.insert(Pattern::Identifier(identifier))
    }

    /// Retrieves a reference to an Ast for a given ID, if it exists
    pub(crate) fn get(&self, id: PatternId) -> Option<&Pattern> {
        self.patterns.get(id.as_u32() as usize)
    }
}

#[derive(Copy, Clone, Debug)]
/// Unique identifier of an Pattern expression in the context's vector of Asts
pub struct PatternId(u32);

impl PatternId {
    /// Create a new PatternId
    pub(crate) fn new(id: u32) -> Self {
        PatternId(id)
    }

    /// Convert an PatternId to a u32
    pub(crate) fn as_u32(&self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Identifier(SymbolId),
    // TODO: Add more!
}
