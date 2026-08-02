use crate::{interner::SymbolId, span::Span};

/// Contains the ASTs used in a Karta context
pub(crate) struct PatternHeap {
    patterns: Vec<Pattern>,
    spans: Vec<Span>,
}

impl PatternHeap {
    /// Create a new Pattern Heap
    pub(crate) fn new() -> Self {
        Self {
            patterns: vec![],
            spans: vec![],
        }
    }

    /// Inserts a new Pattern into the heap, and returns its ID
    fn insert(&mut self, ast: Pattern, span: Span) -> PatternId {
        let retval = PatternId::new(self.patterns.len() as u32);
        self.patterns.push(ast);
        self.spans.push(span);
        assert_eq!(self.patterns.len(), self.spans.len());
        retval
    }

    pub(crate) fn create_identifier(&mut self, span: Span, identifier: SymbolId) -> PatternId {
        self.insert(Pattern::Identifier(identifier), span)
    }

    /// Retrieves a reference to an Ast for a given ID, if it exists
    pub(crate) fn get(&self, id: PatternId) -> Option<&Pattern> {
        self.patterns.get(id.as_u32() as usize)
    }

    pub(crate) fn span(&self, id: PatternId) -> Span {
        self.spans[id.as_u32() as usize]
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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
