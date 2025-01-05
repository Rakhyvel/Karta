use std::{collections::HashMap, fmt::Display};

use crate::atom::AtomId;

/// Contains the ASTs used in a Karta file
pub(crate) struct AstHeap {
    asts: Vec<Ast>,
}

impl AstHeap {
    /// Create a new Ast Heap
    pub(crate) fn new() -> Self {
        Self { asts: vec![] }
    }

    /// Inserts a new Ast into the heap, and returns it's ID
    fn insert(&mut self, ast: Ast) -> AstId {
        let retval = AstId::new(self.asts.len());
        self.asts.push(ast);
        retval
    }

    /// Inserts an integer Ast, and returns it's ID
    pub(crate) fn create_int(&mut self, value: i64) -> AstId {
        self.insert(Ast::Int(value))
    }

    /// Inserts a float Ast, and returns it's ID
    pub(crate) fn create_float(&mut self, value: f64) -> AstId {
        self.insert(Ast::Float(value))
    }

    /// Inserts a char Ast, and returns it's ID
    pub(crate) fn create_char(&mut self, value: u8) -> AstId {
        self.insert(Ast::Char(value))
    }

    /// Inserts a string Ast, and returns it's ID
    pub(crate) fn create_string(&mut self, value: String) -> AstId {
        self.insert(Ast::String(value))
    }

    /// Inserts an atom Ast, and returns it's ID
    pub(crate) fn create_atom(&mut self, value: AtomId) -> AstId {
        self.insert(Ast::Atom(value))
    }

    /// Inserts a map Ast, and returns it's ID
    pub(crate) fn create_map(&mut self, value: HashMap<AtomId, AstId>) -> AstId {
        self.insert(Ast::Map(value))
    }

    /// Returns the AstId of the nil atom
    pub(crate) fn nil_atom(&self) -> AstId {
        AstId::new(0)
    }

    /// Creates a linked-list node out of a map Ast
    pub(crate) fn make_list_node(
        &mut self,
        head_atom: AtomId,
        head: AstId,
        tail_atom: AtomId,
    ) -> AstId {
        let mut fields: HashMap<AtomId, AstId> = HashMap::new();
        fields.insert(head_atom, head);
        fields.insert(tail_atom, self.nil_atom());
        self.create_map(fields)
    }

    /// Retrieves a reference to an Ast for a given ID, if it exists
    pub(crate) fn get(&self, ast_id: AstId) -> Option<&Ast> {
        self.asts.get(ast_id.as_usize())
    }

    /// Retrieves a mutable reference to an Ast for a given ID, if it exists
    pub(crate) fn get_mut(&mut self, ast_id: AstId) -> Option<&mut Ast> {
        self.asts.get_mut(ast_id.as_usize())
    }
}

#[derive(Copy, Clone, Debug)]
/// Unique identifier of an Ast expression in the file's vector of Asts
pub struct AstId(usize);

impl AstId {
    /// Create a new AstId
    pub(crate) fn new(id: usize) -> Self {
        AstId(id)
    }

    /// Convert an AstId to a usize
    pub(crate) fn as_usize(&self) -> usize {
        self.0
    }
}

impl Display for AstId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AstId:{}", self.0)
    }
}

#[derive(Debug)]
/// Represents an expression in the Karta file
pub(crate) enum Ast {
    /// A basic integer
    Int(i64),
    /// A floating point number
    Float(f64),
    /// A character
    Char(u8),
    /// A string
    String(String),
    /// An atomic value
    Atom(AtomId),
    /// Maps AtomId's to an Ast within the file
    Map(HashMap<AtomId, AstId>),
}
