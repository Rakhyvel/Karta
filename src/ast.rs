use std::{collections::HashMap, fmt::Display};

use crate::atom::{AtomId, AtomMap};

/// Contains the ASTs used in a Karta file
pub(crate) struct AstHeap {
    asts: Vec<Ast>,

    pub(crate) nil_id: AstId,
    pub(crate) truthy_id: AstId,
    pub(crate) empty_map_id: AstId,
}

impl AstHeap {
    /// Create a new Ast Heap
    pub(crate) fn new(atoms: &mut AtomMap) -> Self {
        let mut retval = Self {
            asts: vec![],
            nil_id: AstId::new(0),
            truthy_id: AstId::new(0),
            empty_map_id: AstId::new(0),
        };

        let nil_atom_id = atoms.put_atoms_in_set(".nil");
        retval.nil_id = retval.create_atom(nil_atom_id);

        let truthy_id = atoms.put_atoms_in_set(".t");
        retval.nil_id = retval.create_atom(truthy_id);

        let empty_map = HashMap::new();
        retval.empty_map_id = retval.create_map(empty_map);

        retval
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

    pub(crate) fn create_and(&mut self, lhs: AstId, rhs: AstId) -> AstId {
        self.insert(Ast::And(lhs, rhs))
    }

    pub(crate) fn create_or(&mut self, lhs: AstId, rhs: AstId) -> AstId {
        self.insert(Ast::Or(lhs, rhs))
    }

    pub(crate) fn create_equals(&mut self, lhs: AstId, rhs: AstId) -> AstId {
        self.insert(Ast::Equals(lhs, rhs))
    }

    pub(crate) fn create_not_equals(&mut self, lhs: AstId, rhs: AstId) -> AstId {
        self.insert(Ast::NotEquals(lhs, rhs))
    }

    pub(crate) fn create_greater(&mut self, lhs: AstId, rhs: AstId) -> AstId {
        self.insert(Ast::Greater(lhs, rhs))
    }

    pub(crate) fn create_lesser(&mut self, lhs: AstId, rhs: AstId) -> AstId {
        self.insert(Ast::Lesser(lhs, rhs))
    }

    pub(crate) fn create_greater_equal(&mut self, lhs: AstId, rhs: AstId) -> AstId {
        self.insert(Ast::GreaterEqual(lhs, rhs))
    }

    pub(crate) fn create_lesser_equal(&mut self, lhs: AstId, rhs: AstId) -> AstId {
        self.insert(Ast::LesserEqual(lhs, rhs))
    }

    pub(crate) fn create_add(&mut self, lhs: AstId, rhs: AstId) -> AstId {
        self.insert(Ast::Add(lhs, rhs))
    }

    pub(crate) fn create_subtract(&mut self, lhs: AstId, rhs: AstId) -> AstId {
        self.insert(Ast::Subtract(lhs, rhs))
    }

    pub(crate) fn create_multiply(&mut self, lhs: AstId, rhs: AstId) -> AstId {
        self.insert(Ast::Multiply(lhs, rhs))
    }

    pub(crate) fn create_divide(&mut self, lhs: AstId, rhs: AstId) -> AstId {
        self.insert(Ast::Divide(lhs, rhs))
    }

    pub(crate) fn create_modulus(&mut self, lhs: AstId, rhs: AstId) -> AstId {
        self.insert(Ast::Modulus(lhs, rhs))
    }

    pub(crate) fn create_neg(&mut self, expr: AstId) -> AstId {
        self.insert(Ast::Negate(expr))
    }

    pub(crate) fn create_not(&mut self, expr: AstId) -> AstId {
        self.insert(Ast::Not(expr))
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
        fields.insert(tail_atom, self.empty_map_id);
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

#[derive(Debug, Clone)]
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

    And(AstId, AstId),
    Or(AstId, AstId),

    Equals(AstId, AstId),
    NotEquals(AstId, AstId),
    Greater(AstId, AstId),
    Lesser(AstId, AstId),
    GreaterEqual(AstId, AstId),
    LesserEqual(AstId, AstId),

    Add(AstId, AstId),
    Subtract(AstId, AstId),

    Multiply(AstId, AstId),
    Divide(AstId, AstId),
    Modulus(AstId, AstId),

    Not(AstId),
    Negate(AstId),
}
