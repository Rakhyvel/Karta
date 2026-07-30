use std::fmt::Display;

use crate::{
    builtin::Builtin,
    interner::{AtomId, StringLiteralId, SymbolId},
    pattern::PatternId,
};

/// Contains the ASTs used in a Karta context
pub(crate) struct AstHeap {
    asts: Vec<Ast>,
}

impl AstHeap {
    /// Create a new Ast Heap
    pub(crate) fn new() -> Self {
        Self { asts: vec![] }
    }

    /// Inserts a new Ast into the heap, and returns its ID
    fn insert(&mut self, ast: Ast) -> AstId {
        let retval = AstId::new(self.asts.len() as u32);
        self.asts.push(ast);
        retval
    }

    /// Inserts an integer Ast, and returns its ID
    pub(crate) fn create_int(&mut self, value: i64) -> AstId {
        self.insert(Ast::Int(value))
    }

    /// Inserts a float Ast, and returns its ID
    pub(crate) fn create_float(&mut self, value: f64) -> AstId {
        self.insert(Ast::Float(value))
    }

    /// Inserts a char Ast, and returns its ID
    pub(crate) fn create_char(&mut self, value: char) -> AstId {
        self.insert(Ast::Char(value))
    }

    /// Inserts a string Ast, and returns its ID
    pub(crate) fn create_string(&mut self, value: StringLiteralId) -> AstId {
        self.insert(Ast::String(value))
    }

    /// Inserts an atom Ast, and returns its ID
    pub(crate) fn create_atom(&mut self, value: AtomId) -> AstId {
        self.insert(Ast::Atom(value))
    }

    pub(crate) fn create_binding(
        &mut self,
        name: SymbolId,
        params: Vec<PatternId>,
        rhs: AstId,
    ) -> AstId {
        self.insert(Ast::Binding { name, params, rhs })
    }

    /// Inserts a map Ast, and returns its ID
    pub(crate) fn create_map(&mut self, fields: Vec<(AstId, AstId)>) -> AstId {
        self.insert(Ast::Map(fields))
    }

    pub(crate) fn make_list(&mut self, terms: Vec<AstId>) -> AstId {
        self.insert(Ast::List(terms))
    }

    pub(crate) fn make_tuple(&mut self, terms: Vec<AstId>) -> AstId {
        self.insert(Ast::Tuple(terms))
    }

    pub(crate) fn create_file(&mut self) -> AstId {
        self.insert(Ast::File())
    }

    pub(crate) fn create_let(&mut self, fields: Vec<AstId>, in_expr: AstId) -> AstId {
        self.insert(Ast::Let(fields, in_expr))
    }

    pub(crate) fn create_identifier(&mut self, identifier: SymbolId) -> AstId {
        self.insert(Ast::Identifier(identifier))
    }

    pub(crate) fn create_builtin_function(&mut self, id: Builtin) -> AstId {
        self.insert(Ast::BuiltinFunction(id))
    }

    pub(crate) fn create_apply(&mut self, lhs: AstId, rhs: AstId) -> AstId {
        self.insert(Ast::Apply(lhs, rhs))
    }

    pub(crate) fn create_lambda(&mut self, arg_name: PatternId, expr: AstId) -> AstId {
        self.insert(Ast::Lambda(arg_name, expr))
    }

    pub(crate) fn create_if(&mut self, conds: Vec<(AstId, AstId)>, else_: AstId) -> AstId {
        self.insert(Ast::If(conds, else_))
    }

    pub(crate) fn create_panic(&mut self) -> AstId {
        self.insert(Ast::Panic())
    }

    /// Retrieves a reference to an Ast for a given ID, if it exists
    pub(crate) fn get(&self, ast_id: AstId) -> Option<&Ast> {
        self.asts.get(ast_id.as_u32() as usize)
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
/// Unique identifier of an Ast expression in the context's vector of Asts
pub struct AstId(u32);

impl AstId {
    /// Create a new AstId
    pub(crate) fn new(id: u32) -> Self {
        AstId(id)
    }

    /// Convert an AstId to a u32
    pub(crate) fn as_u32(&self) -> u32 {
        self.0
    }
}

impl Display for AstId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AstId:{}", self.0)
    }
}

#[derive(Debug, Clone)]
/// Represents an expression in the Karta program
pub(crate) enum Ast {
    /// A basic integer
    Int(i64),
    /// A floating point number
    Float(f64),
    /// A character
    Char(char),
    /// An atomic value
    Atom(AtomId),
    /// A builtin function
    BuiltinFunction(Builtin),
    /// A string
    String(StringLiteralId),
    /// Maps AtomId's to an Ast within the context
    Map(Vec<(AstId, AstId)>),
    Tuple(Vec<AstId>),
    List(Vec<AstId>),
    Identifier(SymbolId),
    Apply(AstId, AstId),
    Let(Vec<AstId>, AstId),
    /// A binding
    Binding {
        name: SymbolId,
        params: Vec<PatternId>,
        rhs: AstId,
    },
    /// A function, with the name of its arg and expression
    Lambda(PatternId, AstId),
    /// Closure to represent an applied function
    /// Just a scope for the file
    File(),

    If(Vec<(AstId, AstId)>, AstId),

    Panic(),
}
