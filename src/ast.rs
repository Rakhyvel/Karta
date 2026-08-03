use std::fmt::Display;

use crate::{
    builtin::Builtin,
    interner::{AtomId, StringLiteralId, SymbolId},
    pattern::PatternId,
    span::Span,
};

/// Contains the ASTs used in a Karta context
pub(crate) struct AstHeap {
    asts: Vec<Ast>,
    spans: Vec<Span>,
}

impl AstHeap {
    /// Create a new Ast Heap
    pub(crate) fn new() -> Self {
        Self {
            asts: vec![],
            spans: vec![],
        }
    }

    /// Inserts a new Ast into the heap, and returns its ID
    fn insert(&mut self, ast: Ast, span: Span) -> AstId {
        let retval = AstId::new(self.asts.len() as u32);
        self.asts.push(ast);
        self.spans.push(span);
        assert_eq!(self.asts.len(), self.spans.len());
        retval
    }

    /// Inserts an integer Ast, and returns its ID
    pub(crate) fn create_int(&mut self, span: Span, value: i64) -> AstId {
        self.insert(Ast::Int(value), span)
    }

    /// Inserts a float Ast, and returns its ID
    pub(crate) fn create_float(&mut self, span: Span, value: f64) -> AstId {
        self.insert(Ast::Float(value), span)
    }

    /// Inserts a char Ast, and returns its ID
    pub(crate) fn create_char(&mut self, span: Span, value: char) -> AstId {
        self.insert(Ast::Char(value), span)
    }

    /// Inserts a string Ast, and returns its ID
    pub(crate) fn create_string(&mut self, span: Span, value: StringLiteralId) -> AstId {
        self.insert(Ast::String(value), span)
    }

    /// Inserts an atom Ast, and returns its ID
    pub(crate) fn create_atom(&mut self, span: Span, value: AtomId) -> AstId {
        self.insert(Ast::Atom(value), span)
    }

    pub(crate) fn create_binding(
        &mut self,
        span: Span,
        name: SymbolId,
        params: Vec<PatternId>,
        rhs: AstId,
    ) -> AstId {
        self.insert(Ast::Binding { name, params, rhs }, span)
    }

    /// Inserts a map Ast, and returns its ID
    pub(crate) fn create_map(&mut self, span: Span, fields: Vec<(AstId, AstId)>) -> AstId {
        self.insert(Ast::Map(fields), span)
    }

    pub(crate) fn make_list(&mut self, span: Span, terms: Vec<AstId>) -> AstId {
        self.insert(Ast::List(terms), span)
    }

    pub(crate) fn make_tuple(&mut self, span: Span, terms: Vec<AstId>) -> AstId {
        self.insert(Ast::Tuple(terms), span)
    }

    pub(crate) fn create_file(&mut self, span: Span, bindings: Vec<AstId>) -> AstId {
        self.insert(Ast::File(bindings), span)
    }

    pub(crate) fn create_let(&mut self, span: Span, fields: Vec<AstId>, in_expr: AstId) -> AstId {
        self.insert(Ast::Let(fields, in_expr), span)
    }

    pub(crate) fn create_identifier(&mut self, span: Span, identifier: SymbolId) -> AstId {
        self.insert(Ast::Identifier(identifier), span)
    }

    pub(crate) fn create_builtin_function(&mut self, span: Span, id: Builtin) -> AstId {
        self.insert(Ast::BuiltinFunction(id), span)
    }

    pub(crate) fn create_apply(&mut self, span: Span, lhs: AstId, rhs: AstId) -> AstId {
        self.insert(Ast::Apply(lhs, rhs), span)
    }

    pub(crate) fn create_lambda(&mut self, span: Span, arg: PatternId, body: AstId) -> AstId {
        self.insert(Ast::Lambda { arg, body }, span)
    }

    pub(crate) fn create_if(
        &mut self,
        span: Span,
        conds: Vec<(AstId, AstId)>,
        else_: AstId,
    ) -> AstId {
        self.insert(Ast::If(conds, else_), span)
    }

    /// Retrieves a reference to an Ast for a given ID, if it exists
    pub(crate) fn get(&self, ast_id: AstId) -> Option<&Ast> {
        self.asts.get(ast_id.as_u32() as usize)
    }

    pub(crate) fn span(&self, ast_id: AstId) -> Span {
        self.spans[ast_id.as_u32() as usize]
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
    /// A function, with an arg and body expression
    Lambda {
        arg: PatternId,
        body: AstId,
    },
    /// Stores all the bindings in a file
    File(Vec<AstId>),

    If(Vec<(AstId, AstId)>, AstId),
}
