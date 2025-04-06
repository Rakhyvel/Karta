use std::{collections::HashMap, fmt::Display, thread::scope};

use crate::{
    atom::{AtomId, AtomKind, AtomMap},
    scope::{ScopeId, SymbolTable},
};

type EvalFn = for<'a, 'b, 'c> fn(
    &'a mut AstHeap,
    AstId,
    ScopeId,
    &'b mut AtomMap,
    &'c mut SymbolTable,
) -> Result<AstId, String>;

/// Contains the ASTs used in a Karta context
pub(crate) struct AstHeap {
    asts: Vec<Ast>,

    pub(crate) nil_id: AstId,
    pub(crate) truthy_id: AstId,
    pub(crate) empty_map_id: AstId,

    pub(crate) builtin_function_methods: HashMap<String, EvalFn>,
}

impl AstHeap {
    /// Create a new Ast Heap
    pub(crate) fn new(atoms: &mut AtomMap) -> Self {
        let mut retval = Self {
            asts: vec![],
            nil_id: AstId::new(0),
            truthy_id: AstId::new(0),
            empty_map_id: AstId::new(0),
            builtin_function_methods: HashMap::from([
                (String::from("@and"), AstHeap::and as EvalFn),
                (String::from("@or"), AstHeap::or as EvalFn),
                (String::from("@not"), AstHeap::not as EvalFn),
                (String::from("@eql"), AstHeap::equal as EvalFn),
                (String::from("@neq"), AstHeap::not_equal as EvalFn),
                (String::from("@gtr"), AstHeap::greater as EvalFn),
                (String::from("@lsr"), AstHeap::lesser as EvalFn),
                (String::from("@gte"), AstHeap::greater_equal as EvalFn),
                (String::from("@lte"), AstHeap::lesser_equal as EvalFn),
                (String::from("@add"), AstHeap::add as EvalFn),
                (String::from("@sub"), AstHeap::subtract as EvalFn),
                (String::from("@mul"), AstHeap::multiply as EvalFn),
                (String::from("@div"), AstHeap::divide as EvalFn),
                (String::from("@mod"), AstHeap::modulus as EvalFn),
                (String::from("@neg"), AstHeap::negate as EvalFn),
            ]),
        };

        let nil_atom_id = atoms.put_atoms_in_set(AtomKind::NamedAtom(String::from(".nil")));
        retval.nil_id = retval.create_atom(nil_atom_id);

        let truthy_id = atoms.put_atoms_in_set(AtomKind::NamedAtom(String::from(".t")));
        retval.truthy_id = retval.create_atom(truthy_id);

        let empty_map = HashMap::new();
        retval.empty_map_id = retval.create_map(empty_map);

        retval
    }

    /// Inserts a new Ast into the heap, and returns its ID
    fn insert(&mut self, ast: Ast) -> AstId {
        let retval = AstId::new(self.asts.len());
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
    pub(crate) fn create_string(&mut self, value: String) -> AstId {
        self.insert(Ast::String(value))
    }

    /// Inserts an atom Ast, and returns its ID
    pub(crate) fn create_atom(&mut self, value: AtomId) -> AstId {
        self.insert(Ast::Atom(value))
    }

    /// Inserts a map Ast, and returns its ID
    pub(crate) fn create_map(&mut self, value: HashMap<AtomId, AstId>) -> AstId {
        self.insert(Ast::Map(value))
    }

    pub(crate) fn create_file(&mut self, scope: ScopeId) -> AstId {
        self.insert(Ast::File(scope))
    }

    pub(crate) fn create_let(&mut self, scope: ScopeId, expr: AstId) -> AstId {
        self.insert(Ast::Let(scope, expr))
    }

    pub(crate) fn create_identifier(&mut self, identifier: AtomId) -> AstId {
        self.insert(Ast::Identifier(identifier))
    }

    pub(crate) fn create_builtin_function(&mut self, identifier: AtomId) -> AstId {
        self.insert(Ast::BuiltinFunction(identifier))
    }

    pub(crate) fn create_apply(&mut self, lhs: AstId, rhs: AstId) -> AstId {
        self.insert(Ast::Apply(lhs, rhs))
    }

    pub(crate) fn create_lambda(&mut self, arg_name: String, expr: AstId) -> AstId {
        self.insert(Ast::Lambda(arg_name, expr))
    }

    pub(crate) fn create_closure(
        &mut self,
        arg_name: String,
        expr: AstId,
        scope: ScopeId,
    ) -> AstId {
        self.insert(Ast::Closure(arg_name, expr, scope))
    }

    pub(crate) fn create_if(&mut self, conds: Vec<(AstId, AstId)>, else_: AstId) -> AstId {
        self.insert(Ast::If(conds, else_))
    }

    pub(crate) fn create_panic(&mut self) -> AstId {
        self.insert(Ast::Panic())
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

    pub(crate) fn make_tuple(&mut self, terms: Vec<AstId>, atoms: &mut AtomMap) -> AstId {
        let mut children: HashMap<AtomId, AstId> = HashMap::new();
        for (i, term) in terms.iter().enumerate() {
            children.insert(
                atoms.put_atoms_in_set(AtomKind::Int(i.try_into().unwrap())),
                *term,
            );
        }
        self.create_map(children)
    }

    /// Retrieves a reference to an Ast for a given ID, if it exists
    pub(crate) fn get(&self, ast_id: AstId) -> Option<&Ast> {
        self.asts.get(ast_id.as_usize())
    }

    /// Retrieves a mutable reference to an Ast for a given ID, if it exists
    pub(crate) fn get_mut(&mut self, ast_id: AstId) -> Option<&mut Ast> {
        self.asts.get_mut(ast_id.as_usize())
    }

    pub(crate) fn identifier_is_bif(&self, name: &String) -> bool {
        self.builtin_function_methods.contains_key(name)
    }

    pub(crate) fn println_ast_id(&self, ast_id: &AstId, atoms: &AtomMap, symbols: &SymbolTable) {
        self.print_ast_id(ast_id, atoms, symbols);
        println!("");
    }

    pub(crate) fn print_ast_id(&self, ast_id: &AstId, atoms: &AtomMap, symbols: &SymbolTable) {
        let ast = self.get(*ast_id).unwrap();
        match ast {
            Ast::Int(x) => print!("{}", x),
            Ast::Float(x) => print!("{}", x),
            Ast::Char(x) => print!("'{}'", x),
            Ast::String(x) => print!("\"{}\"", x),
            Ast::Atom(atom_id) => match atoms.atom_from_id(*atom_id).unwrap() {
                AtomKind::NamedAtom(s) => print!("{}", s),
                AtomKind::Int(n) => print!("{}", n),
                AtomKind::Char(c) => print!("\'{}\'", c),
            },

            Ast::File(_) => print!("<file>"),

            Ast::Map(hash_map) => {
                print!("{{");
                for (k, v) in hash_map {
                    match atoms.atom_from_id(*k).unwrap() {
                        AtomKind::NamedAtom(s) => print!("{}", s),
                        AtomKind::Int(n) => print!("{}", n),
                        AtomKind::Char(c) => print!("\'{}\'", c),
                    }
                    print!(" = ");
                    self.print_ast_id(v, atoms, symbols);
                    print!(", ");
                }
                print!("}}");
            }
            Ast::Let(scope, ast_id) => {
                print!("let ");
                symbols.print_scope(*scope, atoms);
                println!(" in ");
                self.print_ast_id(ast_id, atoms, symbols);
            }
            Ast::Identifier(atom_id) => {
                print!("Ident({:?})", atoms.string_from_atom(*atom_id).unwrap())
            }
            Ast::Apply(f_id, a_id) => {
                print!("(");
                self.print_ast_id(f_id, atoms, symbols);
                print!(" ");
                self.print_ast_id(a_id, atoms, symbols);
                print!(")");
            }
            Ast::Lambda(arg, expr) => {
                print!("(\\{} -> ", arg);
                self.print_ast_id(expr, atoms, symbols);
                print!(")");
            }
            Ast::Closure(_arg, _expr, _scope) => {
                print!("Closure()")
            }
            Ast::BuiltinFunction(atom_id) => {
                print!("Ident({:?})", atoms.string_from_atom(*atom_id).unwrap())
            }
            Ast::If(conds, else_) => {
                for (i, (cond, expr)) in conds.iter().enumerate() {
                    if i == 0 {
                        print!("if ");
                    } else {
                        print!(" elif ");
                    }
                    self.print_ast_id(cond, atoms, symbols);
                    print!(" then ");
                    self.print_ast_id(expr, atoms, symbols);
                }
                print!(" else ");
                self.print_ast_id(else_, atoms, symbols);
            }
            Ast::Panic() => {
                print!("panic")
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
/// Unique identifier of an Ast expression in the context's vector of Asts
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
    /// A string
    String(String),
    /// A function, with the name of its arg and expression
    Lambda(String, AstId),
    /// Closure to represent an applied function
    Closure(String, AstId, ScopeId),
    /// A builtin function
    BuiltinFunction(AtomId),
    /// Maps AtomId's to an Ast within the context
    Map(HashMap<AtomId, AstId>),
    /// Just a scope for the file
    File(ScopeId),

    Let(ScopeId, AstId),
    Identifier(AtomId),

    Apply(AstId, AstId),

    If(Vec<(AstId, AstId)>, AstId),

    Panic(),
}
