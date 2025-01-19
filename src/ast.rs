use std::{
    collections::HashMap,
    fmt::Display,
    sync::{Arc, Mutex},
};

use crate::{
    atom::{AtomId, AtomKind, AtomMap},
    scope::Scope,
};

type EvalFn = for<'a, 'b, 'c> fn(
    &'a mut AstHeap,
    AstId,
    &'b Arc<Mutex<Scope>>,
    &'c AtomMap,
) -> Result<AstId, String>;

/// Contains the ASTs used in a Karta file
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
    pub(crate) fn create_char(&mut self, value: char) -> AstId {
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

    pub(crate) fn create_let(&mut self, scope: Arc<Mutex<Scope>>, expr: AstId) -> AstId {
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

    pub(crate) fn identifier_is_bif(&self, name: &String) -> bool {
        self.builtin_function_methods.contains_key(name)
    }

    pub(crate) fn println_ast_id(&self, ast_id: &AstId, atoms: &AtomMap) {
        self.print_ast_id(ast_id, atoms);
        println!("");
    }

    pub(crate) fn print_ast_id(&self, ast_id: &AstId, atoms: &AtomMap) {
        let ast = self.get(*ast_id).unwrap();
        match ast {
            Ast::Int(x) => print!("{}", x),
            Ast::Float(x) => print!("{}", x),
            Ast::Char(x) => print!("'{}'", x),
            Ast::String(x) => print!("\"{}\"", x),
            Ast::Atom(atom_id) => print!("{:?}", atom_id),

            Ast::Map(hash_map) => {
                print!("{{");
                for (k, v) in hash_map {
                    print!("{:?} = ", k);
                    self.print_ast_id(v, atoms);
                    print!(", ");
                }
                print!("}}");
            }
            Ast::Let(_scope, ast_id) => {
                print!("let {{ ... }} in ");
                self.print_ast_id(ast_id, atoms);
            }
            Ast::Identifier(atom_id) => {
                print!("Ident({:?})", atoms.string_from_atom(*atom_id).unwrap())
            }
            Ast::Apply(f_id, a_id) => {
                print!("(");
                self.print_ast_id(f_id, atoms);
                print!(" ");
                self.print_ast_id(a_id, atoms);
                print!(")");
            }
            Ast::Lambda(arg, expr) => {
                print!("(\\");
                self.print_ast_id(arg, atoms);
                print!(" -> ");
                self.print_ast_id(expr, atoms);
                print!(")");
            }
            Ast::BuiltinFunction(atom_id) => {
                print!("Ident({:?})", atoms.string_from_atom(*atom_id).unwrap())
            }
        }
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
    Char(char),
    /// An atomic value
    Atom(AtomId),
    /// A string
    String(String),
    /// A function, with it's captured arg, and it's expression
    Lambda(AstId, AstId),
    /// A builtin function
    BuiltinFunction(AtomId),
    /// Maps AtomId's to an Ast within the file
    Map(HashMap<AtomId, AstId>),

    Let(Arc<Mutex<Scope>>, AstId),
    Identifier(AtomId),

    Apply(AstId, AstId),
}
