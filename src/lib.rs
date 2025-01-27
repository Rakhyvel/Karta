//! TODO:
//! - [x] Add queries
//! - [x] Move to own crate
//! - [x] Split up into ast.rs, parser.rs, tokenizer.rs, error.rs, query.rs, lib.rs
//! - [x] Add AtomMap
//! - [x] ! Add tests, maybe even unit tests!
//! - [x] ! Implement IntoIterator for lists
//! - [x] ! Add readme with BASIC run down on Karta
//!     - Dynamically typed, haskell-y (lazy) lisp that's also great for data description
//!     - Everything is a map, map get syntax looks like/is indistiguishable from function call
//!     - Predicate-based types
//!     - Open multimethods
//! - [x] ! Implement new file syntax, and true identifier tokenization
//! - [x] ! Add basic operators
//!     - && ||
//!     - == != < <= > >=
//!     - + -
//!     - * / %
//!     - not neg
//!     - terms
//!     - ()
//! - [x] ! Add `let` ... `in`
//! - [x] ! Simplify operators to be prefix only!
//! - [x] ! map get
//! - [x] ! map keys besides atoms
//! - [x] ! tuples
//! - [x] ! Add builtin functions
//! - [x] ! Add lambdas
//! - [x] ! if then else
//! - [x] ! imports
//!     * KartaContext, which contains a string map to module ids, and a vault of the actual modules themselves
//!     * User calls `KartaContext::import(&mut self, module_name, path)` to compile and assign the context
//!     * The modules are added in the root scope of the context, and are assigned to maps of the bindings
//!     * User calls `KartaContext::eval(&mut self, expr)`, using the context's module scope
//! - [ ] ! core/prelude
//! - [x] Sets
//! - [ ] Add pattern matching to functions
//!     - [ ] Laziness
//!     - [ ] `f x y = z` => `f = \x -> \y -> z`
//!     - [ ] list, tuple, map, set destructuring
//!     - [ ] Add `match` ... `with`
//!     - [ ] Extend union, difference, intersection operators to all functors
//!     - [ ] Add type predicate matching
//! - [ ] Add multi-method overloads
//! - [ ] Implement REPL
//! - [ ] String interpolation
//! - [ ] unions, intersection, difference
//! - [ ] Add `where`
//! - [ ] `$` for parens until end of line

pub mod ast;
mod atom;
mod eval;
mod layout;
mod parser;
pub mod query;
mod scope;
mod tokenizer;

use std::{
    fs,
    sync::{Arc, Mutex},
};

use ast::AstHeap;
use atom::{AtomKind, AtomMap};
use parser::Parser;
use query::KartaQuery;
use scope::{ScopeId, SymbolTable};

/// Represents the context for evaluating Karta files and expressions
pub struct KartaContext {
    /// Maps atom string representations to their atom id
    atoms: Arc<Mutex<AtomMap>>,
    /// Heap of all Asts, can be accessed with an AstId
    ast_heap: Arc<Mutex<AstHeap>>,
    /// Symbol table for this context, which maps identifiers to their Asts for a given context
    symbol_table: Arc<Mutex<SymbolTable>>,
    /// The AstHeap ID of the root AST expression for this karta context
    root: ScopeId,
}

impl KartaContext {
    /// Creates a new Karta Context, or a string is any errors occured
    pub fn new() -> Result<Self, String> {
        let mut atoms = AtomMap::new();

        let ast_heap = AstHeap::new(&mut atoms);
        let mut symbol_table = SymbolTable::new();

        let root = symbol_table.new_scope(None);

        Ok(Self {
            ast_heap: Arc::new(Mutex::new(ast_heap)),
            atoms: Arc::new(Mutex::new(atoms)),
            root,
            symbol_table: Arc::new(Mutex::new(symbol_table)),
        })
    }

    /// Amends a module with the bindings in a file
    pub fn import_file(&mut self, module_name: String, filename: String) -> Result<(), String> {
        let file_contents: String = match fs::read_to_string(filename) {
            Ok(c) => c,
            Err(x) => return Err(x.to_string()),
        };
        self.import(module_name, file_contents)
    }

    /// Amends a module with the bindings in a string
    pub fn import(
        &mut self,
        module_name: impl ToString,
        file_contents: impl ToString,
    ) -> Result<(), String> {
        let file_contents = file_contents.to_string();

        let mut parser = Parser::new();

        let mut ast_heap = self.ast_heap.try_lock().unwrap();
        let mut atoms = self.atoms.try_lock().unwrap();
        let mut symbol_table = self.symbol_table.try_lock().unwrap();

        let file_root = symbol_table.new_scope(None);
        let file_ast = parser.parse_file(
            file_contents,
            file_root,
            &mut ast_heap,
            &mut atoms,
            &mut symbol_table,
        )?;

        let module_atom = atoms.put_atoms_in_set(AtomKind::NamedAtom(module_name.to_string()));
        symbol_table.insert(self.root, module_atom, file_ast);

        Ok(())
    }

    /// Constructs a new query from an expression, to be evaluated within the context constructed so far
    pub fn eval(&self, expr_str: &str) -> Result<KartaQuery, String> {
        let mut parser = Parser::new();
        let result = {
            let mut ast_heap = self.ast_heap.try_lock().unwrap();
            let mut atoms = self.atoms.try_lock().unwrap();
            let mut symbol_table = self.symbol_table.try_lock().unwrap();

            let expr_ast = parser.parse_expr(
                String::from(expr_str),
                self.root,
                &mut ast_heap,
                &mut atoms,
                &mut symbol_table,
            )?;
            ast_heap.eval(expr_ast, self.root, &mut atoms, &mut symbol_table)?
        };

        Ok(KartaQuery::new(self, result))
    }

    /// The Ast Heap of this Karta context
    pub(crate) fn ast_heap(&self) -> std::sync::MutexGuard<'_, AstHeap> {
        self.ast_heap.try_lock().unwrap() // Automatically unlocks when it goes out of scope
    }

    /// The atoms map for this Karta context
    pub(crate) fn atoms(&self) -> std::sync::MutexGuard<'_, AtomMap> {
        self.atoms.try_lock().unwrap() // Automatically unlocks when it goes out of scope
    }
}

mod tests {
    #[cfg(test)]
    use super::*;

    #[test]
    fn basic_variable() -> Result<(), String> {
        let karta_context = KartaContext::new()?;

        let res: i64 = karta_context.eval("let x = 100 in x")?.as_int()?;

        assert_eq!(res, 100);
        Ok(())
    }

    #[test]
    fn get_map_int() -> Result<(), String> {
        let karta_context = KartaContext::new()?;

        let res: i64 = karta_context
            .eval("let test = {.test-atom = 4} in test.test-atom")?
            .as_int()?;

        assert_eq!(res, 4);
        Ok(())
    }

    #[test]
    fn get_map_floats() -> Result<(), String> {
        let karta_context = KartaContext::new()?;

        let res: f64 = karta_context
            .eval("let test = {.test-atom = 4.5} in test.test-atom")?
            .as_float()?;

        assert_eq!(res, 4.5);
        Ok(())
    }

    #[test]
    fn get_map_string() -> Result<(), String> {
        let karta_context = KartaContext::new()?;

        let binding =
            karta_context.eval("let test = {.test-atom = \"Hello, World!\"} in test.test-atom")?;
        let res = binding.as_string()?;

        assert_eq!(res, "Hello, World!");
        Ok(())
    }

    #[test]
    fn truthy_falsey() -> Result<(), String> {
        let karta_context = KartaContext::new()?;

        let test_atom1 = karta_context.eval(".t")?.truthy()?;
        let test_atom2 = karta_context.eval(".nil")?.truthy()?;

        assert!(test_atom1);
        assert!(!test_atom2);
        Ok(())
    }

    #[test]
    fn list_iterator() -> Result<(), String> {
        let karta_context = KartaContext::new()?;

        let mut counter: i64 = 1;
        for elem in karta_context.eval("[1, 2, 3]")? {
            assert_eq!(counter, elem.as_int::<i64>()?);
            counter += 1;
        }

        Ok(())
    }

    #[test]
    fn double_list_iterator() -> Result<(), String> {
        let karta_context = KartaContext::new()?;

        let mut counter: i64 = 1;
        for elem in karta_context.eval("[[1, 2, 3], [4, 5, 6], [7, 8, 9]]")? {
            for elem2 in elem {
                assert_eq!(counter, elem2.as_int::<i64>()?);
                counter += 1;
            }
        }

        Ok(())
    }

    #[test]
    fn builtin_functions_operators() -> Result<(), String> {
        let karta_context = KartaContext::new()?;

        let res: i64 = karta_context.eval("@add (19, 4)")?.as_int()?;

        assert_eq!(res, 23);
        Ok(())
    }

    #[test]
    fn let_in_multiple_lines() -> Result<(), String> {
        let karta_context = KartaContext::new()?;

        let res: i64 = karta_context
            .eval(
                r#"let
  x = 4
  y = 5
in (@add (x, y))
"#,
            )?
            .as_int()?;

        assert_eq!(res, 9);
        Ok(())
    }

    #[test]
    fn integer_map_keys() -> Result<(), String> {
        let kctx = KartaContext::new()?;

        let res: i64 = kctx.eval("{0 = 23} 0")?.as_int()?;

        assert_eq!(res, 23);
        Ok(())
    }

    #[test]
    fn tuples() -> Result<(), String> {
        let kctx = KartaContext::new()?;

        let res: i64 = kctx.eval("(1, 2, 3, 4) 2")?.as_int()?;

        assert_eq!(res, 3);
        Ok(())
    }

    #[test]
    fn lambdas() -> Result<(), String> {
        let kctx = KartaContext::new()?;

        let res: i64 = kctx.eval("(\\x -> @add(x, 4)) 5")?.as_int()?;

        assert_eq!(res, 9);
        Ok(())
    }

    #[test]
    fn curry() -> Result<(), String> {
        let kctx = KartaContext::new()?;

        let res: i64 = kctx
            .eval("let + = \\x -> \\y -> @add (x, y) in (+ 5 4)")?
            .as_int()?;

        assert_eq!(res, 9);
        Ok(())
    }

    #[test]
    fn if_then_else() -> Result<(), String> {
        let kctx = KartaContext::new()?;

        let res: i64 = kctx.eval("let safe-div = \\x -> \\y -> if @eql(y, 0) then .inf else @div(x, y) in (safe-div 100 4)")?.as_int()?;

        assert_eq!(res, 25);
        Ok(())
    }

    #[test]
    fn set() -> Result<(), String> {
        let kctx = KartaContext::new()?;

        let res: bool = kctx.eval("{0, 1, 2, 3} 2")?.truthy()?;

        assert!(res);
        Ok(())
    }

    #[test]
    fn import() -> Result<(), String> {
        let mut kctx = KartaContext::new()?;

        kctx.import("test", "x = 100")?;
        let res: i64 = kctx.eval("test.x")?.as_int()?;

        assert_eq!(res, 100);
        Ok(())
    }
}
