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
//! - [ ] ! map get, unions, intersection, difference
//! - [ ] ! if then else, layout blocks
//! - [ ] ! imports
//! - [ ] ! Add functions with only one argument
//! - [ ] ! Add functions with multiple arguments
//! - [ ] ! tuples
//! - [ ] ! String interpolation
//! - [ ] Implement REPL
//! - [ ] Add `where`
//! - [ ] Sets
//! - [ ] Add partial application of functions
//!     - [ ] `\` for anonymous functions
//! - [ ] Add pattern matching to function
//!     - [ ] Add `match` ... `with`
//!     - [ ] Exten union, difference, intersection operators to all functors
//!     - [ ] Add type predicate matching
//! - [ ] Add multi-method overloads
//! - [ ] `$` for parens until end of line
//! - [ ] #[attributes] and `attrib` to get a list of things in a file that have that attribute?
//! - [ ] `do` notation
//! - [ ] `!` denotes side-effects, or maybe strictness?

pub mod ast;
mod atom;
mod eval;
mod layout;
mod parser;
pub mod query;
mod scope;
mod tokenizer;

use std::{
    fs::{self},
    sync::{Arc, Mutex},
};

use ast::AstHeap;
use atom::AtomMap;
use parser::Parser;
use query::KartaQuery;
use scope::Scope;

/// Represents a file after being parsed
pub struct KartaFile {
    /// Maps atom string representations to their atom id
    atoms: Arc<Mutex<AtomMap>>,
    /// The AstHeap ID of the root AST expression for this karta file
    // root: AstId,
    root: Arc<Mutex<Scope>>,
    /// Heap of all Asts, can be accessed with an AstId
    ast_heap: Arc<Mutex<AstHeap>>,
}

impl KartaFile {
    /// Create and parse a new Karta file from file contents. Returns an error if tokenization or parsing fails.
    pub fn new(file_contents: impl ToString) -> Result<Self, String> {
        let file_contents = file_contents.to_string();

        let mut atoms = AtomMap::new();

        let mut ast_heap = AstHeap::new(&mut atoms);

        let mut parser = Parser::new();
        let root = Scope::new(None);
        parser.parse(file_contents, &mut ast_heap, &mut atoms, &root)?;

        Ok(Self {
            ast_heap: Arc::new(Mutex::new(ast_heap)),
            atoms: Arc::new(Mutex::new(atoms)),
            root,
        })
    }

    /// Create and parse a new Karta file from a file. Returns an error if reading the file, tokenization, or parsing fails.
    pub fn from_file(filename: &str) -> Result<Self, String> {
        let file_contents: String = match fs::read_to_string(filename) {
            Ok(c) => c,
            Err(x) => return Err(x.to_string()),
        };
        Self::new(file_contents)
    }

    pub fn eval(&self, expr_str: &str) -> Result<KartaQuery, String> {
        let mut parser = Parser::new();
        let result = {
            let mut ast_heap = self.ast_heap.lock().unwrap();
            let mut atoms = self.atoms.lock().unwrap();

            let expr_ast = parser.parse_expr(
                String::from(expr_str),
                &mut ast_heap,
                &mut atoms,
                &self.root,
            )?;
            ast_heap.eval(expr_ast, &self.root, &atoms)?
        };

        Ok(KartaQuery::new(self, result))
    }

    /// The Ast Heap of this Karta file
    pub(crate) fn ast_heap(&self) -> std::sync::MutexGuard<'_, AstHeap> {
        self.ast_heap.lock().unwrap() // Automatically unlocks when it goes out of scope
    }

    /// The atoms map for this Karta file
    pub(crate) fn atoms(&self) -> std::sync::MutexGuard<'_, AtomMap> {
        self.atoms.lock().unwrap() // Automatically unlocks when it goes out of scope
    }
}

mod tests {
    #[cfg(test)]
    use super::*;

    #[test]
    fn basic_variable() -> Result<(), String> {
        let karta_file = KartaFile::new("x = 100")?;

        let res: i64 = karta_file.eval("x")?.as_int()?;

        assert_eq!(res, 100);

        Ok(())
    }

    #[test]
    fn get_map_int() -> Result<(), String> {
        let karta_file = KartaFile::new("test = {.test-atom = 4}")?;

        let res: i64 = karta_file.eval("test.test-atom")?.as_int()?;

        assert_eq!(res, 4);
        Ok(())
    }

    #[test]
    fn get_map_floats() -> Result<(), String> {
        let karta_file = KartaFile::new("test = {.test-atom = 4.5}")?;

        let res: f64 = karta_file.eval("test.test-atom")?.as_float()?;

        assert_eq!(res, 4.5);
        Ok(())
    }

    #[test]
    fn get_map_string() -> Result<(), String> {
        let karta_file = KartaFile::new("test = {.test-atom = \"Hello, World!\"}")?;

        let binding = karta_file.eval("test.test-atom")?;
        let res = binding.as_string()?;

        assert_eq!(res, "Hello, World!");
        Ok(())
    }

    #[test]
    fn truthy_falsey() -> Result<(), String> {
        let karta_file = KartaFile::new("test = {.test-atom1 = .t, .test-atom2 = .nil}")?;

        let test_atom1 = karta_file.eval("test.test-atom1")?.truthy()?;
        let test_atom2 = karta_file.eval("test.test-atom2")?.truthy()?;

        assert!(test_atom1);
        assert!(!test_atom2);

        Ok(())
    }

    #[test]
    fn list_iterator() -> Result<(), String> {
        let karta_file = KartaFile::new("test = [1, 2, 3]")?;

        let mut counter: i64 = 1;
        for elem in karta_file.eval("test")? {
            assert_eq!(counter, elem.as_int::<i64>()?);
            counter += 1;
        }

        Ok(())
    }

    #[test]
    fn double_list_iterator() -> Result<(), String> {
        let karta_file = KartaFile::new("test = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]")?;

        let mut counter: i64 = 1;
        for elem in karta_file.eval("test")? {
            for elem2 in elem {
                assert_eq!(counter, elem2.as_int::<i64>()?);
                counter += 1;
            }
        }

        Ok(())
    }

    #[test]
    fn operators() -> Result<(), String> {
        let karta_file = KartaFile::new("test = 3 + 5 * 4")?;

        let res: i64 = karta_file.eval("test")?.as_int()?;

        assert_eq!(res, 23);

        Ok(())
    }

    #[test]
    fn parenthesis() -> Result<(), String> {
        let karta_file = KartaFile::new("test = (3 + 5) * 4")?;

        let res: i64 = karta_file.eval("test")?.as_int()?;

        assert_eq!(res, 32);

        Ok(())
    }

    #[test]
    fn let_in() -> Result<(), String> {
        let karta_file = KartaFile::new("test = let x = 4 in x")?;

        let res: i64 = karta_file.eval("test")?.as_int()?;

        assert_eq!(res, 4);

        Ok(())
    }

    #[test]
    fn let_in_multiple_lines() -> Result<(), String> {
        let karta_file = KartaFile::new(
            r#"test = let
  x = 4
  y = 5
in x + y
"#,
        )?;

        let res: i64 = karta_file.eval("test")?.as_int()?;

        assert_eq!(res, 9);

        Ok(())
    }
}
