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
//! - [ ] ! Add `let` ... `in`
//! - [ ] ! Add functions, without much pattern matching
//! - [ ] Implement REPL
//! - [ ] Add `where`
//! - [ ] Maps with fields other than strict atoms
//! - [ ] Map dereferencing
//! - [ ] Tuples
//! - [ ] Extend union operator to maps
//! - [ ] Sets
//! - [ ] Extend difference operator to maps
//! - [ ] Add partial application of functions
//! - [ ] Add pattern matching to function
//!     - [ ] Add `match` ... `with`
//!     - [ ] Exten union, difference, intersection operators to all functors
//!     - [ ] Add type predicate matching
//! - [ ] Add imports
//! - [ ] Add multi-method overloads
//! - [ ] String interpolation
//! - [ ] `$` for parens until end of line

pub mod ast;
mod atom;
mod eval;
mod parser;
pub mod query;
mod tokenizer;

use std::fs::{self};

use ast::{AstHeap, AstId};
use atom::AtomMap;
use parser::Parser;
use query::KartaQuery;

/// Represents a file after being parsed
pub struct KartaFile {
    /// Maps atom string representations to their atom id
    atoms: AtomMap,
    /// The AstHeap ID of the root AST expression for this karta file
    root: AstId,
    /// Heap of all Asts, can be accessed with an AstId
    ast_heap: AstHeap,
}

impl KartaFile {
    /// Create and parse a new Karta file from file contents. Returns an error if tokenization or parsing fails.
    pub fn new(file_contents: impl ToString) -> Result<Self, String> {
        let mut file_contents = file_contents.to_string();
        file_contents.push('\n'); // This is required to make the tokenizer happy

        let mut atoms = AtomMap::new();

        let mut ast_heap = AstHeap::new(&mut atoms);

        let mut parser = Parser::new();
        let root = parser.parse(file_contents, &mut ast_heap, &mut atoms)?;

        let evaluated_root = ast_heap.eval(root, &mut atoms)?;

        Ok(Self {
            ast_heap,
            atoms,
            root: evaluated_root,
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

    /// Begin a query of a this Karta file, starting at its root
    pub fn query(&self) -> KartaQuery {
        KartaQuery::new(self)
    }

    /// The Ast Heap of this Karta file
    fn ast_heap(&self) -> &AstHeap {
        &self.ast_heap
    }

    /// The atoms map for this Karta file
    fn atoms(&self) -> &AtomMap {
        &self.atoms
    }
}

mod tests {
    #[cfg(test)]
    use super::*;

    #[test]
    fn basic_variable() -> Result<(), String> {
        let karta_file = KartaFile::new("x = 100")?;

        let res: i64 = karta_file.query().get("x").as_int()?;

        assert_eq!(res, 100);

        Ok(())
    }

    #[test]
    fn get_map_int() -> Result<(), String> {
        let karta_file = KartaFile::new("test = {.test-atom = 4}")?;

        let res: i64 = karta_file.query().get("test").get(".test-atom").as_int()?;

        assert_eq!(res, 4);
        Ok(())
    }

    #[test]
    fn get_map_floats() -> Result<(), String> {
        let karta_file = KartaFile::new("test = {.test-atom = 4.5}")?;

        let res: f64 = karta_file
            .query()
            .get("test")
            .get(".test-atom")
            .as_float()?;

        assert_eq!(res, 4.5);
        Ok(())
    }

    #[test]
    fn get_map_string() -> Result<(), String> {
        let karta_file = KartaFile::new("test = {.test-atom = \"Hello, World!\"}")?;

        let binding = karta_file.query().get("test").get(".test-atom");
        let res: &str = binding.as_string()?;

        assert_eq!(res, "Hello, World!");
        Ok(())
    }

    #[test]
    fn truthy_falsey() -> Result<(), String> {
        let karta_file = KartaFile::new("test = {.test-atom1 = .t, .test-atom2 = .nil}")?;

        let test_atom1 = karta_file.query().get("test").get(".test-atom1").truthy()?;
        let test_atom2 = karta_file.query().get("test").get(".test-atom2").truthy()?;

        assert!(test_atom1);
        assert!(!test_atom2);

        Ok(())
    }

    #[test]
    fn list_iterator() -> Result<(), String> {
        let karta_file = KartaFile::new("test = [1, 2, 3]")?;

        let mut counter: i64 = 1;
        for elem in karta_file.query().get("test") {
            assert_eq!(counter, elem.as_int::<i64>()?);
            counter += 1;
        }

        Ok(())
    }

    #[test]
    fn double_list_iterator() -> Result<(), String> {
        let karta_file = KartaFile::new("test = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]")?;

        let mut counter: i64 = 1;
        for elem in karta_file.query().get("test") {
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

        let res: i64 = karta_file.query().get("test").as_int()?;

        assert_eq!(res, 23);

        Ok(())
    }

    #[test]
    fn parenthesis() -> Result<(), String> {
        let karta_file = KartaFile::new("test = (3 + 5) * 4")?;

        let res: i64 = karta_file.query().get("test").as_int()?;

        assert_eq!(res, 32);

        Ok(())
    }
}
