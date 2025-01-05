//! TODO:
//! - [x] Add queries
//! - [x] Move to own crate
//! - [x] Split up into ast.rs, parser.rs, tokenizer.rs, error.rs, query.rs, lib.rs
//! - [ ] Add AtomMap
//! - [ ] Add tests, maybe even unit tests!
//! - [ ] Implement IntoIterator for lists
//! - [ ] Add readme with BASIC run down on Karta
//!     - Dynamically typed, haskell-y (lazy) lisp that's also great for data description
//!     - Everything is a map, map get syntax looks like/is indistiguishable from function call
//!     - Predicate-based types
//!     - Open multimethods
//! - [ ] Implement new file syntax, and true identifier tokenization
//! - [ ] Implement REPL
//! - [ ] Add basic operators
//! - [ ] Add `let` ... `in`
//! - [ ] Add `where`
//! - [ ] Maps with fields other than strict atoms
//! - [ ] Map dereferencing
//! - [ ] Tuples
//! - [ ] Extend union operator to maps
//! - [ ] Sets
//! - [ ] Extend difference operator to maps
//! - [ ] Add functions, without much pattern matching
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
pub mod atom;
pub mod parser;
pub mod query;
pub mod tokenizer;

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
    pub fn new(file_contents: String) -> Result<Self, String> {
        let mut atoms = AtomMap::new();
        let nil_atom_id = atoms.put_atoms_in_set(".nil");
        atoms.put_atoms_in_set(".t");
        atoms.put_atoms_in_set(".head");
        atoms.put_atoms_in_set(".tail");

        let mut ast_heap = AstHeap::new();
        ast_heap.create_atom(nil_atom_id);

        let mut parser = Parser::new();
        let root = parser.parse(file_contents, &mut ast_heap, &mut atoms)?;

        Ok(Self {
            ast_heap,
            atoms,
            root,
        })
    }

    /// Create and parse a new Karta file from a file. Returns an error if reading the file, tokenization, or parsing fails.
    pub fn from_file(filename: &str) -> Result<Self, String> {
        let mut file_contents: String = match fs::read_to_string(filename) {
            Ok(c) => c,
            Err(x) => return Err(x.to_string()),
        };
        file_contents.push('\n'); // This is required to make the tokenizer happy
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
