//! TODO:
//! - [x] Add queries
//! - [x] Move to own crate
//! - [x] Split up into ast.rs, parser.rs, tokenizer.rs, error.rs, query.rs, lib.rs
//! - [ ] Add AtomMap
//! - [ ] Add tests, maybe even unit tests!
//! - [ ] Implement IntoIterator for lists
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

use std::{
    collections::HashMap,
    fs::{self},
};

use ast::{AstHeap, AstId};
use atom::AtomId;
use parser::Parser;
use query::KartaQuery;

/// Represents a file after being parsed
pub struct KartaFile {
    /// Maps atom string representations to their atom id
    atoms: HashMap<String, AtomId>,
    /// The AstHeap ID of the root AST expression for this karta file
    root: AstId,
    /// Heap of all Asts, can be accessed with an AstId
    ast_heap: AstHeap,
}

impl KartaFile {
    /// Create and parse a new Karta file from file contents. Returns an error if tokenization or parsing fails.
    pub fn new(file_contents: String) -> Result<Self, String> {
        let mut atoms = HashMap::new();
        let nil_atom_id = put_atoms_in_set(&mut atoms, String::from(".nil"));
        put_atoms_in_set(&mut atoms, String::from(".t"));
        put_atoms_in_set(&mut atoms, String::from(".head"));
        put_atoms_in_set(&mut atoms, String::from(".tail"));

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
    fn atoms(&self) -> &HashMap<String, AtomId> {
        &self.atoms
    }
}

/// Puts and returns the ID of an atom
fn put_atoms_in_set(atoms: &mut HashMap<String, AtomId>, atom: String) -> AtomId {
    if let Some(the_atom) = atoms.get(&atom) {
        return *the_atom;
    } else {
        let the_atom = AtomId::new(atoms.len());
        atoms.insert(atom, the_atom);
        the_atom
    }
}
