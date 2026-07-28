use std::fmt::Debug;
use std::{collections::HashMap, marker::PhantomData};

#[derive(Debug, Copy, Clone)]
struct Id<T: Debug + Copy + Clone> {
    index: u32,
    _marker: PhantomData<T>,
}

struct InternTable<T: Debug + Copy + Clone> {
    values: Vec<String>,
    lookup: HashMap<String, Id<T>>,
}

impl<T: Debug + Copy + Clone> InternTable<T> {
    pub fn new() -> Self {
        Self {
            values: Vec::new(),
            lookup: HashMap::new(),
        }
    }

    pub fn intern(&mut self, s: &str) -> Id<T> {
        if let Some(id) = self.lookup.get(s) {
            return *id;
        }

        let id = Id::<T> {
            index: self.values.len() as u32,
            _marker: PhantomData,
        };

        self.values.push(s.to_owned());
        self.lookup.insert(s.to_owned(), id);

        id
    }

    pub fn get(&self, id: Id<T>) -> &str {
        &self.values[id.index as usize]
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Atom {}
pub type AtomId = Id<Atom>;
pub type AtomTable = InternTable<Atom>;

#[derive(Debug, Copy, Clone)]
pub enum StringLiteral {}
pub type StringLiteralId = Id<StringLiteral>;
pub type StringLiteralTable = InternTable<StringLiteral>;

#[derive(Debug, Copy, Clone)]
pub enum Symbol {}
pub type SymbolId = Id<Symbol>;
pub type SymbolTable = InternTable<Symbol>;
