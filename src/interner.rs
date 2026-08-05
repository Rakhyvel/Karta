use std::any::type_name;
use std::fmt::Debug;
use std::hash::Hash;
use std::{collections::HashMap, marker::PhantomData};

/// A unique ID into an intern table
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Id<T> {
    index: u32,
    _marker: PhantomData<T>,
}

/// A bidirectional table mapping interned strings to unique IDs
#[derive(Debug)]
pub struct InternTable<T: Debug + Copy + Clone> {
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Atom {}
pub type AtomId = Id<Atom>;
pub type AtomTable = InternTable<Atom>;

impl AtomTable {
    pub const TRUE: AtomId = Id {
        index: 0,
        _marker: PhantomData,
    };
    pub const HEAD: AtomId = Id {
        index: 1,
        _marker: PhantomData,
    };
    pub const TAIL: AtomId = Id {
        index: 2,
        _marker: PhantomData,
    };

    pub fn with_wellknown() -> Self {
        let mut table = Self::new();
        let t = table.intern(".t");
        assert_eq!(t, Self::TRUE);
        let head = table.intern(".head");
        assert_eq!(head, Self::HEAD);
        let tail = table.intern(".tail");
        assert_eq!(tail, Self::TAIL);
        table
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum StringLiteral {}
pub type StringLiteralId = Id<StringLiteral>;
pub type StringLiteralTable = InternTable<StringLiteral>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Symbol {}
pub type SymbolId = Id<Symbol>;
pub type SymbolTable = InternTable<Symbol>;

/// just put the name in the bag bro
fn short_type_name<T>() -> &'static str {
    type_name::<T>().rsplit("::").next().unwrap()
}

impl<T> Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}Id({})", short_type_name::<T>(), self.index)
    }
}
