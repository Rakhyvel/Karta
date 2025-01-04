//! TODO:
//! - [x] Add queries
//! - [ ] Move to own crate
//! - [ ] Split up into ast.rs, parser.rs, tokenizer.rs, error.rs, query.rs, lib.rs
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

use std::{
    collections::HashMap,
    fmt::Display,
    fs::{self},
};

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

/// A struct representing a query over a Karta file and it's intermediate result
pub struct KartaQuery<'a> {
    /// The Karta file that this query is over
    file: &'a KartaFile,
    /// The current, immediate result of this query.
    current_result: Result<AstId, String>,
}

impl<'a> KartaQuery<'a> {
    /// Create a new query with a Karta file and current result set as the file's root.
    fn new(file: &'a KartaFile) -> Self {
        Self {
            file,
            current_result: Ok(file.root),
        }
    }

    /// Return a new query with it's result being the result of applying the atom to the current result.
    /// The result becomes an error if applied to a non-map, or if the previous result was errant.
    pub fn get_atom(mut self, field: &str) -> Self {
        let current_result = match self.current_result {
            Ok(x) => x,
            Err(_) => return self,
        };

        let root_ast = self
            .file
            .ast_heap()
            .get(current_result)
            .expect("couldn't get Ast for AstId");

        let field_atom_id = match self.file.atoms().get(&String::from(field)) {
            Some(x) => x,
            None => return self,
        };

        self.current_result = match root_ast {
            Ast::Map(map) => Ok(map.get(&field_atom_id).copied().unwrap_or(AstId::new(0))),
            _ => Err(format!("cannot call `get` on {:?} type AST", root_ast)),
        };

        self
    }

    /// Interpret the current result of this query as an integer.
    /// Returns an error if the query result cannot be converted to an integer, or if any errors occured during the query process.
    pub fn as_int<T>(&self) -> Result<T, String>
    where
        T: From<i64>,
    {
        if let Ok(current_result) = self.current_result {
            let ast = self
                .file
                .ast_heap()
                .get(current_result)
                .expect("couldn't get Ast for AstId");
            match ast {
                Ast::Int(x) => Ok(T::from(*x as i64)),
                Ast::Float(x) => Ok(T::from(*x as i64)),
                Ast::Char(x) => Ok(T::from(*x as i64)),
                _ => Err(format!("cannot convert {:?} to int", ast)),
            }
        } else {
            Err(self.current_result.clone().unwrap_err())
        }
    }

    /// Interpret the current result of this query as a float.
    /// Returns an error if the query result cannot be converted to a float, or if any errors occured during the query process.
    pub fn as_float<T>(&self) -> Result<T, String>
    where
        T: From<f64>,
    {
        if let Ok(current_result) = self.current_result {
            let ast = self
                .file
                .ast_heap()
                .get(current_result)
                .expect("couldn't get Ast for AstId");
            match ast {
                Ast::Int(x) => Ok(T::from(*x as f64)),
                Ast::Float(x) => Ok(T::from(*x as f64)),
                Ast::Char(x) => Ok(T::from(*x as f64)),
                _ => Err(format!("cannot convert {:?} to float", ast)),
            }
        } else {
            Err(self.current_result.clone().unwrap_err())
        }
    }

    /// Interpret the current result of this query as a string.
    /// Returns an error if the query result cannot be converted to a string, or if any errors occured during the query process.
    pub fn as_string(&self) -> Result<&str, String> {
        if let Ok(current_result) = self.current_result {
            let ast = self
                .file
                .ast_heap()
                .get(current_result)
                .expect("couldn't get Ast for AstId");
            match ast {
                Ast::String(x) => Ok((*x).as_str()),
                _ => Err(format!("cannot convert {:?} to string", ast)),
            }
        } else {
            Err(self.current_result.clone().unwrap_err())
        }
    }

    /// Whether or not the current result of this query is truthy (ie not the .nil atom).
    /// Propagates any errors that may have occured during the query process.
    pub fn truthy(&self) -> Result<bool, String> {
        if let Ok(current_result) = self.current_result {
            let ast = self
                .file
                .ast_heap()
                .get(current_result)
                .expect("couldn't get Ast for AstId");
            match ast {
                Ast::Atom(x) => Ok(x.as_usize() != 0),
                _ => Ok(true),
            }
        } else {
            Err(self.current_result.clone().unwrap_err())
        }
    }

    /// Whether or not the current result of this query is truthy (ie the .nil atom).
    /// Propagates any errors that may have occured during the query process.
    pub fn falsey(&self) -> Result<bool, String> {
        Ok(!(self.truthy()?))
    }

    // TODO: Implement IntoIterator for lists
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

/// Contains the ASTs used in a Karta file
struct AstHeap {
    asts: Vec<Ast>,
}

impl AstHeap {
    /// Create a new Ast Heap
    fn new() -> Self {
        Self { asts: vec![] }
    }

    /// Inserts a new Ast into the heap, and returns it's ID
    fn insert(&mut self, ast: Ast) -> AstId {
        let retval = AstId::new(self.asts.len());
        self.asts.push(ast);
        retval
    }

    /// Inserts an integer Ast, and returns it's ID
    fn create_int(&mut self, value: i64) -> AstId {
        self.insert(Ast::Int(value))
    }

    /// Inserts a float Ast, and returns it's ID
    fn create_float(&mut self, value: f64) -> AstId {
        self.insert(Ast::Float(value))
    }

    /// Inserts a char Ast, and returns it's ID
    fn create_char(&mut self, value: u8) -> AstId {
        self.insert(Ast::Char(value))
    }

    /// Inserts a string Ast, and returns it's ID
    fn create_string(&mut self, value: String) -> AstId {
        self.insert(Ast::String(value))
    }

    /// Inserts an atom Ast, and returns it's ID
    fn create_atom(&mut self, value: AtomId) -> AstId {
        self.insert(Ast::Atom(value))
    }

    /// Inserts a map Ast, and returns it's ID
    fn create_map(&mut self, value: HashMap<AtomId, AstId>) -> AstId {
        self.insert(Ast::Map(value))
    }

    /// Returns the AstId of the nil atom
    fn nil_atom(&self) -> AstId {
        AstId::new(0)
    }

    /// Creates a linked-list node out of a map Ast
    fn make_list_node(&mut self, head_atom: AtomId, head: AstId, tail_atom: AtomId) -> AstId {
        let mut fields: HashMap<AtomId, AstId> = HashMap::new();
        fields.insert(head_atom, head);
        fields.insert(tail_atom, self.nil_atom());
        self.create_map(fields)
    }

    /// Retrieves a reference to an Ast for a given ID, if it exists
    fn get(&self, ast_id: AstId) -> Option<&Ast> {
        self.asts.get(ast_id.as_usize())
    }

    /// Retrieves a mutable reference to an Ast for a given ID, if it exists
    fn get_mut(&mut self, ast_id: AstId) -> Option<&mut Ast> {
        self.asts.get_mut(ast_id.as_usize())
    }
}

#[derive(Copy, Clone, Debug)]
/// Unique identifier of an Ast expression in the file's vector of Asts
pub struct AstId(usize);

impl AstId {
    /// Create a new AstId
    fn new(id: usize) -> Self {
        AstId(id)
    }

    /// Convert an AstId to a usize
    fn as_usize(&self) -> usize {
        self.0
    }
}

impl Display for AstId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AstId:{}", self.0)
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
/// Unique identifier of an Atom in the file's vector of Atoms
pub struct AtomId(usize);

impl AtomId {
    /// Create a new AtomId
    fn new(id: usize) -> Self {
        AtomId(id)
    }

    /// Convert an AtomId to a usize
    fn as_usize(&self) -> usize {
        self.0
    }
}

impl Display for AtomId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AtomId:{}", self.0)
    }
}

#[derive(Debug)]
/// Represents an expression in the Karta file
enum Ast {
    /// A basic integer
    Int(i64),
    /// A floating point number
    Float(f64),
    /// A character
    Char(u8),
    /// A string
    String(String),
    /// An atomic value
    Atom(AtomId),
    /// Maps AtomId's to an Ast within the file
    Map(HashMap<AtomId, AstId>),
}

#[derive(PartialEq, Clone, Copy, Debug)]
/// Represents the various kinds a token can be
enum TokenKind {
    LeftBrace,
    RightBrace,
    LeftSquare,
    RightSquare,
    Atom,
    Integer,
    Float,
    Char,
    String,
    Identifier,
    Comma,
    Assign,
    EndOfFile,
}

impl TokenKind {
    /// Get the token kind from a string representation
    fn from_string(str: &str) -> Self {
        assert!(str.len() > 0);
        match str {
            "{" => TokenKind::LeftBrace,
            "}" => TokenKind::RightBrace,
            "[" => TokenKind::LeftSquare,
            "]" => TokenKind::RightSquare,
            "," => TokenKind::Comma,
            "=" => TokenKind::Assign,
            _ if str.chars().nth(0).unwrap() == '.' => TokenKind::Atom,
            _ if str.chars().nth(0).unwrap().is_digit(10) => TokenKind::Integer,
            _ => TokenKind::Identifier,
        }
    }
}

#[derive(Clone, Copy, Debug)]
/// Represents a position in a text file
struct Span {
    /// Line number of the file, starts at 1
    line: usize,
    /// Column number of the file, starts at 1
    col: usize,
}

#[derive(Clone, Debug)]
/// Represents a single piece of text in the file
struct Token {
    /// Owning string representing the actual text data for this string
    data: String, // TODO: Should figure out how to just use `&'a str` here
    /// What kind of token this is
    kind: TokenKind,
    /// Where in the file this token came from
    span: Span,
}

/// Converts file contents text into a stream of tokens
struct Tokenizer {
    /// Where in the file the tokenizer is currently working
    cursor: usize,
    /// The cursor of the begining of the current token that the tokenizer is working on
    starting_cursor: usize,
    /// The actual contents of the file
    file_contents: String,
    /// The current line number for where the tokenizer is in the file
    line: usize,
    /// The current column number for where the tokenizer is in the file
    col: usize,
    /// The state of the tokenizer
    state: TokenizerState,
}

impl Tokenizer {
    /// Create a new tokenizer, taking ownership of the file contents string
    fn new(file_contents: String) -> Self {
        Self {
            cursor: 0,
            file_contents,
            line: 1,
            col: 1,
            state: TokenizerState::None,
            starting_cursor: 0,
        }
    }

    /// Convert the file contents string into a stream of tokens
    fn tokenize(&mut self, tokens: &mut Vec<Token>) -> Result<(), String> {
        while !self.eof() {
            let char = self.file_contents.chars().nth(self.cursor).unwrap(); // yeah probably slow, but it doesn't matter

            match self.state {
                // The none state branches off into various other states depending on the next character
                TokenizerState::None if char.is_whitespace() => {
                    self.advance(TokenizerState::Whitespace)
                }
                TokenizerState::None if char.is_digit(10) => self.advance(TokenizerState::Integer),
                TokenizerState::None if char == '.' => self.advance(TokenizerState::Atom),
                TokenizerState::None if char == '\'' => self.advance(TokenizerState::Char),
                TokenizerState::None if char == '"' => self.advance(TokenizerState::String),
                TokenizerState::None if char == ';' => self.advance(TokenizerState::Comment),
                TokenizerState::None => self.advance(TokenizerState::Symbol),

                // Whitespace state ends when the next char isn't whitespace
                TokenizerState::Whitespace if self.eof() || !char.is_whitespace() => {
                    self.starting_cursor = self.cursor;
                    self.state = TokenizerState::None
                }
                TokenizerState::Whitespace => {
                    if char == '\n' {
                        self.line += 1;
                        self.col = 1;
                    }
                    self.advance(TokenizerState::Whitespace);
                }

                // Integers become floats if a `.` is encountered, otherwise end when the next char isn't a digit
                TokenizerState::Integer if char == '.' => self.advance(TokenizerState::Float),
                TokenizerState::Integer if self.eof() || !char.is_digit(10) => {
                    self.add_token(TokenKind::Integer, tokens);
                }

                // Atoms end when the next char isn't a valid atom character
                TokenizerState::Atom
                    if self.eof()
                        || (!char.is_alphanumeric()
                            && char != '_'
                            && char != '-'
                            && char != '?') =>
                {
                    self.add_token(TokenKind::Atom, tokens)
                }

                // Strings end at the second single quote
                TokenizerState::Char if self.eof() => {
                    return Err(String::from("error: char goes to end of file"))
                }
                TokenizerState::Char if char == '\'' => {
                    self.advance(TokenizerState::None);
                    self.add_token(TokenKind::Char, tokens);
                }

                // Strings end at the second double quote
                TokenizerState::String if self.eof() => {
                    return Err(String::from("error: string goes to end of file"))
                }
                TokenizerState::String if char == '"' => {
                    self.advance(TokenizerState::None);
                    self.add_token(TokenKind::String, tokens);
                }

                // Symbols end at the end of the file, or if the next token isn't recognized
                TokenizerState::Symbol
                    if self.eof()
                        || TokenKind::from_string(
                            &self.file_contents[self.starting_cursor..self.cursor + 1],
                        ) == TokenKind::Identifier =>
                {
                    let token_data = &self.file_contents[self.starting_cursor..self.cursor];
                    let token_kind = TokenKind::from_string(token_data);
                    self.add_token(token_kind, tokens);
                }

                // Floats end at the end of file, or if the character is no longer a digit
                TokenizerState::Float if self.eof() || !char.is_digit(10) => {
                    self.add_token(TokenKind::Float, tokens)
                }

                // Comments end at newlines
                TokenizerState::Comment if char == '\n' => {
                    self.line += 1;
                    self.col = 1;
                    self.starting_cursor = self.cursor;
                    self.state = TokenizerState::None
                }

                // None of the above transitions passed, just keep the current state and advance the cursor
                _ => self.advance(self.state),
            }
        }

        self.add_token(TokenKind::EndOfFile, tokens);

        Ok(())
    }

    /// Whether or not the tokenizer is at the end of the file
    fn eof(&self) -> bool {
        self.file_contents.chars().nth(self.cursor).is_none()
    }

    /// Advances the cursor and column number, and changes the state to a new state
    fn advance(&mut self, new_state: TokenizerState) {
        self.cursor += 1;
        self.col += 1;
        self.state = new_state;
    }

    /// Adds the current span as a token to the list of tokens
    fn add_token(&mut self, kind: TokenKind, tokens: &mut Vec<Token>) {
        let token_data = String::from(&self.file_contents[self.starting_cursor..self.cursor]);
        let token = Token {
            data: token_data,
            kind,
            span: Span {
                col: self.col - 1,
                line: self.line,
            },
        };
        tokens.push(token);

        self.starting_cursor = self.cursor;
        self.state = TokenizerState::None;
    }
}

#[derive(Clone, Copy)]
/// States that the tokenizer can be in
enum TokenizerState {
    None,
    Whitespace,
    Integer,
    Atom,
    Char,
    String,
    Symbol,
    Float,
    Comment,
}

/// Parses a stream of tokens into Asts
struct Parser {
    cursor: usize,
    tokens: Vec<Token>,
}

impl Parser {
    /// Creates a new Parser
    fn new() -> Self {
        Self {
            cursor: 0,
            tokens: vec![],
        }
    }

    /// Parses file contents into Asts and atoms
    fn parse(
        &mut self,
        file_contents: String,
        ast_heap: &mut AstHeap,
        atoms: &mut HashMap<String, AtomId>,
    ) -> Result<AstId, String> {
        let mut tokenizer = Tokenizer::new(file_contents);
        let _ = tokenizer.tokenize(&mut self.tokens).unwrap();

        self.parse_expression(ast_heap, atoms)
    }

    /// Returns the token at the begining of the stream without removing it
    fn peek(&self) -> &Token {
        &self.tokens[self.cursor]
    }

    /// Removes and returns the token at the begining of the stream
    fn pop(&mut self) -> &Token {
        self.cursor += 1;
        &self.tokens[self.cursor - 1]
    }

    /// Returns whether or not the parser is at the end of the stream
    fn eos(&self) -> bool {
        self.cursor >= self.tokens.len()
    }

    /// Creates a parser error, with an expectation and what was actually received
    fn parse_error(&self, expected: String, got: String) -> String {
        format!(
            "error: {}:{}: expected {}, got {}",
            self.peek().span.line,
            self.peek().span.col,
            expected,
            got
        )
    }

    /// Pops the token at the begining of the stream if it's kind matches the given kind, otherwise None
    fn accept(&mut self, kind: TokenKind) -> Option<&Token> {
        if !self.eos() && self.peek().kind == kind {
            Some(self.pop())
        } else {
            None
        }
    }

    /// Pops the token at the begining of the stream if it's kind matches the given kind, otherwise Err
    fn expect(&mut self, kind: TokenKind) -> Result<&Token, String> {
        let peeked = self.peek().kind;
        let err = self.parse_error(format!("{:?}", kind), format!("{:?}", peeked));
        self.accept(kind).ok_or(err)
    }

    /// Parses an expression
    fn parse_expression(
        &mut self,
        ast_heap: &mut AstHeap,
        atoms: &mut HashMap<String, AtomId>,
    ) -> Result<AstId, String> {
        if let Some(token) = self.accept(TokenKind::Integer) {
            Ok(ast_heap.create_int(token.data.parse::<i64>().unwrap()))
        } else if let Some(token) = self.accept(TokenKind::Float) {
            Ok(ast_heap.create_float(token.data.parse::<f64>().unwrap()))
        } else if let Some(token) = self.accept(TokenKind::Char) {
            Ok(ast_heap.create_char(token.data.as_bytes()[1]))
        } else if let Some(token) = self.accept(TokenKind::String) {
            let token_len = token.data.len();
            Ok(ast_heap.create_string(String::from(&token.data[1..token_len - 1])))
        } else if let Some(token) = self.accept(TokenKind::Atom) {
            let atom_value = put_atoms_in_set(atoms, token.data.clone());
            Ok(ast_heap.create_atom(atom_value))
        } else if let Some(_token) = self.accept(TokenKind::LeftBrace) {
            let mut children: HashMap<AtomId, AstId> = HashMap::new();
            loop {
                let key_ast_id = self.parse_expression(ast_heap, atoms)?;
                let key = match *ast_heap.get(key_ast_id).unwrap() {
                    Ast::Atom(s) => s,
                    _ => return Err(String::from("bad!")),
                };
                let _ = self.expect(TokenKind::Assign)?;
                let value = self.parse_expression(ast_heap, atoms)?;

                children.insert(key, value);

                if self.accept(TokenKind::Comma).is_none() {
                    break;
                }
            }
            let _ = self.expect(TokenKind::RightBrace)?;

            Ok(ast_heap.create_map(children))
        } else if let Some(_token) = self.accept(TokenKind::LeftSquare) {
            let head_atom = put_atoms_in_set(atoms, String::from(".head"));
            let tail_atom = put_atoms_in_set(atoms, String::from(".tail"));

            if self.accept(TokenKind::RightSquare).is_some() {
                Ok(ast_heap.nil_atom())
            } else {
                let head = self.parse_expression(ast_heap, atoms)?;
                let retval = ast_heap.make_list_node(head_atom, head, tail_atom);
                let mut curr_id = retval;
                while self.accept(TokenKind::Comma).is_some() {
                    let head = self.parse_expression(ast_heap, atoms)?;
                    let new_map_id = ast_heap.make_list_node(head_atom, head, tail_atom);
                    let curr_map = if let Ast::Map(map) = ast_heap.get_mut(curr_id).unwrap() {
                        map
                    } else {
                        panic!("unreachable")
                    };
                    curr_map.insert(tail_atom, new_map_id);
                    curr_id = new_map_id;
                }
                let _ = self.expect(TokenKind::RightSquare)?;
                Ok(retval)
            }
        } else {
            Err(self.parse_error(
                String::from("an expression"),
                format!("{:?}", self.peek().kind),
            ))
        }
    }
}
