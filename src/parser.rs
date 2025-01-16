use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use crate::{
    ast::{Ast, AstHeap, AstId},
    atom::{AtomId, AtomMap},
    layout,
    scope::Scope,
    tokenizer::{Token, TokenKind, Tokenizer},
};

/// Parses a stream of tokens into Asts
pub(crate) struct Parser {
    cursor: usize,
    tokens: Vec<Token>,
}

impl Parser {
    /// Creates a new Parser
    pub(crate) fn new() -> Self {
        Self {
            cursor: 0,
            tokens: vec![],
        }
    }

    /// Parses file contents into Asts and atoms
    pub(crate) fn parse(
        &mut self,
        file_contents: String,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
        scope: &Arc<Mutex<Scope>>,
    ) -> Result<(), String> {
        self.get_tokens(file_contents);

        self.parse_bindings(TokenKind::EndOfFile, ast_heap, atoms, scope)?;

        Ok(())
    }

    pub(crate) fn parse_expr(
        &mut self,
        file_contents: String,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
        scope: &Arc<Mutex<Scope>>,
    ) -> Result<AstId, String> {
        self.get_tokens(file_contents);

        self.let_in_expr(ast_heap, atoms, scope)
    }

    fn get_tokens(&mut self, file_contents: String) {
        let mut tokenizer = Tokenizer::new(file_contents);
        let _ = tokenizer.tokenize(&mut self.tokens).unwrap();
        layout::layout(&mut self.tokens);

        for token in &self.tokens {
            println!("{:?}({}) ", token.kind, token.data)
        }
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

    fn next_is_expr(&self) -> bool {
        self.peek().kind == TokenKind::Integer
            || self.peek().kind == TokenKind::Float
            || self.peek().kind == TokenKind::Char
            || self.peek().kind == TokenKind::String
            || self.peek().kind == TokenKind::Atom
            || self.peek().kind == TokenKind::Identifier
            || self.peek().kind == TokenKind::LeftBrace
            || self.peek().kind == TokenKind::LeftParen
            || self.peek().kind == TokenKind::LeftSquare
    }

    /// Returns whether or not the parser is at the end of the stream
    fn eos(&self) -> bool {
        self.cursor >= self.tokens.len() || self.peek().kind == TokenKind::EndOfFile
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

    fn parse_bindings(
        &mut self,
        end_kind: TokenKind,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
        scope: &Arc<Mutex<Scope>>,
    ) -> Result<(), String> {
        while self.peek().kind != end_kind {
            let identifier = self.expect(TokenKind::Identifier)?.clone();
            let _ = self.expect(TokenKind::Assign)?;
            let value = self.let_in_expr(ast_heap, atoms, scope)?;

            let _ = self.accept(TokenKind::Newline);

            let key = atoms.put_atoms_in_set(&identifier.data);

            println!("{:?} = {:?}", key, value);
            let mut scope = scope.lock().unwrap();
            scope.insert(key, value);
        }
        Ok(())
    }

    fn let_in_expr(
        &mut self,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
        scope: &Arc<Mutex<Scope>>,
    ) -> Result<AstId, String> {
        match self.peek().kind {
            TokenKind::Let => {
                let _ = self.pop();
                let new_scope = Scope::new(Some(scope.clone()));
                if self.peek().kind == TokenKind::Indent {
                    let _ = self.expect(TokenKind::Indent)?;
                    self.parse_bindings(TokenKind::Dedent, ast_heap, atoms, &new_scope)?;
                    let _ = self.expect(TokenKind::Dedent)?;
                    let _ = self.accept(TokenKind::Newline);
                } else {
                    self.parse_bindings(TokenKind::In, ast_heap, atoms, &new_scope)?;
                }
                let _ = self.expect(TokenKind::In)?;
                let expr = self.apply_expr(ast_heap, atoms, scope)?;
                Ok(ast_heap.create_let(new_scope, expr))
            }
            _ => self.apply_expr(ast_heap, atoms, scope),
        }
    }

    fn apply_expr(
        &mut self,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
        scope: &Arc<Mutex<Scope>>,
    ) -> Result<AstId, String> {
        let mut expr = self.expr(ast_heap, atoms, scope)?;
        while self.next_is_expr() {
            let rhs = self.expr(ast_heap, atoms, scope)?;
            expr = ast_heap.create_apply(expr, rhs);
        }
        Ok(expr)
    }

    /// Parses an expression
    fn expr(
        &mut self,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
        scope: &Arc<Mutex<Scope>>,
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
            let atom_value = atoms.put_atoms_in_set(&token.data.as_str());
            Ok(ast_heap.create_atom(atom_value))
        } else if let Some(token) = self.accept(TokenKind::Identifier) {
            let atom_value = atoms.put_atoms_in_set(&token.data.as_str());
            Ok(ast_heap.create_identifier(atom_value))
        } else if let Some(_token) = self.accept(TokenKind::LeftBrace) {
            let mut children: HashMap<AtomId, AstId> = HashMap::new();
            loop {
                let key_ast_id = self.let_in_expr(ast_heap, atoms, scope)?;
                let key = match *ast_heap.get(key_ast_id).unwrap() {
                    Ast::Atom(s) => s,
                    _ => return Err(String::from("bad!")),
                };
                let _ = self.expect(TokenKind::Assign)?;
                let value = self.let_in_expr(ast_heap, atoms, scope)?;

                children.insert(key, value);

                if self.accept(TokenKind::Comma).is_none() {
                    break;
                }
            }
            let _ = self.expect(TokenKind::RightBrace)?;

            Ok(ast_heap.create_map(children))
        } else if let Some(_token) = self.accept(TokenKind::LeftSquare) {
            let head_atom = atoms.put_atoms_in_set(".head");
            let tail_atom = atoms.put_atoms_in_set(".tail");

            if self.accept(TokenKind::RightSquare).is_some() {
                Ok(ast_heap.nil_id)
            } else {
                let head = self.let_in_expr(ast_heap, atoms, scope)?;
                let retval = ast_heap.make_list_node(head_atom, head, tail_atom);
                let mut curr_id = retval;
                while self.accept(TokenKind::Comma).is_some() {
                    let head = self.let_in_expr(ast_heap, atoms, scope)?;
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
        } else if let Some(_token) = self.accept(TokenKind::LeftParen) {
            let retval = self.let_in_expr(ast_heap, atoms, scope)?;
            let _ = self.expect(TokenKind::RightParen)?;
            Ok(retval)
        } else {
            Err(self.parse_error(
                String::from("an expression"),
                format!("{:?}", self.peek().kind),
            ))
        }
    }
}
