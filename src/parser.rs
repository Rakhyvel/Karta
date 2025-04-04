use std::collections::HashMap;

use crate::{
    ast::{Ast, AstHeap, AstId},
    atom::{AtomId, AtomKind, AtomMap},
    layout,
    scope::{ScopeId, SymbolTable},
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
    ///
    /// #### Parameters
    /// - `scope`: The scope of the module, to be filled in with the bindings in the file
    ///
    /// ### Returns
    /// Returns the File Ast for this file
    pub(crate) fn parse_file(
        &mut self,
        file_contents: String,
        scope: ScopeId,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
        symbol_table: &mut SymbolTable,
    ) -> Result<AstId, String> {
        self.get_tokens(file_contents);

        while let Some(_) = self.accept(TokenKind::Newline) {}

        self.parse_bindings(scope, TokenKind::EndOfFile, ast_heap, atoms, symbol_table)?;

        Ok(ast_heap.create_file(scope))
    }

    pub(crate) fn parse_expr(
        &mut self,
        file_contents: String,
        scope: ScopeId,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
        symbol_table: &mut SymbolTable,
    ) -> Result<AstId, String> {
        self.get_tokens(file_contents);

        self.let_in_expr(scope, ast_heap, atoms, symbol_table)
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

    /// Pops the token at the begining of the stream if its kind matches the given kind, otherwise None
    fn accept(&mut self, kind: TokenKind) -> Option<&Token> {
        if !self.eos() && self.peek().kind == kind {
            Some(self.pop())
        } else {
            None
        }
    }

    /// Pops the token at the begining of the stream if its kind matches the given kind, otherwise Err
    fn expect(&mut self, kind: TokenKind) -> Result<&Token, String> {
        let peeked = self.peek().kind;
        let err = self.parse_error(format!("{:?}", kind), format!("{:?}", peeked));
        self.accept(kind).ok_or(err)
    }

    fn parse_bindings(
        &mut self,
        scope: ScopeId,
        end_kind: TokenKind,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
        symbol_table: &mut SymbolTable,
    ) -> Result<(), String> {
        while self.peek().kind != end_kind {
            // Parse the name of this binding
            let identifier = self.expect(TokenKind::Identifier)?.clone();

            // Parse patterns into a list of args before the `=`
            let mut args = vec![];
            while let None = self.accept(TokenKind::Assign) {
                args.push(self.expr(scope, ast_heap, atoms, symbol_table)?);
            }

            // Parse the RHS after the `=`
            let mut value = self.let_in_expr(scope, ast_heap, atoms, symbol_table)?;
            while let Some(_) = self.accept(TokenKind::Newline) {}

            // Modify the RHS based on the pattern
            while args.len() > 0 {
                let arg_id = args.pop().unwrap();
                let arg_ast = ast_heap.get(arg_id).unwrap();
                match arg_ast {
                    Ast::Identifier(name) => {
                        let name_str = atoms.string_from_atom(*name).unwrap();
                        value = ast_heap.create_lambda(name_str, value)
                    }
                    Ast::Int(_int_value) => {
                        let random_name = String::from("this is pretty random!");
                        value = ast_heap.create_lambda(random_name, value)
                    }
                    _ => panic!("no! {:?}", arg_ast),
                }
            }

            let key = atoms.put_atoms_in_set(AtomKind::NamedAtom(String::from(&identifier.data)));

            let scope_ref = symbol_table.get_scope(scope);
            if let Some(arity) = scope_ref.get_arity(key) {
                // We've seen this def before
                if arity != args.len() {
                    panic!("arities don't match!")
                } else if args.len() == 0 {
                    panic!("can't redfine a constant!")
                }
                // Dive into the lambda chain's expr, get the if elif chain AST
                let mut if_chain_id = scope_ref.get_def(key).unwrap();
                loop {
                    let if_chain_ast = ast_heap.get(if_chain_id).unwrap();
                    match if_chain_ast {
                        Ast::Lambda(_arg, expr) => if_chain_id = *expr,
                        Ast::If(_, _) => break,
                        _ => panic!("whoa! {:?}", if_chain_ast),
                    }
                }

                // Modify it's cond list IN PLACE to insert a new pattern match chedk before the totality panic
            } else {
                // Haven't seen this def before in this scope, insert it!
                symbol_table.insert(scope, key, args.len(), value);
            }

            // fib 0 = 0
            // ; fib does not exist. create a lambda that takes in the arity number of arguments, and has an `if-elif-else` chain:
            // ;   \arg0 -> if (== arg0 0) then 0 else (panic! "function `fib` is not total!")
            // fib 1 = 1
            // ; fib does exist. Modify the lambda expression's if statement:
            // ;   \arg0 -> ... elif (== arg0 1) then 1 ...
            // fib n = ...
            // ; fib does exist. Modify the lambda expression's if statement:
            // ;   \arg0 -> .. elif (.t) then ...

            // ; arity MUST match!
            // ; fib 0 4 = 5 ;=> ERROR!

            // ; constants (defs with arity 0) don't need lambdas
            // my-constant = 5
            // ; my-constant does not exist. arity is 0. just create a def for it
        }
        Ok(())
    }

    fn tuple_expr(
        &mut self,
        scope: ScopeId,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
        symbol_table: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let expr = self.let_in_expr(scope, ast_heap, atoms, symbol_table)?;
        if self.peek().kind == TokenKind::Comma {
            let mut i: i64 = 0;
            let mut children: HashMap<AtomId, AstId> = HashMap::new();
            children.insert(atoms.put_atoms_in_set(AtomKind::Int(i)), expr);

            while let Some(_) = self.accept(TokenKind::Comma) {
                i += 1;
                let elem = self.let_in_expr(scope, ast_heap, atoms, symbol_table)?;
                children.insert(atoms.put_atoms_in_set(AtomKind::Int(i)), elem);
            }

            Ok(ast_heap.create_map(children))
        } else {
            Ok(expr)
        }
    }

    fn let_in_expr(
        &mut self,
        scope: ScopeId,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
        symbol_table: &mut SymbolTable,
    ) -> Result<AstId, String> {
        match self.peek().kind {
            TokenKind::Let => {
                let _ = self.pop();
                let new_scope = symbol_table.new_scope(Some(scope));
                if self.peek().kind == TokenKind::Indent {
                    let _ = self.expect(TokenKind::Indent)?;
                    self.parse_bindings(
                        new_scope,
                        TokenKind::Dedent,
                        ast_heap,
                        atoms,
                        symbol_table,
                    )?;
                    let _ = self.expect(TokenKind::Dedent)?;
                    let _ = self.accept(TokenKind::Newline);
                } else {
                    self.parse_bindings(new_scope, TokenKind::In, ast_heap, atoms, symbol_table)?;
                }
                let _ = self.expect(TokenKind::In)?;
                let expr = self.lambda_expr(new_scope, ast_heap, atoms, symbol_table)?;
                Ok(ast_heap.create_let(new_scope, expr))
            }
            _ => self.lambda_expr(scope, ast_heap, atoms, symbol_table),
        }
    }

    fn lambda_expr(
        &mut self,
        scope: ScopeId,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
        symbol_table: &mut SymbolTable,
    ) -> Result<AstId, String> {
        match self.peek().kind {
            TokenKind::Backslash => {
                let _ = self.pop();
                let name = self.pop().data.clone();
                let _ = self.expect(TokenKind::Arrow)?;
                let expr = self.lambda_expr(scope, ast_heap, atoms, symbol_table)?;
                Ok(ast_heap.create_lambda(name, expr))
            }
            _ => self.apply_expr(scope, ast_heap, atoms, symbol_table),
        }
    }

    fn apply_expr(
        &mut self,
        scope: ScopeId,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
        symbol_table: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let mut expr = self.expr(scope, ast_heap, atoms, symbol_table)?;
        while self.next_is_expr() {
            let rhs = self.expr(scope, ast_heap, atoms, symbol_table)?;
            expr = ast_heap.create_apply(expr, rhs);
        }
        Ok(expr)
    }

    /// Parses an expression
    fn expr(
        &mut self,
        scope: ScopeId,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
        symbol_table: &mut SymbolTable,
    ) -> Result<AstId, String> {
        if let Some(token) = self.accept(TokenKind::Integer) {
            Ok(ast_heap.create_int(token.data.parse::<i64>().unwrap()))
        } else if let Some(token) = self.accept(TokenKind::Float) {
            Ok(ast_heap.create_float(token.data.parse::<f64>().unwrap()))
        } else if let Some(token) = self.accept(TokenKind::Char) {
            Ok(ast_heap.create_char(token.data.chars().nth(1).unwrap()))
        } else if let Some(token) = self.accept(TokenKind::String) {
            let token_len = token.data.len();
            Ok(ast_heap.create_string(String::from(&token.data[1..token_len - 1])))
        } else if let Some(token) = self.accept(TokenKind::Atom) {
            let atom_value = atoms.put_atoms_in_set(AtomKind::NamedAtom(token.data.clone()));
            Ok(ast_heap.create_atom(atom_value))
        } else if let Some(token) = self.accept(TokenKind::Identifier) {
            let atom_value = atoms.put_atoms_in_set(AtomKind::NamedAtom(token.data.clone()));
            if ast_heap.identifier_is_bif(&token.data) {
                Ok(ast_heap.create_builtin_function(atom_value))
            } else {
                Ok(ast_heap.create_identifier(atom_value))
            }
        } else if let Some(_token) = self.accept(TokenKind::If) {
            let mut conds = vec![];
            let condition = self.let_in_expr(scope, ast_heap, atoms, symbol_table)?;
            let _ = self.expect(TokenKind::Then);
            let then = self.let_in_expr(scope, ast_heap, atoms, symbol_table)?;
            conds.push((condition, then));
            while self.accept(TokenKind::Elif).is_some() {
                let condition = self.let_in_expr(scope, ast_heap, atoms, symbol_table)?;
                let _ = self.expect(TokenKind::Then);
                let then = self.let_in_expr(scope, ast_heap, atoms, symbol_table)?;
                conds.push((condition, then));
            }
            let _ = self.expect(TokenKind::Else);
            let else_ = self.let_in_expr(scope, ast_heap, atoms, symbol_table)?;
            Ok(ast_heap.create_if(conds, else_))
        } else if let Some(_token) = self.accept(TokenKind::LeftBrace) {
            let mut children: HashMap<AtomId, AstId> = HashMap::new();
            loop {
                let key_ast_id = self.let_in_expr(scope, ast_heap, atoms, symbol_table)?;
                let key = match *ast_heap.get(key_ast_id).unwrap() {
                    Ast::Atom(s) => s,
                    Ast::Int(n) => atoms.put_atoms_in_set(AtomKind::Int(n)),
                    Ast::Char(c) => atoms.put_atoms_in_set(AtomKind::Char(c)),
                    _ => return Err(String::from("cannot use this as a key")),
                };
                if let Some(_token) = self.accept(TokenKind::Assign) {
                    // Map field, parse and insert value
                    let value = self.let_in_expr(scope, ast_heap, atoms, symbol_table)?;
                    children.insert(key, value);
                } else {
                    // Set field, insert `.t` as the value
                    children.insert(key, ast_heap.truthy_id);
                }

                if self.accept(TokenKind::Comma).is_none() {
                    break;
                }
            }
            let _ = self.expect(TokenKind::RightBrace)?;

            Ok(ast_heap.create_map(children))
        } else if let Some(_token) = self.accept(TokenKind::LeftSquare) {
            let head_atom = atoms.put_atoms_in_set(AtomKind::NamedAtom(String::from(".head")));
            let tail_atom = atoms.put_atoms_in_set(AtomKind::NamedAtom(String::from(".tail")));

            if self.accept(TokenKind::RightSquare).is_some() {
                Ok(ast_heap.nil_id)
            } else {
                let head = self.let_in_expr(scope, ast_heap, atoms, symbol_table)?;
                let retval = ast_heap.make_list_node(head_atom, head, tail_atom);
                let mut curr_id = retval;
                while self.accept(TokenKind::Comma).is_some() {
                    let head = self.let_in_expr(scope, ast_heap, atoms, symbol_table)?;
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
            let retval = self.tuple_expr(scope, ast_heap, atoms, symbol_table)?;
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
