use std::{collections::HashMap, ops::RemAssign};

use crate::{
    ast::{Ast, AstHeap, AstId},
    atom::{self, AtomId, AtomKind, AtomMap},
    layout,
    scope::{self, ScopeId, SymbolTable},
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
    }

    /// Returns the token at the begining of the stream without removing it
    fn peek(&self) -> &Token {
        &self.tokens[self.cursor]
    }

    /// Removes and returns the token at the begining of the stream
    fn pop(&mut self) -> &Token {
        self.cursor += 1;
        let retval = &self.tokens[self.cursor - 1];
        retval
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

    // fib 0 = 0
    // ; fib does not exist. create a lambda that takes in the arity number of arguments, and has an `if-elif-else` chain:
    // ;   \arg0 -> if (== arg0 0) then 0 else (panic! "function `fib` is not total!")
    // fib 1 = 1
    // ; fib does exist. Modify the lambda expression's if statement:
    // ;   \arg0 -> ... elif (== arg0 1) then 1 ...
    // fib n = ...
    // ; fib does exist. Modify the lambda expression's if statement:
    // ;   \arg0 -> .. elif (.t) then ...
    //
    // 1. check to see if there is a definition
    //   * if there is with a mismatch arity, that's an error
    //   * if there is with an arity of 0, that's an error
    //   * if there is no definition yet, construct the lambda chain and an empty if-else list
    // 2. (there is a definition here now) construct the pattern match condition, and insert it

    // ; arity MUST match!
    // ; fib 0 4 = 5 ;=> ERROR!

    // ; constants (defs with arity 0) don't need lambdas
    // my-constant = 5
    // ; my-constant does not exist. arity is 0. just create a def for it
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
            let args: Vec<AstId> = self.parse_binding_head(scope, ast_heap, atoms, symbol_table);

            // Parse the RHS after the `=`
            let rhs_value = self.let_in_expr(scope, ast_heap, atoms, symbol_table)?;
            while let Some(_) = self.accept(TokenKind::Newline) {}

            let if_chain_id = self.resolve_or_create_function_symbol(
                &identifier,
                scope,
                args.len(),
                ast_heap,
                atoms,
                symbol_table,
            )?;

            let cond = self.build_pattern_conditions(&args, ast_heap, atoms)?;
            let rewritten_rhs =
                self.rewrite_rhs_with_args(&args, rhs_value, scope, ast_heap, atoms, symbol_table)?;

            self.insert_into_if_chain(if_chain_id, cond, rewritten_rhs, ast_heap);
        }
        Ok(())
    }

    fn parse_binding_head(
        &mut self,
        scope: ScopeId,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
        symbol_table: &mut SymbolTable,
    ) -> Vec<AstId> {
        std::iter::from_fn(|| {
            if self.accept(TokenKind::Assign).is_none() {
                Some(self.expr(scope, ast_heap, atoms, symbol_table).ok()?)
            } else {
                None
            }
        })
        .collect()
    }

    fn resolve_or_create_function_symbol(
        &mut self,
        identifier: &Token,
        scope: ScopeId,
        args_len: usize,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
        symbol_table: &mut SymbolTable,
    ) -> Result<AstId, String> {
        // Find the symbol
        let scope_ref = symbol_table.get_scope(scope);
        let key = atoms.put_atoms_in_set(AtomKind::NamedAtom(String::from(&identifier.data)));
        let if_chain_id: AstId;
        if let Some(symbol) = scope_ref.get(key) {
            // symbol definition exists
            let arity = symbol.arity();
            // check that arity matches and isn't 0
            if arity != args_len {
                panic!("arities don't match!")
            } else if args_len == 0 {
                panic!("can't redfine a constant!")
            }
            // Dive into the lambda chain's expr, get the if elif chain AST
            let mut symbol_def_id = symbol.def();
            loop {
                let symbol_def_ast = ast_heap.get(symbol_def_id).unwrap();
                match symbol_def_ast {
                    Ast::Lambda(_arg, expr) => symbol_def_id = *expr,
                    Ast::If(_, _) => break,
                    _ => panic!("whoa! {:?}", symbol_def_ast),
                }
            }
            if_chain_id = symbol_def_id;
        } else {
            // symbol did not exist!
            // create the lambda chain based on the args, starting with the empty if-else list
            let panic_ast_id = ast_heap.create_panic();
            if_chain_id = ast_heap.create_if(vec![], panic_ast_id);
            let mut def = if_chain_id;
            for i in 0..args_len {
                let arg_name = format!("$arg{}", i);
                def = ast_heap.create_lambda(arg_name, def)
            }
            symbol_table.insert(scope, key, args_len, def);
        }
        Ok(if_chain_id)
    }

    fn build_pattern_conditions(
        &mut self,
        args: &Vec<AstId>,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
    ) -> Result<AstId, String> {
        let pattern_conds: Vec<AstId> = args
            .iter()
            .enumerate()
            .map(|(i, arg_id)| {
                let arg_ast = ast_heap.get(*arg_id).unwrap();
                let arg_name = format!("$arg{}", i);
                match arg_ast {
                    Ast::Identifier(_) => ast_heap.truthy_id,
                    Ast::Int(_) => {
                        let eql_bif = ast_heap.create_builtin_function(
                            atoms.put_atoms_in_set(AtomKind::NamedAtom(String::from("@eql"))),
                        );
                        let arg_name_atom_value =
                            atoms.put_atoms_in_set(AtomKind::NamedAtom(arg_name));
                        let arg_ident = ast_heap.create_identifier(arg_name_atom_value);
                        let eql_args = ast_heap.make_tuple(vec![*arg_id, arg_ident], atoms);
                        ast_heap.create_apply(eql_bif, eql_args)
                    }
                    _ => panic!("not a pattern!"),
                }
            })
            .collect();

        // Conjunct the conditions
        Ok(pattern_conds
            .iter()
            .copied()
            .reduce(|acc: AstId, e: AstId| {
                let and_bif = ast_heap.create_builtin_function(
                    atoms.put_atoms_in_set(AtomKind::NamedAtom(String::from("@and"))),
                );
                let and_args = ast_heap.make_tuple(vec![acc, e], atoms);
                ast_heap.create_apply(and_bif, and_args)
            })
            .unwrap_or(ast_heap.truthy_id))
    }

    fn rewrite_rhs_with_args(
        &mut self,
        args: &Vec<AstId>,
        rhs_value: AstId,
        scope: ScopeId,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
        symbol_table: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let mut modified_rhs_value = rhs_value;
        for (i, arg_id) in args.iter().enumerate() {
            let anon_arg_name = format!("$arg{}", i);
            let anon_arg_name_atom_value =
                atoms.put_atoms_in_set(AtomKind::NamedAtom(anon_arg_name));
            let anon_arg_ident = ast_heap.create_identifier(anon_arg_name_atom_value);

            let arg_ast = ast_heap.get(*arg_id).unwrap();
            match arg_ast {
                Ast::Identifier(atom) => {
                    let new_scope = symbol_table.new_scope(Some(scope));
                    symbol_table.insert(new_scope, *atom, 0, anon_arg_ident);
                    modified_rhs_value = ast_heap.create_let(new_scope, modified_rhs_value);
                }
                _ => {} // Leave rhs unchanged
            }
        }
        Ok(modified_rhs_value)
    }

    fn insert_into_if_chain(
        &mut self,
        if_chain_id: AstId,
        cond: AstId,
        rewritten_rhs: AstId,
        ast_heap: &mut AstHeap,
    ) {
        let if_chain_ast = ast_heap.get_mut(if_chain_id).unwrap();
        match if_chain_ast {
            Ast::If(conds, _else) => conds.push((cond, rewritten_rhs)),
            _ => panic!("whoa!"),
        }
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
            let mut terms = vec![];
            terms.push(expr);

            while let Some(_) = self.accept(TokenKind::Comma) {
                terms.push(self.let_in_expr(scope, ast_heap, atoms, symbol_table)?);
            }

            Ok(ast_heap.make_tuple(terms, atoms))
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
                    while let Some(_) = self.accept(TokenKind::Newline) {}
                    let _ = self.expect(TokenKind::Dedent)?;
                    while let Some(_) = self.accept(TokenKind::Newline) {}
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
        match self.peek().kind {
            TokenKind::Integer => self.parse_integer(ast_heap),
            TokenKind::Float => self.parse_float(ast_heap),
            TokenKind::Char => self.parse_char(ast_heap),
            TokenKind::String => self.parse_string(ast_heap),
            TokenKind::Atom => self.parse_atom(ast_heap, atoms),
            TokenKind::Identifier => self.parse_identifier(ast_heap, atoms),
            TokenKind::If => self.parse_if_expr(scope, ast_heap, atoms, symbol_table),
            TokenKind::LeftBrace => self.parse_map(scope, ast_heap, atoms, symbol_table),
            TokenKind::LeftSquare => self.parse_list(scope, ast_heap, atoms, symbol_table),
            TokenKind::LeftParen => self.parse_parens(scope, ast_heap, atoms, symbol_table),
            TokenKind::Indent => self.parse_indent(scope, ast_heap, atoms, symbol_table),
            _ => Err(self.parse_error(
                String::from("an expression"),
                format!("{:?}", self.peek().kind),
            )),
        }
    }

    fn parse_integer(&mut self, ast_heap: &mut AstHeap) -> Result<AstId, String> {
        let token = self.expect(TokenKind::Integer)?;
        let value = token.data.parse::<i64>().map_err(|e| e.to_string())?;
        Ok(ast_heap.create_int(value))
    }

    fn parse_float(&mut self, ast_heap: &mut AstHeap) -> Result<AstId, String> {
        let token = self.expect(TokenKind::Float)?;
        let value = token.data.parse::<f64>().map_err(|e| e.to_string())?;
        Ok(ast_heap.create_float(value))
    }

    fn parse_char(&mut self, ast_heap: &mut AstHeap) -> Result<AstId, String> {
        let token = self.expect(TokenKind::Char)?;
        Ok(ast_heap.create_char(token.data.chars().nth(1).unwrap()))
    }

    fn parse_string(&mut self, ast_heap: &mut AstHeap) -> Result<AstId, String> {
        let token = self.expect(TokenKind::String)?;
        let token_len = token.data.len();
        Ok(ast_heap.create_string(String::from(&token.data[1..token_len - 1])))
    }

    fn parse_atom(&mut self, ast_heap: &mut AstHeap, atoms: &mut AtomMap) -> Result<AstId, String> {
        let token = self.expect(TokenKind::Atom)?;
        let atom_value = atoms.put_atoms_in_set(AtomKind::NamedAtom(token.data.clone()));
        Ok(ast_heap.create_atom(atom_value))
    }

    fn parse_identifier(
        &mut self,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
    ) -> Result<AstId, String> {
        let token = self.expect(TokenKind::Identifier)?;
        let atom_value = atoms.put_atoms_in_set(AtomKind::NamedAtom(token.data.clone()));
        if ast_heap.identifier_is_bif(&token.data) {
            Ok(ast_heap.create_builtin_function(atom_value))
        } else {
            Ok(ast_heap.create_identifier(atom_value))
        }
    }

    fn parse_if_expr(
        &mut self,
        scope: ScopeId,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
        symbol_table: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let _ = self.expect(TokenKind::If)?;
        let mut conds = vec![];
        let condition = self.let_in_expr(scope, ast_heap, atoms, symbol_table)?;
        while let Some(_) = self.accept(TokenKind::Newline) {}
        let _ = self.expect(TokenKind::Then);
        let then = self.let_in_expr(scope, ast_heap, atoms, symbol_table)?;
        conds.push((condition, then));
        while self.accept(TokenKind::Elif).is_some() {
            let condition = self.let_in_expr(scope, ast_heap, atoms, symbol_table)?;
            let _ = self.expect(TokenKind::Then);
            let then = self.let_in_expr(scope, ast_heap, atoms, symbol_table)?;
            conds.push((condition, then));
        }
        while let Some(_) = self.accept(TokenKind::Newline) {}
        let _ = self.expect(TokenKind::Else);
        let else_ = self.let_in_expr(scope, ast_heap, atoms, symbol_table)?;
        Ok(ast_heap.create_if(conds, else_))
    }

    fn parse_map(
        &mut self,
        scope: ScopeId,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
        symbol_table: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let _ = self.expect(TokenKind::LeftBrace)?;
        let mut children: HashMap<AtomId, AstId> = HashMap::new();
        loop {
            let key_ast_id = self.let_in_expr(scope, ast_heap, atoms, symbol_table)?;
            let key = ast_heap.atom_from_ast(key_ast_id, atoms)?;
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
    }

    fn parse_list(
        &mut self,
        scope: ScopeId,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
        symbol_table: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let _ = self.expect(TokenKind::LeftSquare)?;
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
    }

    fn parse_parens(
        &mut self,
        scope: ScopeId,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
        symbol_table: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let _ = self.expect(TokenKind::LeftParen)?;
        let retval = self.tuple_expr(scope, ast_heap, atoms, symbol_table)?;
        let _ = self.expect(TokenKind::RightParen)?;
        Ok(retval)
    }

    fn parse_indent(
        &mut self,
        scope: ScopeId,
        ast_heap: &mut AstHeap,
        atoms: &mut AtomMap,
        symbol_table: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let _ = self.expect(TokenKind::Indent)?;
        let retval = self.lambda_expr(scope, ast_heap, atoms, symbol_table)?;
        while let Some(_) = self.accept(TokenKind::Newline) {}
        let _ = self.expect(TokenKind::Dedent)?;
        Ok(retval)
    }
}
