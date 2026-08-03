use crate::{
    ast::{AstHeap, AstId},
    builtin::Builtin,
    error::{ErrorKind, KartaError},
    interner::{AtomTable, StringLiteralTable, SymbolTable},
    pattern::{PatternHeap, PatternId},
    source::SourceFile,
    span::Span,
    tokenizer::{Token, TokenKind},
};

/// Parses a stream of tokens into Asts
pub(crate) struct Parser<'a> {
    cursor: usize,
    tokens: &'a Vec<Token>,
    source: &'a SourceFile,
    asts: &'a mut AstHeap,
    patterns: &'a mut PatternHeap,
    symbols: &'a mut SymbolTable,
    strings: &'a mut StringLiteralTable,
    atoms: &'a mut AtomTable,
    errors: Vec<KartaError>,
}

impl<'a> Parser<'a> {
    /// Creates a new Parser
    pub(crate) fn new(
        source: &'a SourceFile,
        tokens: &'a Vec<Token>,
        asts: &'a mut AstHeap,
        patterns: &'a mut PatternHeap,
        symbols: &'a mut SymbolTable,
        strings: &'a mut StringLiteralTable,
        atoms: &'a mut AtomTable,
    ) -> Self {
        Self {
            cursor: 0,
            tokens,
            source,
            asts,
            patterns,
            symbols,
            strings,
            atoms,
            errors: vec![],
        }
    }

    /// Parses file contents into an AST
    pub(crate) fn parse_file(mut self) -> (AstId, Vec<KartaError>) {
        self.accept_newlines();

        let bindings = self.parse_bindings(TokenKind::EndOfFile);

        (
            self.asts.create_file(self.tokens[0].span, bindings),
            self.errors,
        )
    }

    pub(crate) fn parse_expr(&mut self) -> Result<AstId, KartaError> {
        self.let_in_expr()
    }

    /// Returns the token at the begining of the stream without removing it
    fn peek(&self) -> Token {
        self.tokens[self.cursor]
    }

    /// Removes and returns the token at the begining of the stream
    fn pop(&mut self) -> Token {
        self.cursor += 1;
        self.tokens[self.cursor - 1]
    }

    /// Determines if the next token in the stream begins an expression
    fn next_is_expr(&self) -> bool {
        matches!(
            self.peek().kind,
            TokenKind::Integer
                | TokenKind::Float
                | TokenKind::Char
                | TokenKind::String
                | TokenKind::Atom
                | TokenKind::Builtin
                | TokenKind::Identifier
                | TokenKind::Indent
                | TokenKind::LeftBrace
                | TokenKind::LeftParen
                | TokenKind::LeftSquare
        )
    }

    /// Returns whether or not the parser is at the end of the stream
    fn eos(&self) -> bool {
        self.cursor >= self.tokens.len() || self.peek().kind == TokenKind::EndOfFile
    }

    /// Pops the token at the begining of the stream if its kind matches the given kind, otherwise None
    fn accept(&mut self, kind: TokenKind) -> Option<Token> {
        if !self.eos() && self.peek().kind == kind {
            Some(self.pop())
        } else {
            None
        }
    }

    /// Pops the token at the begining of the stream if its kind matches the given kind, otherwise Err
    fn expect(&mut self, kind: TokenKind) -> Result<Token, KartaError> {
        let peeked = self.peek();
        self.accept(kind).ok_or(KartaError {
            span: peeked.span,
            kind: ErrorKind::Unexpected {
                expected: format!("{kind:?}"),
                got: format!("{:?}", peeked.kind),
            },
        })
    }

    fn accept_newlines(&mut self) {
        while self.accept(TokenKind::Newline).is_some() {}
    }

    fn sync(&mut self, end_kind: TokenKind) {
        while !self.eos()
            && self.peek().kind != end_kind
            && self.peek().kind != TokenKind::Newline
            && self.peek().kind != TokenKind::Dedent
        {
            self.pop();
        }
    }

    fn parse_bindings(&mut self, end_kind: TokenKind) -> Vec<AstId> {
        let mut retval = vec![];
        while !self.eos() && self.peek().kind != end_kind {
            let before = self.cursor;

            match self.parse_binding() {
                Ok(binding) => retval.push(binding),
                Err(err) => {
                    self.errors.push(err);
                    if self.cursor == before {
                        self.pop();
                    }
                    self.sync(end_kind)
                }
            }
            self.accept_newlines();
        }
        retval
    }

    fn parse_binding(&mut self) -> Result<AstId, KartaError> {
        // Parse the name of this binding
        let name_span = self.expect(TokenKind::Identifier)?.span;
        let name_text = self.source.span_text(name_span);
        let name = self.symbols.intern(name_text);

        // Parse patterns into a list of args before the `=`
        let params: Vec<PatternId> = self.parse_pattern_list()?;

        // Parse the RHS after the `=`
        let rhs_value = self.let_in_expr()?;
        self.accept_newlines();

        // Create the binding
        Ok(self.asts.create_binding(name_span, name, params, rhs_value))
    }

    /// Parses binding params in between the binding name and its `=` sign
    fn parse_pattern_list(&mut self) -> Result<Vec<PatternId>, KartaError> {
        let mut params = Vec::new();
        // Keep trying to parse exprs until you hit an `=` sign
        while self.accept(TokenKind::Assign).is_none() {
            params.push(self.parse_pattern()?);
        }
        Ok(params)
    }

    fn tuple_expr(&mut self) -> Result<AstId, KartaError> {
        let expr = self.let_in_expr()?;
        if let TokenKind::Comma = self.peek().kind {
            let mut terms = vec![];
            terms.push(expr);

            while self.accept(TokenKind::Comma).is_some() {
                terms.push(self.let_in_expr()?);
            }

            Ok(self.asts.make_tuple(self.peek().span, terms))
        } else {
            Ok(expr)
        }
    }

    fn let_in_expr(&mut self) -> Result<AstId, KartaError> {
        match self.peek().kind {
            TokenKind::Let => {
                self.pop();
                let bindings: Vec<AstId>;
                if self.peek().kind == TokenKind::Indent {
                    self.expect(TokenKind::Indent)?;
                    self.accept_newlines();
                    bindings = self.parse_bindings(TokenKind::Dedent);
                    self.accept_newlines();
                    self.expect(TokenKind::Dedent)?;
                    self.accept_newlines();
                } else {
                    bindings = self.parse_bindings(TokenKind::In);
                }
                self.expect(TokenKind::In)?;
                let expr = self.let_in_expr()?;
                Ok(self.asts.create_let(self.peek().span, bindings, expr))
            }
            _ => self.lambda_expr(),
        }
    }

    fn lambda_expr(&mut self) -> Result<AstId, KartaError> {
        match self.peek().kind {
            TokenKind::Backslash => {
                self.pop();
                let pattern = self.parse_pattern()?;
                self.expect(TokenKind::Arrow)?;
                let expr = self.lambda_expr()?;
                Ok(self.asts.create_lambda(self.peek().span, pattern, expr))
            }
            _ => self.apply_expr(),
        }
    }

    fn apply_expr(&mut self) -> Result<AstId, KartaError> {
        let mut expr = self.expr()?;
        while self.next_is_expr() {
            let rhs = self.expr()?;

            let span = self.asts.span(expr);
            expr = self.asts.create_apply(span, expr, rhs);
        }
        Ok(expr)
    }

    /// Parses an expression
    fn expr(&mut self) -> Result<AstId, KartaError> {
        match self.peek().kind {
            TokenKind::Integer => self.parse_integer(),
            TokenKind::Float => self.parse_float(),
            TokenKind::Char => self.parse_char(),
            TokenKind::String => self.parse_string(),
            TokenKind::Atom => self.parse_atom(),
            TokenKind::Builtin => self.parse_builtin(),
            TokenKind::Identifier => self.parse_identifier(),
            TokenKind::If => self.parse_if_expr(),
            TokenKind::LeftBrace => self.parse_map(),
            TokenKind::LeftSquare => self.parse_list(),
            TokenKind::LeftParen => self.parse_parens(),
            TokenKind::Indent => self.parse_indent(),
            _ => Err(KartaError {
                span: self.peek().span,
                kind: ErrorKind::Unexpected {
                    expected: String::from("an expression"),
                    got: format!("{:?}", self.peek().kind),
                },
            }),
        }
    }

    fn parse_pattern(&mut self) -> Result<PatternId, KartaError> {
        match self.peek().kind {
            TokenKind::Identifier => self.parse_pattern_identifier(),
            _ => Err(KartaError {
                span: self.peek().span,
                kind: ErrorKind::Unexpected {
                    expected: String::from("a pattern"),
                    got: format!("{:?}", self.peek().kind),
                },
            }),
        }
    }

    fn parse_integer(&mut self) -> Result<AstId, KartaError> {
        let token = self.expect(TokenKind::Integer)?;
        let value = self
            .source
            .span_text(token.span)
            .parse::<i64>()
            .map_err(|e| KartaError {
                span: token.span,
                kind: ErrorKind::ParseIntError(e),
            })?;
        Ok(self.asts.create_int(token.span, value))
    }

    fn parse_float(&mut self) -> Result<AstId, KartaError> {
        let token = self.expect(TokenKind::Float)?;
        let value = self
            .source
            .span_text(token.span)
            .parse::<f64>()
            .map_err(|e| KartaError {
                span: token.span,
                kind: ErrorKind::ParseFloatError(e),
            })?;
        Ok(self.asts.create_float(token.span, value))
    }

    fn parse_char(&mut self) -> Result<AstId, KartaError> {
        let token = self.expect(TokenKind::Char)?;
        Ok(self.asts.create_char(
            token.span,
            self.source.span_text(token.span).chars().nth(1).unwrap(),
        ))
    }

    fn parse_string(&mut self) -> Result<AstId, KartaError> {
        let token_span = self.expect(TokenKind::String)?.span;
        let token_text = self.source.span_text(token_span);
        let string_literal_id = self.strings.intern(token_text);
        Ok(self.asts.create_string(token_span, string_literal_id))
    }

    fn parse_atom(&mut self) -> Result<AstId, KartaError> {
        let token_span = self.expect(TokenKind::Atom)?.span;
        let token_text = self.source.span_text(token_span);
        let atom_id = self.atoms.intern(token_text);
        Ok(self.asts.create_atom(token_span, atom_id))
    }

    fn parse_builtin(&mut self) -> Result<AstId, KartaError> {
        let name_span = self.expect(TokenKind::Builtin)?.span;
        let name_text = self.source.span_text(name_span);
        let builtin = Builtin::parse(name_text).ok_or(KartaError {
            span: name_span,
            kind: ErrorKind::UnknownBuiltin {
                name: String::from(name_text),
            },
        })?;
        Ok(self.asts.create_builtin_function(name_span, builtin))
    }

    fn parse_identifier(&mut self) -> Result<AstId, KartaError> {
        let name_span = self.expect(TokenKind::Identifier)?.span;
        let name_text = self.source.span_text(name_span);
        let name = self.symbols.intern(name_text);
        Ok(self.asts.create_identifier(name_span, name))
    }

    fn parse_pattern_identifier(&mut self) -> Result<PatternId, KartaError> {
        let name_span = self.expect(TokenKind::Identifier)?.span;
        let name_text = self.source.span_text(name_span);
        let name = self.symbols.intern(name_text);
        Ok(self.patterns.create_identifier(name_span, name))
    }

    fn parse_if_expr(&mut self) -> Result<AstId, KartaError> {
        let token = self.expect(TokenKind::If)?;
        let mut conds = vec![];
        let condition = self.let_in_expr()?;
        self.accept_newlines();
        self.expect(TokenKind::Then)?;
        let then = self.let_in_expr()?;
        conds.push((condition, then));
        self.accept_newlines();
        while self.accept(TokenKind::Elif).is_some() {
            let condition = self.let_in_expr()?;
            self.accept_newlines();
            self.expect(TokenKind::Then)?;
            self.accept_newlines();
            let then = self.let_in_expr()?;
            conds.push((condition, then));
        }
        self.accept_newlines();
        self.expect(TokenKind::Else)?;
        let else_ = self.let_in_expr()?;
        Ok(self.asts.create_if(token.span, conds, else_))
    }

    fn parse_map(&mut self) -> Result<AstId, KartaError> {
        let token = self.expect(TokenKind::LeftBrace)?;
        let mut children: Vec<(AstId, AstId)> = Vec::new();

        let truthy_atom = self
            .asts
            .create_atom(Span { start: 0, end: 0 }, AtomTable::TRUE);

        if self.accept(TokenKind::RightBrace).is_none() {
            children.push(self.parse_map_elem(truthy_atom)?);

            while self.accept(TokenKind::Comma).is_some() {
                if let TokenKind::RightBrace = self.peek().kind {
                    // Comma accepted above was a trailing comma, break
                    break;
                }
                children.push(self.parse_map_elem(truthy_atom)?);
            }
            self.expect(TokenKind::RightBrace)?;
        }
        Ok(self.asts.create_map(token.span, children))
    }

    fn parse_map_elem(&mut self, truthy_atom: AstId) -> Result<(AstId, AstId), KartaError> {
        let key = self.let_in_expr()?;

        if let Some(_token) = self.accept(TokenKind::Assign) {
            // Map field, parse and insert value
            let value = self.let_in_expr()?;
            Ok((key, value))
        } else {
            // Set field, insert `.t` as the value
            Ok((key, truthy_atom))
        }
    }

    fn parse_list(&mut self) -> Result<AstId, KartaError> {
        let token = self.expect(TokenKind::LeftSquare)?;

        let mut children = Vec::new();

        loop {
            let child = self.let_in_expr()?;
            children.push(child);

            if self.accept(TokenKind::Comma).is_none() {
                break;
            }
        }
        self.expect(TokenKind::RightSquare)?;
        Ok(self.asts.make_list(token.span, children))
    }

    fn parse_parens(&mut self) -> Result<AstId, KartaError> {
        self.expect(TokenKind::LeftParen)?;
        let retval = self.tuple_expr()?;
        self.expect(TokenKind::RightParen)?;
        Ok(retval)
    }

    fn parse_indent(&mut self) -> Result<AstId, KartaError> {
        self.expect(TokenKind::Indent)?;
        let retval = self.lambda_expr()?;
        self.accept_newlines();
        self.expect(TokenKind::Dedent)?;
        Ok(retval)
    }
}
