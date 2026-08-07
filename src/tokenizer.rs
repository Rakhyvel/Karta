use crate::{
    error::{ErrorKind, KartaError},
    source::SourceFile,
    span::Span,
};

/// Converts file contents text into a stream of tokens
pub struct Tokenizer<'a> {
    /// Where in the file the tokenizer is currently working
    cursor: u32,
    /// The cursor of the begining of the current token that the tokenizer is working on
    starting_cursor: u32,
    /// The actual contents of the file
    source_file: &'a SourceFile,
    /// The state of the tokenizer
    state: TokenizerState,
}

impl<'a> Tokenizer<'a> {
    /// Create a new tokenizer, taking ownership of the file contents string
    pub fn new(source_file: &'a SourceFile) -> Self {
        Self {
            cursor: 0,
            source_file,
            state: TokenizerState::None,
            starting_cursor: 0,
        }
    }

    /// Convert the file contents string into a stream of tokens
    pub fn tokenize(&mut self, tokens: &mut Vec<Token>) -> Result<(), KartaError> {
        while let Some(char) = self.current_char() {
            match self.state {
                TokenizerState::None => self.handle_none(char),
                TokenizerState::Whitespace => self.handle_whitespace(char, tokens),
                TokenizerState::Integer => self.handle_integer(char, tokens),
                TokenizerState::Atom => self.handle_sigiled(TokenKind::Atom, char, tokens),
                TokenizerState::Builtin => self.handle_sigiled(TokenKind::Builtin, char, tokens),
                TokenizerState::Char => self.handle_quoted('\'', char, tokens)?,
                TokenizerState::String => self.handle_quoted('"', char, tokens)?,
                TokenizerState::Symbol => self.handle_symbol(char, tokens),
                TokenizerState::Float => self.handle_float(char, tokens),
                TokenizerState::Comment => self.handle_comment(char),
            }
        }

        self.add_token(TokenKind::EndOfFile, tokens);

        Ok(())
    }

    /// The none state branches off into various other states depending on the next character
    fn handle_none(&mut self, char: char) {
        if char.is_whitespace() {
            self.advance(TokenizerState::Whitespace)
        } else if char.is_ascii_digit() {
            self.advance(TokenizerState::Integer)
        } else if char == '.' {
            self.advance(TokenizerState::Atom)
        } else if char == '@' {
            self.advance(TokenizerState::Builtin)
        } else if char == '\'' {
            self.advance(TokenizerState::Char)
        } else if char == '"' {
            self.advance(TokenizerState::String)
        } else if char == ';' {
            self.advance(TokenizerState::Comment)
        } else {
            self.advance(TokenizerState::Symbol)
        }
    }

    /// Whitespace state ends when the next char isn't whitespace
    fn handle_whitespace(&mut self, char: char, tokens: &mut Vec<Token>) {
        if self.eof() || !char.is_whitespace() {
            // If token doesn't contain a newline, just ignore it
            // If it does, the data is from the newline all the way to the end

            let token_data = &self.source_file.span_text(Span {
                start: self.starting_cursor,
                end: self.cursor,
            });
            if let Some(offset) = token_data.rfind('\n') {
                let token = Token {
                    kind: TokenKind::Newline,
                    span: Span {
                        start: self.starting_cursor + offset as u32,
                        end: self.cursor,
                    },
                };
                tokens.push(token);
            }

            self.starting_cursor = self.cursor;
            self.state = TokenizerState::None
        } else {
            self.advance(TokenizerState::Whitespace);
        }
    }

    /// Integers become floats if a `.` is encountered, otherwise end when the next char isn't a digit
    fn handle_integer(&mut self, char: char, tokens: &mut Vec<Token>) {
        if char == '.' {
            self.advance(TokenizerState::Float)
        } else if self.eof() || !char.is_ascii_digit() {
            self.add_token(TokenKind::Integer, tokens);
        } else {
            self.advance(self.state)
        }
    }

    /// Create a sigil token (atom or builtin)
    fn handle_sigiled(&mut self, kind: TokenKind, char: char, tokens: &mut Vec<Token>) {
        if self.eof() || (char.is_whitespace() || self.char_is_singular(char)) {
            self.add_token(kind, tokens)
        } else {
            self.advance(self.state)
        }
    }

    /// Quoted tokens end at the second single quote
    fn handle_quoted(
        &mut self,
        quote: char,
        char: char,
        tokens: &mut Vec<Token>,
    ) -> Result<(), KartaError> {
        // TODO: Escapes
        if self.eof() {
            Err(KartaError {
                span: Span {
                    start: self.starting_cursor,
                    end: self.cursor,
                },
                kind: ErrorKind::QuotedEof,
            })
        } else if char == quote {
            self.advance(TokenizerState::None);
            let kind = match quote {
                '\'' => TokenKind::Char,
                '"' => TokenKind::String,
                _ => unreachable!(),
            };
            self.add_token(kind, tokens);
            Ok(())
        } else {
            self.advance(self.state);
            Ok(())
        }
    }

    /// Symbols end at the end of the file, or if the next token isn't recognized
    fn handle_symbol(&mut self, char: char, tokens: &mut Vec<Token>) {
        if self.eof()
            || self.first_char_is_singular()
            || self.char_is_singular(char)
            || char.is_whitespace()
            || char == '.'
        {
            let token_data = &self.source_file.span_text(Span {
                start: self.starting_cursor,
                end: self.cursor,
            });
            let token_kind = TokenKind::from_string(token_data);
            self.add_token(token_kind, tokens);
        } else {
            self.advance(self.state);
        }
    }

    /// Floats end at the end of file, or if the character is no longer a digit
    fn handle_float(&mut self, char: char, tokens: &mut Vec<Token>) {
        if self.eof() || !char.is_ascii_digit() {
            self.add_token(TokenKind::Float, tokens)
        } else {
            self.advance(self.state);
        }
    }

    /// Comments end at newlines
    fn handle_comment(&mut self, char: char) {
        if char == '\n' {
            self.starting_cursor = self.cursor;
            self.state = TokenizerState::None
        } else {
            self.advance(self.state)
        }
    }

    fn first_char_is_singular(&self) -> bool {
        self.source_file.text()[self.starting_cursor as usize..]
            .chars()
            .next()
            .is_some_and(|c| self.char_is_singular(c))
    }

    fn char_is_singular(&self, c: char) -> bool {
        const SINGULAR_CHARS: [char; 8] = ['[', ']', '(', ')', '{', '}', ',', '\\'];
        SINGULAR_CHARS.contains(&c)
    }

    fn current_char(&self) -> Option<char> {
        self.source_file.text()[self.cursor as usize..]
            .chars()
            .next()
    }

    /// Whether or not the tokenizer is at the end of the file
    fn eof(&self) -> bool {
        self.current_char().is_none()
    }

    /// Advances the cursor and column number, and changes the state to a new state
    fn advance(&mut self, new_state: TokenizerState) {
        if let Some(c) = self.current_char() {
            self.cursor += c.len_utf8() as u32;
        }
        self.state = new_state;
    }

    /// Adds the current span as a token to the list of tokens
    fn add_token(&mut self, kind: TokenKind, tokens: &mut Vec<Token>) {
        let token = Token {
            kind,
            span: Span {
                start: self.starting_cursor,
                end: self.cursor,
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
    Builtin,
    Char,
    String,
    Symbol,
    Float,
    Comment,
}

#[derive(Copy, Clone, Debug)]
/// Represents a single piece of text in the file
pub struct Token {
    /// What kind of token this is
    pub kind: TokenKind,
    /// Where in the file this token came from
    pub span: Span,
}

impl Token {
    pub fn len(&self) -> u32 {
        self.span.end - self.span.start
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
/// Represents the various kinds a token can be
/// TODO: impl Display
pub enum TokenKind {
    Newline,
    LeftBrace,
    RightBrace,
    LeftSquare,
    RightSquare,
    LeftParen,
    RightParen,
    Backslash,
    Arrow,
    Atom,
    Builtin,
    Integer,
    Float,
    Char,
    String,
    Identifier,
    Wildcard,
    Comma,
    Assign,
    Let,
    In,
    If,
    Then,
    Elif,
    Else,
    Dedent,
    Indent,
    EndOfFile,
}

impl TokenKind {
    /// Get the token kind from a string representation
    fn from_string(str: &str) -> Self {
        assert!(!str.is_empty());
        match str {
            "{" => TokenKind::LeftBrace,
            "}" => TokenKind::RightBrace,
            "[" => TokenKind::LeftSquare,
            "]" => TokenKind::RightSquare,
            "(" => TokenKind::LeftParen,
            ")" => TokenKind::RightParen,
            "_" => TokenKind::Wildcard,
            "\\" => TokenKind::Backslash,
            "->" => TokenKind::Arrow,
            "," => TokenKind::Comma,
            "=" => TokenKind::Assign,
            "let" => TokenKind::Let,
            "in" => TokenKind::In,
            "if" => TokenKind::If,
            "then" => TokenKind::Then,
            "elif" => TokenKind::Elif,
            "else" => TokenKind::Else,
            _ => match str.chars().next() {
                Some('.') => TokenKind::Atom,
                Some(c) if c.is_ascii_digit() => TokenKind::Integer,
                _ => TokenKind::Identifier,
            },
        }
    }
}
