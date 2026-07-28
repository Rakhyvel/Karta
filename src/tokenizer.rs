/// Converts file contents text into a stream of tokens
pub(crate) struct Tokenizer {
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
    pub(crate) fn new(mut file_contents: String) -> Self {
        file_contents.push('\n'); // This is required to make the tokenizer happy
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
    pub(crate) fn tokenize(&mut self, tokens: &mut Vec<Token>) -> Result<(), String> {
        while !self.eof() {
            let char = self.file_contents.chars().nth(self.cursor).unwrap(); // yeah probably slow, but it doesn't matter

            match self.state {
                TokenizerState::None => self.handle_none(char),
                TokenizerState::Whitespace => self.handle_whitespace(char, tokens),
                TokenizerState::Integer => self.handle_integer(char, tokens),
                TokenizerState::Atom => self.handle_atom(char, tokens),
                TokenizerState::Char => self.handle_char(char, tokens)?,
                TokenizerState::String => self.handle_string(char, tokens)?,
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

            let token_data = &self.file_contents[self.starting_cursor..self.cursor];
            if let Some(last_newline) = token_data.rfind('\n') {
                let token_data = String::from(
                    &self.file_contents[(self.starting_cursor + last_newline)..self.cursor],
                );
                let token = Token {
                    data: token_data,
                    kind: TokenKind::Newline,
                    span: Span {
                        col: self.col - 1,
                        line: self.line,
                    },
                };
                tokens.push(token);
            }

            self.starting_cursor = self.cursor;
            self.state = TokenizerState::None
        } else {
            if char == '\n' {
                self.line += 1;
                self.col = 0;
            }
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

    /// Atoms end when the next char isn't a valid atom character
    fn handle_atom(&mut self, char: char, tokens: &mut Vec<Token>) {
        if self.eof() || (char.is_whitespace() || self.char_is_singular(char)) {
            self.add_token(TokenKind::Atom, tokens)
        } else {
            self.advance(self.state)
        }
    }

    /// Characters end at the second single quote
    fn handle_char(&mut self, char: char, tokens: &mut Vec<Token>) -> Result<(), String> {
        if self.eof() {
            Err(String::from("error: char goes to end of file"))
        } else if char == '\'' {
            self.advance(TokenizerState::None);
            self.add_token(TokenKind::Char, tokens);
            Ok(())
        } else {
            self.advance(self.state);
            Ok(())
        }
    }

    /// Strings end at the second double quote
    fn handle_string(&mut self, char: char, tokens: &mut Vec<Token>) -> Result<(), String> {
        if self.eof() {
            Err(String::from("error: string goes to end of file"))
        } else if char == '"' {
            self.advance(TokenizerState::None);
            self.add_token(TokenKind::String, tokens);
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
            let token_data = &self.file_contents[self.starting_cursor..self.cursor];
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
            self.line += 1;
            self.col = 1;
            self.starting_cursor = self.cursor;
            self.state = TokenizerState::None
        } else {
            self.advance(self.state)
        }
    }

    fn first_char_is_singular(&self) -> bool {
        self.char_is_singular(
            self.file_contents
                .chars()
                .nth(self.starting_cursor)
                .unwrap(),
        )
    }

    fn char_is_singular(&self, c: char) -> bool {
        const SINGULAR_CHARS: [char; 8] = ['[', ']', '(', ')', '{', '}', ',', '\\'];
        for singular_c in SINGULAR_CHARS {
            if c == singular_c {
                return true;
            }
        }
        false
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

#[derive(Clone, Debug)]
/// Represents a single piece of text in the file
pub(crate) struct Token {
    /// Owning string representing the actual text data for this string
    pub(crate) data: String,
    /// What kind of token this is
    pub(crate) kind: TokenKind,
    /// Where in the file this token came from
    pub(crate) span: Span,
}

#[derive(PartialEq, Clone, Copy, Debug)]
/// Represents the various kinds a token can be
pub(crate) enum TokenKind {
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
    Integer,
    Float,
    Char,
    String,
    Identifier,
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
            _ if str.chars().nth(0).unwrap() == '.' => TokenKind::Atom,
            _ if str.chars().nth(0).unwrap().is_ascii_digit() => TokenKind::Integer,
            _ => TokenKind::Identifier,
        }
    }
}

#[derive(Clone, Copy, Debug)]
/// Represents a position in a text file
pub(crate) struct Span {
    /// Line number of the file, starts at 1
    pub(crate) line: usize,
    /// Column number of the file, starts at 1
    pub(crate) col: usize,
}
