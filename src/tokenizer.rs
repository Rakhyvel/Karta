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
                }
                TokenizerState::Whitespace => {
                    if char == '\n' {
                        self.line += 1;
                        self.col = 0;
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
                        || self.first_char_is_singular()
                        || char.is_whitespace()
                        || char == '.' =>
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

    fn first_char_is_singular(&self) -> bool {
        const SINGULAR_CHARS: [char; 6] = ['[', ']', '(', ')', '{', '}'];
        for c in SINGULAR_CHARS {
            if c == self
                .file_contents
                .chars()
                .nth(self.starting_cursor)
                .unwrap()
            {
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
    pub(crate) data: String, // TODO: Should figure out how to just use `&'a str` here.
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
    Dedent,
    Indent,
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
            "(" => TokenKind::LeftParen,
            ")" => TokenKind::RightParen,
            "," => TokenKind::Comma,
            "=" => TokenKind::Assign,
            "let" => TokenKind::Let,
            "in" => TokenKind::In,
            _ if str.chars().nth(0).unwrap() == '.' => TokenKind::Atom,
            _ if str.chars().nth(0).unwrap().is_digit(10) => TokenKind::Integer,
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
