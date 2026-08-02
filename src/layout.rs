use crate::tokenizer::{Token, TokenKind};

pub(crate) fn layout(tokens: &[Token]) -> Vec<Token> {
    let mut out = Vec::with_capacity(tokens.len());

    // Setup a stack of indentation sizes
    // This is like a precedence stack for infix => postifx transformation
    let mut stack: Vec<u32> = Vec::new();
    stack.push(1);

    let mut brace_depth: u32 = 0;

    let mut i: usize = 0;
    while i < tokens.len() {
        let token_kind = tokens[i].kind;
        let token_data_len = tokens[i].len();
        let token_span = tokens[i].span;

        match token_kind {
            TokenKind::LeftBrace | TokenKind::LeftParen | TokenKind::LeftSquare => {
                out.push(tokens[i]);
                brace_depth += 1
            }
            TokenKind::RightBrace | TokenKind::RightParen | TokenKind::RightSquare => {
                out.push(tokens[i]);
                brace_depth = brace_depth.saturating_sub(1)
            }

            TokenKind::Newline if brace_depth > 0 => {
                // newlines keep moving
                // this is a brace's neighborhood
            }
            TokenKind::Newline => {
                if token_data_len == *stack.last().unwrap() {
                    // If token spaces == peek spaces => do nothing
                    // This implies this new line is a separator, not an indentor
                    out.push(tokens[i])
                } else if token_data_len > *stack.last().unwrap() {
                    // If token spaces > peek spaces => append token spaces, replace with ident
                    stack.push(token_data_len);
                    out.push(Token {
                        kind: TokenKind::Indent,
                        span: token_span,
                    });
                } else {
                    // If token spaces < peek spaces => while token spaces < peek spaces {pop, replace with dedent}
                    while token_data_len < *stack.last().unwrap() {
                        stack.pop();
                        out.push(tokens[i]);
                        out.push(Token {
                            kind: TokenKind::Dedent,
                            span: token_span,
                        });
                        out.push(tokens[i]);
                    }
                }
            }

            _ => out.push(tokens[i]),
        }

        i += 1;
    }

    let eof_token_index = out
        .iter()
        .position(|t| t.kind == TokenKind::EndOfFile)
        .unwrap_or(out.len());

    while stack.len() > 1 {
        stack.pop();
        out.insert(
            eof_token_index,
            Token {
                kind: TokenKind::Dedent,
                span: out[eof_token_index].span,
            },
        );
    }

    out
}
