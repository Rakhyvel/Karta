use crate::tokenizer::{Token, TokenKind};

pub(crate) fn layout(tokens: &mut Vec<Token>) {
    // Setup a stack of indentation sizes
    // This is like a precedence stack for infix => postifx transformation
    let mut stack: Vec<usize> = Vec::new();
    stack.push(1);

    let mut i: usize = 0;
    while i < tokens.len() {
        let token_kind = tokens[i].kind;
        let token_data_len = tokens[i].data.len();
        let token_span = tokens[i].span;

        match token_kind {
            TokenKind::Newline => {
                if token_data_len == *stack.last().unwrap() {
                    // If token spaces == peek spaces => do nothing
                    // This implies this new line is a separator, not an indentor
                } else if token_data_len > *stack.last().unwrap() {
                    // If token spaces > peek spaces => append token spaces, replace with ident
                    stack.push(token_data_len);
                    tokens[i] = Token {
                        data: String::from(""),
                        kind: TokenKind::Indent,
                        span: token_span,
                    };
                } else {
                    // If token spaces < peek spaces => while token spaces < peek spaces {pop, replace with dedent}
                    while token_data_len < *stack.last().unwrap() {
                        stack.pop();
                        let slice = [
                            tokens[i].clone(),
                            Token {
                                data: String::from(""),
                                kind: TokenKind::Dedent,
                                span: token_span,
                            },
                            tokens[i].clone(),
                        ];
                        tokens.splice(i..i + 1, slice.iter().cloned());
                    }
                }
            }

            _ => {}
        }
        i += 1;
    }
}
