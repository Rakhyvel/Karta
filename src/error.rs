use crate::{interner::SymbolId, ir::Value, span::Span, tokenizer::TokenKind};

pub struct Error {
    span: Span,
    kind: ErrorKind,
}

pub enum ErrorKind {
    CannotBinop {
        verb: &'static str,
        lhs: Value,
        rhs: Value,
    },
    CannotConvert {
        value: Value,
        into: &'static str,
    },
    UnexpectedToken {
        expected: &'static str,
        token_kind: TokenKind,
    },
    UnexpectedValue {
        expected: &'static str,
        value: Value,
    },
    UnresolvedIdentifier {
        sym: SymbolId,
    },
    CharEof,
    StringEof,
}
