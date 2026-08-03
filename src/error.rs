use std::{fmt::Display, num};

use crate::{interner::SymbolId, ir::Value, span::Span, tokenizer::TokenKind};

#[derive(Debug, Clone)]
pub struct KartaError {
    pub span: Span,
    pub kind: ErrorKind,
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    CannotOpenFile {
        filename: String,
    },
    ParseIntError(num::ParseIntError),
    ParseFloatError(num::ParseFloatError),
    UnknownBuiltin {
        name: String,
    },
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
    UnexpectedToken2 {
        expected_kind: TokenKind,
        got_kind: TokenKind,
    },
    UnexpectedValue {
        expected: &'static str,
        value: Value,
    },
    UnexpectedGot {
        expected: &'static str,
        got: &'static str,
    },
    UnresolvedIdentifier {
        sym: SymbolId,
    },
    DivisionByZero,
    QuotedEof,
}

impl Display for KartaError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Debug stub for now
        write!(f, "{:?}", self)
    }
}
