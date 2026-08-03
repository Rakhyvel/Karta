use std::{fmt::Display, num};

use crate::span::Span;

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
        lhs: String,
        rhs: String,
    },
    Unexpected {
        expected: String,
        got: String,
    },
    UnresolvedIdentifier {
        symbol_name: String,
    },
    DivisionByZero,
    QuotedEof,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::CannotOpenFile { filename } => write!(f, "cannot open {filename}"),
            ErrorKind::ParseIntError(parse_int_error) => write!(f, "{parse_int_error}"),
            ErrorKind::ParseFloatError(parse_float_error) => write!(f, "{parse_float_error}"),
            ErrorKind::UnknownBuiltin { name } => write!(f, "unknown builtin `{name}`"),
            ErrorKind::CannotBinop { verb, lhs, rhs } => write!(f, "cannot {verb} {lhs} and {rhs}"),
            ErrorKind::Unexpected { expected, got } => write!(f, "expected {expected}, got {got}"),
            ErrorKind::UnresolvedIdentifier { symbol_name } => {
                write!(f, "cannot resolve symbol `{symbol_name}`")
            }
            ErrorKind::DivisionByZero => write!(f, "division by zero"),
            ErrorKind::QuotedEof => write!(f, "unterminated quoted literal"),
        }
    }
}
