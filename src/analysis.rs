use crate::{error::KartaError, span::Span, tokenizer::TokenKind};

pub struct Analysis {
    pub tokens: Vec<(Span, TokenKind)>,
    pub diagnostics: Vec<KartaError>,
    pub semantic: Vec<(Span, SemanticKind)>, // TODO: Get from elaboration, after parsing succeeds
}

pub enum SemanticKind {
    RegularAssToken(TokenKind), // TODO: Will need more...
}

impl Analysis {
    pub fn analyze(text: &str) -> Analysis {
        todo!()
    }
}
