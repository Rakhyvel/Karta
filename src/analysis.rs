use std::collections::HashMap;

use crate::{
    elaborate::{Declare, Resolve},
    error::KartaError,
    layout,
    parser::Parser,
    scope::{DefKind, Definition},
    source::SourceFile,
    span::Span,
    tokenizer::{TokenKind, Tokenizer},
    walker::AstWalker,
    KartaContext,
};

pub struct Analysis {
    pub source: SourceFile,
    pub diagnostics: Vec<KartaError>,
    pub tokens: Vec<(Span, SemanticKind)>,
}

#[derive(Clone, Copy)]
pub enum SemanticKind {
    Keyword,
    Number,
    String,
    Atom,
    Builtin,
    Function,
    Variable, // catch-all for unresolvable identifiers
    Parameter,
    Constant,
    Operator,
    Other,
}

impl KartaContext {
    pub fn analyze(&mut self, text: impl ToString) -> Analysis {
        let mut analysis = Analysis {
            source: SourceFile::new(text.to_string()),
            diagnostics: vec![],
            tokens: vec![],
        };

        let mut tokenizer = Tokenizer::new(&analysis.source);
        let mut old_tokens = vec![];
        match tokenizer.tokenize(&mut old_tokens) {
            Ok(_) => {}
            Err(err) => {
                analysis.diagnostics.push(err);
                return analysis;
            }
        }

        analysis.tokens = old_tokens
            .iter()
            .map(|tok| (tok.span, semantic_kind(tok.kind)))
            .collect();
        let tokens = layout::layout(&old_tokens);

        let parser = Parser::new(
            &analysis.source,
            &tokens,
            &mut self.ast_heap,
            &mut self.pattern_heap,
            &mut self.symbol_table,
            &mut self.string_literal_table,
            &mut self.atom_table,
        );

        let (expr_ast, parse_errors) = parser.parse_file();
        for err in parse_errors {
            analysis.diagnostics.push(err);
        }

        match AstWalker::walk(
            &self.ast_heap,
            &self.pattern_heap,
            expr_ast,
            Declare::new(
                &self.ast_heap,
                &self.pattern_heap,
                &self.symbol_table,
                &mut self.elab,
            ),
        ) {
            Ok(_) => {}
            Err(err) => {
                analysis.diagnostics.push(err);
                return analysis;
            }
        }

        let mut overlay: HashMap<u32, SemanticKind> = HashMap::new();
        for (ast_id, def_id) in self.elab.defines() {
            let span = self.ast_heap.span(*ast_id);
            overlay.insert(span.start, kind_of(self.elab.def(*def_id)));
        }

        for (pattern_id, def_id) in self.elab.pattern_defines() {
            let span = self.pattern_heap.span(*pattern_id);
            overlay.insert(span.start, kind_of(self.elab.def(*def_id)));
        }

        Self::apply_overlay(&overlay, &mut analysis.tokens);

        let resolve = match AstWalker::walk(
            &self.ast_heap,
            &self.pattern_heap,
            expr_ast,
            Resolve::new(&self.ast_heap, &self.symbol_table, &mut self.elab),
        ) {
            Ok(r) => r,
            Err(err) => {
                analysis.diagnostics.push(err);
                return analysis;
            }
        };

        for err in resolve.errors() {
            analysis.diagnostics.push(err.clone());
        }

        for (ast_id, def_id) in self.elab.references() {
            let span = self.ast_heap.span(*ast_id);
            overlay.insert(span.start, kind_of(self.elab.def(*def_id)));
        }

        Self::apply_overlay(&overlay, &mut analysis.tokens);

        analysis
    }

    fn apply_overlay(overlay: &HashMap<u32, SemanticKind>, tokens: &mut [(Span, SemanticKind)]) {
        for (span, old_kind) in tokens.iter_mut() {
            if let Some(kind) = overlay.get(&span.start) {
                *old_kind = *kind
            }
        }
    }
}

fn semantic_kind(kind: TokenKind) -> SemanticKind {
    match kind {
        TokenKind::Let
        | TokenKind::In
        | TokenKind::If
        | TokenKind::Then
        | TokenKind::Elif
        | TokenKind::Else
        | TokenKind::When => SemanticKind::Keyword,

        TokenKind::Integer | TokenKind::Float => SemanticKind::Number,
        TokenKind::String | TokenKind::Char => SemanticKind::String,
        TokenKind::Atom => SemanticKind::Atom,
        TokenKind::Builtin => SemanticKind::Builtin,
        TokenKind::Identifier | TokenKind::Wildcard => SemanticKind::Variable,

        TokenKind::Arrow | TokenKind::Assign | TokenKind::Backslash => SemanticKind::Operator,

        _ => SemanticKind::Other,
    }
}

fn kind_of(definition: &Definition) -> SemanticKind {
    match definition.kind() {
        DefKind::Parameter | DefKind::AnonymousParameter => SemanticKind::Parameter,
        DefKind::Function if definition.arity() > 0 => SemanticKind::Function,
        DefKind::Function => SemanticKind::Constant,
    }
}
