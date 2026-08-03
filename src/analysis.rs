use crate::{
    elaborate::{Declare, Resolve},
    error::KartaError,
    parser::Parser,
    source::SourceFile,
    span::Span,
    tokenizer::TokenKind,
    walker::AstWalker,
    KartaContext,
};

pub struct Analysis {
    pub source: SourceFile,
    pub diagnostics: Vec<KartaError>,
    pub tokens: Vec<(Span, TokenKind)>,
}

impl KartaContext {
    pub fn analyze(&mut self, text: impl ToString) -> Analysis {
        let mut diagnostics = vec![];
        let source = SourceFile::new(text.to_string());

        let mut parser = Parser::new(
            &source,
            &mut self.ast_heap,
            &mut self.pattern_heap,
            &mut self.symbol_table,
            &mut self.string_literal_table,
            &mut self.atom_table,
        );

        let expr_ast = match parser.parse_file() {
            Ok(ok) => ok,
            Err(err) => {
                diagnostics.push(err);
                return Analysis {
                    source,
                    diagnostics,
                    tokens: vec![],
                };
            }
        };

        match AstWalker::walk(
            &self.ast_heap,
            &self.pattern_heap,
            expr_ast,
            Declare::new(&self.ast_heap, &self.pattern_heap, &mut self.elab),
        ) {
            Ok(_) => {}
            Err(err) => {
                diagnostics.push(err);
                return Analysis {
                    source,
                    diagnostics,
                    tokens: vec![],
                };
            }
        }

        match AstWalker::walk(
            &self.ast_heap,
            &self.pattern_heap,
            expr_ast,
            Resolve::new(&self.ast_heap, &self.symbol_table, &mut self.elab),
        ) {
            Ok(_) => {}
            Err(err) => {
                diagnostics.push(err);
            }
        }

        Analysis {
            source,
            diagnostics,
            tokens: vec![],
        }
    }
}
