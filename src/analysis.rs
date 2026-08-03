use crate::{
    elaborate::{Declare, Resolve},
    error::KartaError,
    layout,
    parser::Parser,
    source::SourceFile,
    tokenizer::{Token, Tokenizer},
    walker::AstWalker,
    KartaContext,
};

pub struct Analysis {
    pub source: SourceFile,
    pub diagnostics: Vec<KartaError>,
    pub tokens: Vec<Token>,
}

impl KartaContext {
    pub fn analyze(&mut self, text: impl ToString) -> Analysis {
        let mut analysis = Analysis {
            source: SourceFile::new(text.to_string()),
            diagnostics: vec![],
            tokens: vec![],
        };

        let mut tokenizer = Tokenizer::new(&analysis.source);
        match tokenizer.tokenize(&mut analysis.tokens) {
            Ok(_) => {}
            Err(err) => {
                analysis.diagnostics.push(err);
                return analysis;
            }
        }
        let tokens = layout::layout(&analysis.tokens);

        let mut parser = Parser::new(
            &analysis.source,
            &tokens,
            &mut self.ast_heap,
            &mut self.pattern_heap,
            &mut self.symbol_table,
            &mut self.string_literal_table,
            &mut self.atom_table,
        );

        let expr_ast = match parser.parse_file() {
            Ok(ok) => ok,
            Err(err) => {
                analysis.diagnostics.push(err);
                return analysis;
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
                analysis.diagnostics.push(err);
                return analysis;
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
                analysis.diagnostics.push(err);
                return analysis;
            }
        }

        analysis
    }
}
