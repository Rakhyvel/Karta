pub mod analysis;
mod ast;
mod builtin;
mod debug;
mod elaborate;
mod error;
mod eval;
mod interner;
mod ir;
mod layout;
mod parser;
mod pattern;
mod scope;
pub mod source;
pub mod span;
pub mod tokenizer;
mod walker;

use std::{fs, path::PathBuf};

use ast::AstHeap;
use parser::Parser;

use crate::{
    ast::AstId,
    elaborate::{Declare, Elaboration, Resolve},
    error::{ErrorKind, KartaError},
    eval::{Eval, Heap, ValueRef},
    interner::{AtomTable, StringLiteralTable, SymbolTable},
    ir::Lowerer,
    pattern::PatternHeap,
    source::SourceFile,
    span::Span,
    tokenizer::Tokenizer,
    walker::AstWalker,
};

/// Represents the context for evaluating Karta files and expressions
pub struct KartaContext {
    /// Heap of all Asts, can be accessed with an AstId
    ast_heap: AstHeap,
    /// Heap of pattern ASTs
    pattern_heap: PatternHeap,
    /// Maps atom string representations to their atom id
    atom_table: AtomTable,
    /// Table of interned symbol names
    symbol_table: SymbolTable,
    /// Table of interned string literals
    string_literal_table: StringLiteralTable,
    /// Table of scope and def relationships
    elab: Elaboration,
    /// Heap, for evaluations
    heap: Heap,
}

enum ProcessKind {
    Expr,
    File,
}

impl KartaContext {
    /// Creates a new Karta Context, or a string is any errors occured
    pub fn new() -> Self {
        Self {
            ast_heap: AstHeap::new(),
            atom_table: AtomTable::with_wellknown(),
            pattern_heap: PatternHeap::new(),
            symbol_table: SymbolTable::new(),
            string_literal_table: StringLiteralTable::new(),
            elab: Elaboration::new(),
            heap: Heap::new(),
            // modules: HashMap::new(),
        }
    }

    pub fn run_file(&mut self, path: &PathBuf) -> Result<String, String> {
        let file_contents: String = fs::read_to_string(path)
            .map_err(|_| {
                // TODO: Maybe the error info is useful?
                KartaError {
                    span: Span { start: 0, end: 0 },
                    kind: ErrorKind::CannotOpenFile {
                        filename: path.clone(),
                    },
                }
            })
            .map_err(|err| format!("{}: error: {}", path.display(), err.kind))?;

        let source = SourceFile::new(file_contents);
        let expr_ast = self
            .frontend(&source, ProcessKind::File)
            .map_err(|err| err.in_source(&source, path).to_string())?;

        const MAIN_SYMBOL_NAME: &str = "main";
        let want_sym_id = self.symbol_table.intern(MAIN_SYMBOL_NAME);
        let want = self
            .elab
            .lookup_root(want_sym_id)
            .ok_or(KartaError {
                span: Span { start: 0, end: 0 },
                kind: ErrorKind::UnresolvedIdentifier {
                    symbol_name: MAIN_SYMBOL_NAME.to_string(),
                },
            })
            .map_err(|err| err.in_source(&source, path).to_string())?;

        let program =
            Lowerer::new(&self.ast_heap, &self.pattern_heap, &self.elab).lower_file(expr_ast, want);

        let eval = Eval::new(
            &mut self.heap,
            &self.string_literal_table,
            &self.atom_table,
            program,
        );
        let res = eval
            .eval()
            .map_err(|err| err.in_source(&source, path).to_string())?;
        Ok(format!("{res}"))
    }

    /// Constructs a new query from an expression, to be evaluated within the context constructed so far
    pub fn eval(&'_ mut self, expr_str: impl ToString) -> Result<ValueRef<'_>, KartaError> {
        let source = SourceFile::new(expr_str.to_string());
        let expr_ast = self.frontend(&source, ProcessKind::Expr)?;
        let program =
            Lowerer::new(&self.ast_heap, &self.pattern_heap, &self.elab).lower_expr(expr_ast);
        let eval = Eval::new(
            &mut self.heap,
            &self.string_literal_table,
            &self.atom_table,
            program,
        );
        eval.eval()
    }

    fn frontend(
        &mut self,
        source: &SourceFile,
        process_kind: ProcessKind,
    ) -> Result<AstId, KartaError> {
        let mut tokenizer = Tokenizer::new(source);
        let mut raw_tokens = vec![];
        tokenizer.tokenize(&mut raw_tokens)?;
        let tokens = layout::layout(&raw_tokens);

        let parser = Parser::new(
            source,
            &tokens,
            &mut self.ast_heap,
            &mut self.pattern_heap,
            &mut self.symbol_table,
            &mut self.string_literal_table,
            &mut self.atom_table,
        );
        let (expr_ast, parse_errors) = match process_kind {
            ProcessKind::Expr => parser.parse_expr(),
            ProcessKind::File => parser.parse_file(),
        };
        if !parse_errors.is_empty() {
            return Err(parse_errors[0].clone());
        }

        let declare = AstWalker::walk(
            &self.ast_heap,
            &self.pattern_heap,
            expr_ast,
            Declare::new(&self.ast_heap, &self.pattern_heap, &mut self.elab),
        )?;
        if !declare.errors().is_empty() {
            return Err(declare.errors()[0].clone());
        }

        let resolve = AstWalker::walk(
            &self.ast_heap,
            &self.pattern_heap,
            expr_ast,
            Resolve::new(&self.ast_heap, &self.symbol_table, &mut self.elab),
        )?;
        if !resolve.errors().is_empty() {
            return Err(resolve.errors()[0].clone());
        }

        Ok(expr_ast)
    }
}

impl Default for KartaContext {
    fn default() -> Self {
        Self::new()
    }
}
