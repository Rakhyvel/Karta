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

use std::fs;

use ast::AstHeap;
use parser::Parser;

use crate::{
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

    /// Amends a module with the bindings in a file
    pub fn import_file(
        &mut self,
        module_name: impl ToString,
        filename: impl ToString,
    ) -> Result<(), KartaError> {
        let file_contents: String = match fs::read_to_string(filename.to_string()) {
            Ok(c) => c,
            Err(_) => {
                // TODO: Maybe the error info is useful?
                return Err(KartaError {
                    span: Span { start: 0, end: 0 },
                    kind: ErrorKind::CannotOpenFile {
                        filename: filename.to_string(),
                    },
                });
            }
        };
        self.import(module_name, file_contents)
    }

    /// Amends a module with the bindings in a string
    pub fn import(
        &mut self,
        _module_name: impl ToString,
        file_contents: impl ToString,
    ) -> Result<(), KartaError> {
        let source = SourceFile::new(file_contents.to_string());

        let mut tokenizer = Tokenizer::new(&source);
        let mut old_tokens = vec![];
        tokenizer.tokenize(&mut old_tokens)?;
        let tokens = layout::layout(&old_tokens);

        let parser = Parser::new(
            &source,
            &tokens,
            &mut self.ast_heap,
            &mut self.pattern_heap,
            &mut self.symbol_table,
            &mut self.string_literal_table,
            &mut self.atom_table,
        );

        let _file_ast = parser.parse_file();

        todo!("emplace into the context with a ModuleId")
    }

    /// Constructs a new query from an expression, to be evaluated within the context constructed so far
    pub fn eval(&'_ mut self, expr_str: impl ToString) -> Result<ValueRef<'_>, KartaError> {
        let source = SourceFile::new(expr_str.to_string());

        let mut tokenizer = Tokenizer::new(&source);
        let mut old_tokens = vec![];
        tokenizer.tokenize(&mut old_tokens)?;
        let tokens = layout::layout(&old_tokens);

        let parser = Parser::new(
            &source,
            &tokens,
            &mut self.ast_heap,
            &mut self.pattern_heap,
            &mut self.symbol_table,
            &mut self.string_literal_table,
            &mut self.atom_table,
        );
        let (expr_ast, parse_errors) = parser.parse_expr();
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

        let program = Lowerer::new(&self.ast_heap, &self.pattern_heap, &self.elab).lower(expr_ast);

        let eval = Eval::new(&mut self.heap, &self.string_literal_table, program);
        eval.eval()
    }
}

impl Default for KartaContext {
    fn default() -> Self {
        Self::new()
    }
}

mod tests {
    #[cfg(test)]
    use super::*;

    #[test]
    fn basic_variable() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: i64 = karta_context.eval("let x = 100 in x")?.as_i64().unwrap();

        assert_eq!(res, 100);
        Ok(())
    }

    #[test]
    fn basic_variable_float() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: f64 = karta_context.eval("let x = 100.0 in x")?.as_f64().unwrap();

        assert_eq!(res, 100.0);
        Ok(())
    }

    #[test]
    fn let_in_let_in() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: f64 = karta_context
            .eval("let x = 100.0 in let y = 200.0 in @add(x, y)")?
            .as_f64()
            .unwrap();

        assert_eq!(res, 300.0);
        Ok(())
    }

    #[test]
    fn empty_map() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: f64 = karta_context
            .eval("if {} then 100.0 else 300.0")?
            .as_f64()
            .unwrap();

        assert_eq!(res, 300.0);
        Ok(())
    }

    #[test]
    fn get_map_int() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: i64 = karta_context
            .eval("let test = {.test-atom = 4} in test.test-atom")?
            .as_i64()
            .unwrap();

        assert_eq!(res, 4);
        Ok(())
    }

    #[test]
    fn get_map_floats() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: f64 = karta_context
            .eval("let test = {.test-atom = 4.5} in test.test-atom")?
            .as_f64()
            .unwrap();

        assert_eq!(res, 4.5);
        Ok(())
    }

    #[test]
    fn get_map_char() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res = karta_context
            .eval("let test = {.test-atom = 'H'} in test.test-atom")?
            .as_char()
            .unwrap();

        assert_eq!(res, 'H');
        Ok(())
    }

    #[test]
    fn get_map_string() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res = karta_context
            .eval("let test = {.test-atom = \"Hello, World!\"} in test.test-atom")?
            .as_string()?;

        assert_eq!(res, "Hello, World!");
        Ok(())
    }

    #[test]
    fn map_trailing_comma() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: f64 = karta_context
            .eval("let test = {.test-atom = 4.5, .test-atom-2 = 5.4,} in test.test-atom-2")?
            .as_f64()
            .unwrap();

        assert_eq!(res, 5.4);
        Ok(())
    }

    #[test]
    fn integer_map_keys() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: i64 = karta_context.eval("{0 = 23} 0")?.as_i64().unwrap();

        assert_eq!(res, 23);
        Ok(())
    }

    #[test]
    fn map_multiple_lines() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: i64 = karta_context
            .eval(
                r#"let
  p = { .a = 1
      , .b = 2}
in p .b
"#,
            )?
            .as_i64()
            .unwrap();

        assert_eq!(res, 2);
        Ok(())
    }

    #[test]
    fn builtin_functions_operators() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: i64 = karta_context.eval("@add (19, 4)")?.as_i64().unwrap();

        assert_eq!(res, 23);
        Ok(())
    }

    #[test]
    fn let_in_multiple_lines() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: i64 = karta_context
            .eval(
                r#"let
  x = 4
  y = 5
in (@add (x, y))
"#,
            )?
            .as_i64()
            .unwrap();

        assert_eq!(res, 9);
        Ok(())
    }

    #[test]
    fn let_in_multiple_newlines() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: i64 = karta_context
            .eval(
                r#"let
  x = 4
  y = 5


in (@add (x, y))
"#,
            )?
            .as_i64()
            .unwrap();

        assert_eq!(res, 9);
        Ok(())
    }

    #[test]
    fn let_in_preceding_comment() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: i64 = karta_context
            .eval(
                r#"let
  ; this is a preceding comment
  x = 4
  y = 5
in (@add (x, y))
"#,
            )?
            .as_i64()
            .unwrap();

        assert_eq!(res, 9);
        Ok(())
    }

    #[test]
    fn let_in_own_line() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: i64 = karta_context
            .eval(
                r#"let
  x = 4
  y = 5
in 
    (@add (x, y))
"#,
            )?
            .as_i64()
            .unwrap();

        assert_eq!(res, 9);
        Ok(())
    }

    #[test]
    fn tuples() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: i64 = karta_context.eval("(1, 2, 3, 4) 2")?.as_i64().unwrap();

        assert_eq!(res, 3);
        Ok(())
    }

    #[test]
    fn lambdas() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: i64 = karta_context
            .eval("(\\x -> @add(x, 4)) 5")?
            .as_i64()
            .unwrap();

        assert_eq!(res, 9);
        Ok(())
    }

    #[test]
    fn curry() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: i64 = karta_context
            .eval("let + = \\x -> \\y -> @add (x, y) in (+ 5 4)")?
            .as_i64()
            .unwrap();

        assert_eq!(res, 9);
        Ok(())
    }

    #[test]
    fn function_def_1_arg() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: i64 = karta_context
            .eval("let double x = @mul(x, 2) in double 4")?
            .as_i64()
            .unwrap();

        assert_eq!(res, 8);
        Ok(())
    }

    #[test]
    fn function_def_2_args() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: i64 = karta_context
            .eval("let my-add x y = @add(x, y) in my-add 15 95")?
            .as_i64()
            .unwrap();

        assert_eq!(res, 110);
        Ok(())
    }

    #[test]
    fn if_then_else() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: i64 = karta_context.eval("let safe-div = \\x -> \\y -> if @eql(y, 0) then .inf else @div(x, y) in (safe-div 100 4)")?.as_i64().unwrap();

        assert_eq!(res, 25);
        Ok(())
    }

    #[test]
    fn elif_then_else() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: i64 = karta_context
            .eval("let x = 4 in if @eql(x, 0) then 0 elif @eql(x, 4) then 25 else 45")?
            .as_i64()
            .unwrap();

        assert_eq!(res, 25);
        Ok(())
    }

    #[test]
    fn truthy_falsey() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let test_atom1 = karta_context.eval(".t")?.is_truthy();
        let test_atom2 = karta_context.eval("{}")?.is_truthy();

        assert!(test_atom1);
        assert!(!test_atom2);
        Ok(())
    }

    #[test]
    fn set() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: bool = karta_context.eval("{0, 1, 2, 3} 2")?.is_truthy();

        assert!(res);
        Ok(())
    }

    #[test]
    fn recursion() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res = karta_context
            .eval("let fact x = if @eql(x, 1) then x else @mul(fact (@sub(x, 1)), x) in fact 5")?
            .as_i64()
            .unwrap();

        assert_eq!(res, 120);
        Ok(())
    }

    #[test]
    fn integer_pattern_match() -> Result<(), KartaError> {
        let src = r#"let
    even? 0 = 0
    even? 1 = 1
    even? n = even? (@sub(n, 2))
in even? "#;

        for (input, expected) in [(0, 0), (1, 1), (4, 0), (5, 1), (10, 0)] {
            let mut kctx = KartaContext::new();

            let res = kctx.eval(format!("{src}{input}"))?.as_i64().unwrap();

            assert_eq!(res, expected);
        }

        Ok(())
    }

    #[test]
    fn char_pattern_match() -> Result<(), KartaError> {
        let src = r#"let
    eval 'a' = 100
    eval 'b' = 200
    eval n = 300
in eval "#;

        for (input, expected) in [
            ("'a'", 100),
            ("'b'", 200),
            ("'c'", 300),
            ("0", 300),
            ("4", 300),
        ] {
            let mut kctx = KartaContext::new();

            let res = kctx.eval(format!("{src}{input}"))?.as_i64().unwrap();

            assert_eq!(res, expected);
        }

        Ok(())
    }

    #[test]
    fn atom_pattern_match() -> Result<(), KartaError> {
        let src = r#"let
    eval .a = 100
    eval .b = 200
    eval n = 300
in eval "#;

        for (input, expected) in [
            (".a", 100),
            (".b", 200),
            (".c", 300),
            ("0", 300),
            ("4", 300),
        ] {
            let mut kctx = KartaContext::new();

            let res = kctx.eval(format!("{src}{input}"))?.as_i64().unwrap();

            assert_eq!(res, expected);
        }

        Ok(())
    }

    #[test]
    fn set_pattern_match() -> Result<(), KartaError> {
        let src = r#"let
    eval {{.d}} = 10
    eval {.a, .b} = 400
    eval {.a} = 100
    eval {.b} = 200
    eval n = 300
in eval "#;

        for (input, expected) in [
            ("{.a}", 100),
            ("{.b, .c}", 200),
            ("{.b, .a}", 400),
            ("{.c}", 300),
            ("{}", 300),
            ("0", 300),
            ("4", 300),
        ] {
            let mut kctx = KartaContext::new();

            let res = kctx.eval(format!("{src}{input}"))?.as_i64().unwrap();

            assert_eq!(res, expected);
        }

        Ok(())
    }

    #[test]
    fn map_pattern_match() -> Result<(), KartaError> {
        let src = r#"let
    eval {.a = 1} = 400
    eval {.a = {.b = x}} = x
    eval {.a = x} = x
    eval n = 300
in eval "#;

        for (input, expected) in [
            ("{.a = 1}", 400),
            ("{.a = 200}", 200),
            ("{.a = {.b = 500}}", 500),
            ("{}", 300),
            ("0", 300),
            ("4", 300),
        ] {
            let mut kctx = KartaContext::new();

            let res = kctx.eval(format!("{src}{input}"))?.as_i64().unwrap();

            assert_eq!(res, expected);
        }

        Ok(())
    }

    #[test]
    fn eq_pattern_match() -> Result<(), KartaError> {
        let src = r#"let
    eq x = {x}
in eq "#;

        for (input, expected) in [
            ("0 0", true),
            ("0 1", false),
            ("0 {.a}", false),
            ("{.a = 1} {.a = 1}", true),
            ("{.a = 1, .b = 2} {.b = 2, .a = 1}", true),
            ("{.a = 1} {.a = 2}", false),
            ("{.a = 1} {.a = 1, .b = 2}", false),
            ("{.a = 1, .b = 2} {.a = 1}", false),
            ("{} {}", true),
            ("{.a = {.b = 1}} {.a = {.b = 1}}", true),
            ("\"hi\" \"hi\"", true),
            ("\"hi\" \"hey\"", false),
        ] {
            let mut kctx = KartaContext::new();

            let res = kctx.eval(format!("{src}{input}"))?.is_truthy();

            assert_eq!(res, expected);
        }

        Ok(())
    }

    #[test]
    fn tuple_pattern_match() -> Result<(), KartaError> {
        let src = r#"let
    eval (0, 0) = 0
    eval (0, x) = x
    eval (1, 0) = 1
    eval (1, 0, 1) = 2
    eval _ = 100
in eval "#;

        for (input, expected) in [
            ("(0, 0)", 0),
            ("(0, 200)", 200),
            ("{0 = 0, 1 = 200}", 200),
            ("(1, 0)", 1),
            ("(1, 0, 1)", 2),
            ("{}", 100),
            ("4", 100),
        ] {
            let mut kctx = KartaContext::new();

            let res = kctx.eval(format!("{src}{input}"))?.as_i64().unwrap();

            assert_eq!(res, expected);
        }

        Ok(())
    }

    #[test]
    fn wildcard_match() -> Result<(), KartaError> {
        let src = r#"let
    eval 0 = 100
    eval {.a = x} = x
    eval _ = 0
in eval "#;

        for (input, expected) in [
            ("{.a = 1}", 1),
            ("{.b = 200}", 0),
            ("{}", 0),
            ("0", 100),
            ("4", 0),
        ] {
            let mut kctx = KartaContext::new();

            let res = kctx.eval(format!("{src}{input}"))?.as_i64().unwrap();

            assert_eq!(res, expected);
        }

        Ok(())
    }

    #[test]
    fn match_guard() -> Result<(), KartaError> {
        let src = r#"let
    eval x when @lsr(x, 5) = 1
    eval _ = 0
in eval "#;

        for (input, expected) in [("0", 1), ("4", 1), ("5", 0), ("50", 0)] {
            let mut kctx = KartaContext::new();

            let res = kctx.eval(format!("{src}{input}"))?.as_i64().unwrap();

            assert_eq!(res, expected);
        }

        Ok(())
    }

    #[test]
    fn accepts() -> Result<(), KartaError> {
        let src = r#"let
    eval 0 = 0
    eval 2 = 0
    eval(x, y) = 2
    eval(x, y, z) = 3

    eval2 x 0 = 0

    my-set = {.a, .b}

in @accepts?"#;

        for (input, expected) in [
            ("(eval, 0)", true),
            ("(eval, 1)", false),
            ("(eval, 2)", true),
            ("(eval, 3)", false),
            ("(eval, (1, 2))", true),
            ("(eval, (1, 2, 3))", true),
            ("(eval, (1, 2, 3, 4))", false),
            ("(eval2, 0)", true),
            ("(eval2 1, 0)", true),
            ("(eval2 1, 1)", false),
            ("(my-set, .a)", true),
            ("(my-set, .b)", true),
            ("(my-set, .c)", false),
        ] {
            let mut kctx = KartaContext::new();

            let res = kctx.eval(format!("{src}{input}"))?.is_truthy();

            assert_eq!(res, expected);
        }

        Ok(())
    }

    #[ignore = "imports not impld yet"]
    #[test]
    fn import() -> Result<(), KartaError> {
        let mut kctx = KartaContext::new();

        kctx.import("test", "x = 100")?;
        let res: i64 = kctx.eval("test.x")?.as_i64().unwrap();

        assert_eq!(res, 100);
        Ok(())
    }

    #[ignore = "imports not impld yet"]
    #[test]
    fn import_amend() -> Result<(), KartaError> {
        let mut kctx = KartaContext::new();

        kctx.import("test", "x = 100")?;
        kctx.import("test", "y = 10")?;
        let res: i64 = kctx.eval("@add (test.x, test.y)")?.as_i64().unwrap();

        assert_eq!(res, 110);
        Ok(())
    }

    #[ignore = "imports not impld yet"]
    #[test]
    fn import_core() -> Result<(), KartaError> {
        let mut kctx = KartaContext::new();

        kctx.import_file("core", "core/core.k")?;
        let res: i64 = kctx.eval("core.+ 65 45")?.as_i64().unwrap();

        assert_eq!(res, 110);
        Ok(())
    }

    #[ignore = "patterns not impld yet"]
    #[test]
    fn list_empty_pattern_match() -> Result<(), KartaError> {
        let mut karta_context = KartaContext::new();

        let res: i64 = karta_context
            .eval(
                r#"let
  test [] = 111
in (test [1, 2, 3])
"#,
            )?
            .as_i64()
            .unwrap();

        assert_eq!(res, 111);
        Ok(())
    }

    //     #[test]
    //     fn list_iterator() -> Result<(), String> {
    //         let mut karta_context = KartaContext::new();

    //         let mut counter: i64 = 1;
    //         for elem in karta_context.eval("[1, 2, 3]")? {
    //             assert_eq!(counter, elem.as_int::<i64>()?);
    //             counter += 1;
    //         }

    //         Ok(())
    //     }

    //     #[test]
    //     fn double_list_iterator() -> Result<(), String> {
    //         let mut karta_context = KartaContext::new();

    //         let mut counter: i64 = 1;
    //         for elem in karta_context.eval("[[1, 2, 3], [4, 5, 6], [7, 8, 9]]")? {
    //             for elem2 in elem {
    //                 assert_eq!(counter, elem2.as_int::<i64>()?);
    //                 counter += 1;
    //             }
    //         }

    //         Ok(())
    //     }
}
