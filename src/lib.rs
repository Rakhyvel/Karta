//! TODO:
//! - [x] Add queries
//! - [x] Move to own crate
//! - [x] Split up into ast.rs, parser.rs, tokenizer.rs, error.rs, query.rs, lib.rs
//! - [x] Add AtomTable
//! - [x] ! Add tests, maybe even unit tests!
//! - [x] ! Implement IntoIterator for lists
//! - [x] ! Add readme with BASIC run down on Karta
//!     - Dynamically typed, haskell-y (lazy) lisp that's also great for data description
//!     - Everything is a map, map get syntax looks like/is indistiguishable from function call
//!     - Predicate-based types
//!     - Open multimethods
//! - [x] ! Implement new file syntax, and true identifier tokenization
//! - [x] ! Add basic operators
//!     - && ||
//!     - == != < <= > >=
//!     - + -
//!     - * / %
//!     - not neg
//!     - terms
//!     - ()
//! - [x] ! Add `let` ... `in`
//! - [x] ! Simplify operators to be prefix only!
//! - [x] ! map get
//! - [x] ! map keys besides atoms
//! - [x] ! tuples
//! - [x] ! Add builtin functions
//! - [x] ! Add lambdas
//! - [x] ! if then else
//! - [x] ! imports
//!     * KartaContext, which contains a string map to module ids, and a vault of the actual modules themselves
//!     * User calls `KartaContext::import(&mut self, module_name, path)` to compile and assign the context
//!     * The modules are added in the root scope of the context, and are assigned to maps of the bindings
//!     * User calls `KartaContext::eval(&mut self, expr)`, using the context's module scope
//! - [x] ! core/prelude
//! - [x] Sets
//! - [ ] `f x y = z` => `f = \x -> \y -> z` (but also store the methods and their arity for overloading)
//! - [ ] list patterns
//! - [ ] tuple patterns
//! - [ ] map & set patterns
//! - [ ] unions, intersection, difference
//! - [ ] Extend union, difference, intersection operators to all functors
//! - [ ] Add type predicate matching
//! - [ ] Add `match` ... `with`
//! - [ ] Laziness
//! - [ ] Add multi-method overloads
//! - [ ] Implement REPL
//! - [ ] String interpolation
//! - [ ] Add `where`
//! - [ ] `$` for parens until end of line

pub mod ast;
mod debug;
mod elaborate;
mod eval;
mod interner;
mod ir;
mod layout;
mod parser;
mod pattern;
mod scope;
mod source;
mod span;
mod tokenizer;
mod walker;

use std::{
    fs,
    sync::{Arc, Mutex},
};

use ast::AstHeap;
use parser::Parser;

use crate::{
    debug::TreePrint,
    elaborate::{Declare, Elaboration, Resolve},
    eval::Eval,
    interner::{AtomTable, StringLiteralTable, SymbolTable},
    ir::{Lowerer, Value},
    pattern::PatternHeap,
    source::SourceFile,
    walker::AstWalker,
};

/// Represents the context for evaluating Karta files and expressions
pub struct KartaContext {
    /// Heap of all Asts, can be accessed with an AstId
    ast_heap: Arc<Mutex<AstHeap>>,
    /// Heap of pattern ASTs
    pattern_heap: Arc<Mutex<PatternHeap>>,
    /// Maps atom string representations to their atom id
    atom_table: Arc<Mutex<AtomTable>>,
    /// Table of interned symbol names
    symbol_table: Arc<Mutex<SymbolTable>>,
    /// Table of interned string literals
    string_literal_table: Arc<Mutex<StringLiteralTable>>,
    /// Table of scope and def relationships
    elab: Arc<Mutex<Elaboration>>,
}

impl KartaContext {
    /// Creates a new Karta Context, or a string is any errors occured
    pub fn new() -> Result<Self, String> {
        Ok(Self {
            ast_heap: Arc::new(Mutex::new(AstHeap::new())),
            atom_table: Arc::new(Mutex::new(AtomTable::new())),
            pattern_heap: Arc::new(Mutex::new(PatternHeap::new())),
            symbol_table: Arc::new(Mutex::new(SymbolTable::new())),
            string_literal_table: Arc::new(Mutex::new(StringLiteralTable::new())),
            elab: Arc::new(Mutex::new(Elaboration::new())),
            // modules: HashMap::new(),
        })
    }

    /// Amends a module with the bindings in a file
    pub fn import_file(
        &mut self,
        module_name: impl ToString,
        filename: impl ToString,
    ) -> Result<(), String> {
        let file_contents: String = match fs::read_to_string(filename.to_string()) {
            Ok(c) => c,
            Err(x) => return Err(x.to_string()),
        };
        self.import(module_name, file_contents)
    }

    /// Amends a module with the bindings in a string
    pub fn import(
        &mut self,
        _module_name: impl ToString,
        file_contents: impl ToString,
    ) -> Result<(), String> {
        let source = SourceFile::new(file_contents.to_string());

        let mut pattern_heap = self.pattern_heap.try_lock().unwrap();
        let mut string_literal_table = self.string_literal_table.try_lock().unwrap();
        let mut ast_heap = self.ast_heap.try_lock().unwrap();
        let mut atoms_table = self.atom_table.try_lock().unwrap();
        let mut symbol_table = self.symbol_table.try_lock().unwrap();

        let mut parser = Parser::new(
            &source,
            &mut ast_heap,
            &mut pattern_heap,
            &mut symbol_table,
            &mut string_literal_table,
            &mut atoms_table,
        );

        let _file_ast = parser.parse_file()?;

        todo!("emplace into the context with a ModuleId")
    }

    /// Constructs a new query from an expression, to be evaluated within the context constructed so far
    pub fn eval(&self, expr_str: impl ToString) -> Result<Value, String> {
        let source = SourceFile::new(expr_str.to_string());
        let mut pattern_heap = self.pattern_heap.try_lock().unwrap();
        let mut string_literal_table = self.string_literal_table.try_lock().unwrap();
        let mut ast_heap = self.ast_heap.try_lock().unwrap();
        let mut atoms_table = self.atom_table.try_lock().unwrap();
        let mut symbol_table = self.symbol_table.try_lock().unwrap();
        let mut elab = self.elab.try_lock().unwrap();

        let mut parser = Parser::new(
            &source,
            &mut ast_heap,
            &mut pattern_heap,
            &mut symbol_table,
            &mut string_literal_table,
            &mut atoms_table,
        );
        let expr_ast = parser.parse_expr()?;

        println!("The parsed AST:");
        AstWalker::walk(
            &ast_heap,
            &pattern_heap,
            expr_ast,
            TreePrint::new(&ast_heap, &pattern_heap),
        )?;

        AstWalker::walk(
            &ast_heap,
            &pattern_heap,
            expr_ast,
            Declare::new(&ast_heap, &pattern_heap, &mut elab),
        )?;

        AstWalker::walk(
            &ast_heap,
            &pattern_heap,
            expr_ast,
            Resolve::new(&ast_heap, &pattern_heap, &mut elab),
        )?;

        println!("\n");
        elab.debug();

        let code = Lowerer::new(&ast_heap, &elab).lower(expr_ast);
        println!("\n");
        code.debug();

        Ok(Eval::new(code).eval())
    }
}

mod tests {
    #[cfg(test)]
    use super::*;

    #[test]
    fn basic_variable() -> Result<(), String> {
        let karta_context = KartaContext::new()?;

        let res: i64 = karta_context.eval("let x = 100 in x")?.as_int()?;

        assert_eq!(res, 100);
        Ok(())
    }

    #[test]
    fn basic_variable_float() -> Result<(), String> {
        let karta_context = KartaContext::new()?;

        let res: f64 = karta_context.eval("let x = 100.0 in x")?.as_float()?;

        assert_eq!(res, 100.0);
        Ok(())
    }

    #[test]
    fn get_map_int() -> Result<(), String> {
        let karta_context = KartaContext::new()?;

        let res: i64 = karta_context
            .eval("let test = {.test-atom = 4} in test.test-atom")?
            .as_int()?;

        assert_eq!(res, 4);
        Ok(())
    }

    #[test]
    fn get_map_floats() -> Result<(), String> {
        let karta_context = KartaContext::new()?;

        let res: f64 = karta_context
            .eval("let test = {.test-atom = 4.5} in test.test-atom")?
            .as_float()?;

        assert_eq!(res, 4.5);
        Ok(())
    }

    #[test]
    fn builtin_functions_operators() -> Result<(), String> {
        let karta_context = KartaContext::new()?;

        let res: i64 = karta_context.eval("@add (19, 4)")?.as_int()?;

        assert_eq!(res, 23);
        Ok(())
    }

    #[test]
    fn let_in_multiple_lines() -> Result<(), String> {
        let karta_context = KartaContext::new()?;

        let res: i64 = karta_context
            .eval(
                r#"let
  x = 4
  y = 5
in (@add (x, y))
"#,
            )?
            .as_int()?;

        assert_eq!(res, 9);
        Ok(())
    }

    #[test]
    fn integer_map_keys() -> Result<(), String> {
        let kctx = KartaContext::new()?;

        let res: i64 = kctx.eval("{0 = 23} 0")?.as_int()?;

        assert_eq!(res, 23);
        Ok(())
    }

    #[test]
    fn tuples() -> Result<(), String> {
        let kctx = KartaContext::new()?;

        let res: i64 = kctx.eval("(1, 2, 3, 4) 2")?.as_int()?;

        assert_eq!(res, 3);
        Ok(())
    }

    #[test]
    fn lambdas() -> Result<(), String> {
        let kctx = KartaContext::new()?;

        let res: i64 = kctx.eval("(\\x -> @add(x, 4)) 5")?.as_int()?;

        assert_eq!(res, 9);
        Ok(())
    }

    #[test]
    fn curry() -> Result<(), String> {
        let kctx = KartaContext::new()?;

        let res: i64 = kctx
            .eval("let + = \\x -> \\y -> @add (x, y) in (+ 5 4)")?
            .as_int()?;

        assert_eq!(res, 9);
        Ok(())
    }

    #[test]
    fn function_def_1_arg() -> Result<(), String> {
        let kctx = KartaContext::new()?;

        let res: i64 = kctx
            .eval("let double x = @mul(x, 2) in double 4")?
            .as_int()?;

        assert_eq!(res, 8);
        Ok(())
    }

    #[test]
    fn function_def_2_args() -> Result<(), String> {
        let kctx = KartaContext::new()?;

        let res: i64 = kctx
            .eval("let my-add x y = @add(x, y) in my-add 15 95")?
            .as_int()?;

        assert_eq!(res, 110);
        Ok(())
    }

    #[test]
    fn if_then_else() -> Result<(), String> {
        let kctx = KartaContext::new()?;

        let res: i64 = kctx.eval("let safe-div = \\x -> \\y -> if @eql(y, 0) then .inf else @div(x, y) in (safe-div 100 4)")?.as_int()?;

        assert_eq!(res, 25);
        Ok(())
    }

    #[test]
    fn elif_then_else() -> Result<(), String> {
        let kctx = KartaContext::new()?;

        let res: i64 = kctx
            .eval("let x = 4 in if @eql(x, 0) then 0 elif @eql(x, 4) then 25 else 45")?
            .as_int()?;

        assert_eq!(res, 25);
        Ok(())
    }

    #[test]
    fn import() -> Result<(), String> {
        let mut kctx = KartaContext::new()?;

        kctx.import("test", "x = 100")?;
        let res: i64 = kctx.eval("test.x")?.as_int()?;

        assert_eq!(res, 100);
        Ok(())
    }

    #[test]
    fn import_amend() -> Result<(), String> {
        let mut kctx = KartaContext::new()?;

        kctx.import("test", "x = 100")?;
        kctx.import("test", "y = 10")?;
        let res: i64 = kctx.eval("@add (test.x, test.y)")?.as_int()?;

        assert_eq!(res, 110);
        Ok(())
    }

    #[test]
    fn import_core() -> Result<(), String> {
        let mut kctx = KartaContext::new()?;

        kctx.import_file("core", "core/core.k")?;
        let res: i64 = kctx.eval("core.+ 65 45")?.as_int()?;

        assert_eq!(res, 110);
        Ok(())
    }

    #[test]
    fn list_empty_pattern_match() -> Result<(), String> {
        let kctx = KartaContext::new()?;

        let res: i64 = kctx
            .eval(
                r#"let
  test [] = 111
in (test [1, 2, 3])
"#,
            )?
            .as_int()?;

        assert_eq!(res, 111);
        Ok(())
    }

    //     #[test]
    //     fn get_map_string() -> Result<(), String> {
    //         let karta_context = KartaContext::new()?;

    //         let binding =
    //             karta_context.eval("let test = {.test-atom = \"Hello, World!\"} in test.test-atom")?;
    //         let res = binding.as_string()?;

    //         assert_eq!(res, "Hello, World!");
    //         Ok(())
    //     }

    //     #[test]
    //     fn truthy_falsey() -> Result<(), String> {
    //         let karta_context = KartaContext::new()?;

    //         let test_atom1 = karta_context.eval(".t")?.truthy()?;
    //         let test_atom2 = karta_context.eval(".nil")?.truthy()?;

    //         assert!(test_atom1);
    //         assert!(!test_atom2);
    //         Ok(())
    //     }

    //     #[test]
    //     fn list_iterator() -> Result<(), String> {
    //         let karta_context = KartaContext::new()?;

    //         let mut counter: i64 = 1;
    //         for elem in karta_context.eval("[1, 2, 3]")? {
    //             assert_eq!(counter, elem.as_int::<i64>()?);
    //             counter += 1;
    //         }

    //         Ok(())
    //     }

    //     #[test]
    //     fn double_list_iterator() -> Result<(), String> {
    //         let karta_context = KartaContext::new()?;

    //         let mut counter: i64 = 1;
    //         for elem in karta_context.eval("[[1, 2, 3], [4, 5, 6], [7, 8, 9]]")? {
    //             for elem2 in elem {
    //                 assert_eq!(counter, elem2.as_int::<i64>()?);
    //                 counter += 1;
    //             }
    //         }

    //         Ok(())
    //     }

    //     #[test]
    //     fn set() -> Result<(), String> {
    //         let kctx = KartaContext::new()?;

    //         let res: bool = kctx.eval("{0, 1, 2, 3} 2")?.truthy()?;

    //         assert!(res);
    //         Ok(())
    //     }

    //     #[test]
    //     fn integer_pattern_match() -> Result<(), String> {
    //         let kctx = KartaContext::new()?;

    //         let res = kctx
    //             .eval(
    //                 r#"let
    //   even? 0 = .t
    //   even? 1 = .f
    //   even? n =
    //     if @lsr(n, 0)
    //     then even? (@neg n)
    //     else @sub(n, 2)
    // in even? (@neg 4)
    // "#,
    //             )?
    //             .truthy()?;

    //         assert!(res);
    //         Ok(())
    //     }
}
