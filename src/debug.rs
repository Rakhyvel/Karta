use crate::{
    ast::{AstHeap, AstId},
    pattern::{PatternHeap, PatternId},
    walker::AstVisitor,
};

pub struct TreePrint<'a> {
    asts: &'a AstHeap,
    patterns: &'a PatternHeap,
    indent: usize,
}

impl<'a> TreePrint<'a> {
    const INDENT_WIDTH: usize = 4;

    pub fn new(asts: &'a AstHeap, patterns: &'a PatternHeap) -> Self {
        Self {
            asts,
            patterns,
            indent: 0,
        }
    }
}

impl<'a> AstVisitor for TreePrint<'a> {
    type Error = String;

    fn enter_ast(&mut self, id: AstId) -> Result<(), Self::Error> {
        for _ in 0..self.indent * Self::INDENT_WIDTH {
            print!(" ")
        }

        let ast = self.asts.get(id).expect("got an invalid AST id");
        println!("{ast:?}");

        self.indent += 1;

        Ok(())
    }

    fn leave_ast(&mut self, _id: AstId) -> Result<(), Self::Error> {
        self.indent -= 1;
        Ok(())
    }

    fn enter_pattern(&mut self, id: PatternId) -> Result<(), Self::Error> {
        for _ in 0..self.indent * Self::INDENT_WIDTH {
            print!(" ")
        }

        let pattern = self.patterns.get(id).expect("got an invalid pattern id");
        println!("{pattern:?}");

        self.indent += 1;

        Ok(())
    }

    fn leave_pattern(&mut self, _id: PatternId) -> Result<(), Self::Error> {
        self.indent -= 1;
        Ok(())
    }
}
