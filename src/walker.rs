use crate::{
    ast::{Ast, AstHeap, AstId},
    pattern::{Pattern, PatternHeap, PatternId},
};

pub struct AstWalker<'a, V> {
    asts: &'a AstHeap,
    patterns: &'a PatternHeap,
    visitor: V,
}

pub trait AstVisitor {
    type Error;

    fn enter_ast(&mut self, _id: AstId) -> Result<(), Self::Error> {
        Ok(())
    }

    fn leave_ast(&mut self, _id: AstId) -> Result<(), Self::Error> {
        Ok(())
    }

    fn enter_pattern(&mut self, _id: PatternId) -> Result<(), Self::Error> {
        Ok(())
    }

    fn leave_pattern(&mut self, _id: PatternId) -> Result<(), Self::Error> {
        Ok(())
    }
}

impl<'a, V> AstWalker<'a, V>
where
    V: AstVisitor,
{
    pub fn walk(
        asts: &'a AstHeap,
        patterns: &'a PatternHeap,
        root: AstId,
        visitor: V,
    ) -> Result<V, V::Error> {
        let mut walker = Self {
            asts,
            patterns,
            visitor,
        };
        walker.walk_ast(root)?;
        Ok(walker.visitor)
    }

    fn walk_ast(&mut self, id: AstId) -> Result<(), V::Error> {
        self.visitor.enter_ast(id)?;

        let ast = self.asts.get(id).unwrap();

        match ast {
            Ast::Int(_)
            | Ast::Float(_)
            | Ast::Identifier(_)
            | Ast::BuiltinFunction(_)
            | Ast::Char(_)
            | Ast::Atom(_)
            | Ast::String(_)
            | Ast::Error => {}

            Ast::File(bindings) => {
                for binding in bindings {
                    self.walk_ast(*binding)?;
                }
            }

            Ast::Binding {
                params, rhs, guard, ..
            } => {
                for param in params {
                    self.walk_pattern(*param)?;
                }
                if let Some(guard) = guard {
                    self.walk_ast(*guard)?;
                }
                self.walk_ast(*rhs)?;
            }

            Ast::Lambda { arg, body } => {
                self.walk_pattern(*arg)?;
                self.walk_ast(*body)?;
            }

            Ast::Tuple(xs) | Ast::List(xs) => {
                for child in xs {
                    self.walk_ast(*child)?
                }
            }

            Ast::Map(xs) => {
                for (lhs, rhs) in xs {
                    self.walk_ast(*lhs)?;
                    self.walk_ast(*rhs)?;
                }
            }

            Ast::Apply(lhs, rhs) => {
                self.walk_ast(*lhs)?;
                self.walk_ast(*rhs)?;
            }

            Ast::Let(xs, in_expr) => {
                for child in xs {
                    self.walk_ast(*child)?;
                }
                self.walk_ast(*in_expr)?;
            }

            Ast::If(conds, else_expr) => {
                for (cond, expr) in conds {
                    self.walk_ast(*cond)?;
                    self.walk_ast(*expr)?;
                }
                self.walk_ast(*else_expr)?;
            }
        }

        self.visitor.leave_ast(id)?;

        Ok(())
    }

    fn walk_pattern(&mut self, id: PatternId) -> Result<(), V::Error> {
        self.visitor.enter_pattern(id)?;

        let pattern = self.patterns.get(id).unwrap();

        match pattern {
            Pattern::Wildcard
            | Pattern::Identifier(_)
            | Pattern::Int(_)
            | Pattern::Char(_)
            | Pattern::Atom(_) => {}

            Pattern::Map(patterns) => {
                for (key, value) in patterns {
                    self.walk_pattern(*key)?;
                    if let Some(value) = value {
                        self.walk_pattern(*value)?;
                    }
                }
            }

            Pattern::Tuple(elems) => {
                for elem in elems {
                    self.walk_pattern(*elem)?;
                }
            }
        }

        self.visitor.leave_pattern(id)?;

        Ok(())
    }
}
