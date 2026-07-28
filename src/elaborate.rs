use std::collections::HashMap;

use crate::{
    ast::AstId,
    scope::{DefId, ScopeArena, ScopeId},
    walker::AstVisitor,
};

pub struct Elaboration {
    scopes: ScopeArena,

    ast_scopes: HashMap<AstId, ScopeId>,
    references: HashMap<AstId, DefId>,
}

struct Declare<'a> {
    scope_stack: Vec<ScopeId>,
    elab: &'a mut Elaboration,
}

impl<'a> AstVisitor for Declare<'a> {
    type Error = ();

    fn enter_ast(&mut self, _id: AstId) -> Result<(), Self::Error> {
        todo!("define defs for bindings, scope ASTs");
    }

    fn leave_ast(&mut self, _id: AstId) -> Result<(), Self::Error> {
        todo!("pop scope stack");
    }

    fn enter_pattern(&mut self, _id: crate::pattern::PatternId) -> Result<(), Self::Error> {
        todo!("define param defs for pattern idents, and scope");
    }

    fn leave_pattern(&mut self, _id: crate::pattern::PatternId) -> Result<(), Self::Error> {
        todo!("pop scope stack");
    }
}

struct Resolve<'a> {
    elab: &'a mut Elaboration,
}

impl<'a> AstVisitor for Resolve<'a> {
    type Error = ();

    fn enter_ast(&mut self, _id: AstId) -> Result<(), Self::Error> {
        todo!("assign DefIDs to identifiers");
    }
}
