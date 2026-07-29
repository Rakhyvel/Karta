use std::collections::HashMap;

use crate::{
    ast::{Ast, AstHeap, AstId},
    pattern::{PatternHeap, PatternId},
    scope::{DefArena, DefId, DefKind, ScopeArena, ScopeId},
    walker::AstVisitor,
};

pub struct Elaboration {
    scopes: ScopeArena,
    defs: DefArena,

    ast_scopes: HashMap<AstId, ScopeId>,
    references: HashMap<AstId, DefId>,
}

impl Elaboration {
    pub fn new() -> Self {
        Self {
            scopes: ScopeArena::new(),
            defs: DefArena::new(),
            ast_scopes: HashMap::new(),
            references: HashMap::new(),
        }
    }

    pub fn debug(&self) {
        println!("Scopes:");
        self.scopes.debug();
        println!("\nDefs:");
        self.defs.debug();
    }
}

pub struct Declare<'a> {
    asts: &'a AstHeap,
    patterns: &'a PatternHeap,
    elab: &'a mut Elaboration,

    scope_stack: Vec<ScopeId>,
}

impl<'a> Declare<'a> {
    pub fn new(asts: &'a AstHeap, patterns: &'a PatternHeap, elab: &'a mut Elaboration) -> Self {
        let root_scope_id = elab.scopes.new_scope(None);

        Self {
            asts,
            patterns,
            elab,
            scope_stack: vec![root_scope_id],
        }
    }
}

impl<'a> AstVisitor for Declare<'a> {
    type Error = String;

    fn enter_ast(&mut self, id: AstId) -> Result<(), Self::Error> {
        let this_scope_id = *self
            .scope_stack
            .last()
            .expect("scope stack shouldn't be empty");

        // stamp every single AST that comes through here with the current scope
        self.elab.ast_scopes.insert(id, this_scope_id);

        let ast = self.asts.get(id).expect("got an invalid AST id");
        match ast {
            Ast::Let(_, _) => {
                let new_scope = self.elab.scopes.new_scope(Some(this_scope_id));
                self.scope_stack.push(new_scope)
            }
            Ast::Binding { name, params, rhs } => {
                let def_id =
                    self.elab
                        .defs
                        .create_def(params.len() as u32, DefKind::Function, Some(*rhs));
                self.elab.scopes.insert(this_scope_id, *name, def_id);
            }
            _ => {}
        }

        Ok(())
    }

    fn leave_ast(&mut self, id: AstId) -> Result<(), Self::Error> {
        let ast = self.asts.get(id).expect("got an invalid AST id");

        if let Ast::Let(_, _) = ast {
            _ = self.scope_stack.pop()
        }

        Ok(())
    }

    fn enter_pattern(&mut self, _id: PatternId) -> Result<(), Self::Error> {
        Ok(())
    }

    fn leave_pattern(&mut self, _id: PatternId) -> Result<(), Self::Error> {
        Ok(())
    }
}

pub struct Resolve<'a> {
    elab: &'a mut Elaboration,
}

impl<'a> AstVisitor for Resolve<'a> {
    type Error = ();

    fn enter_ast(&mut self, _id: AstId) -> Result<(), Self::Error> {
        todo!("assign DefIDs to identifiers");
    }
}
