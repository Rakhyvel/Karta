use std::collections::HashMap;

use crate::{
    ast::{Ast, AstHeap, AstId},
    interner::SymbolTable,
    pattern::{Pattern, PatternHeap, PatternId},
    scope::{DefArena, DefId, DefKind, ScopeArena, ScopeId},
    walker::AstVisitor,
};

pub struct Elaboration {
    scopes: ScopeArena,
    defs: DefArena,

    /// Maps ASTs to the scopes that they exist within
    ast_scopes: HashMap<AstId, ScopeId>,
    /// Maps ASTs to the Defs that they define
    defines: HashMap<AstId, DefId>,
    /// Maps patterns to the Defs that they define
    pattern_defines: HashMap<PatternId, DefId>,
    /// Maps ASTs to the Defs that they refer to
    references: HashMap<AstId, DefId>,
}

impl Elaboration {
    pub fn new() -> Self {
        Self {
            scopes: ScopeArena::new(),
            defs: DefArena::new(),
            ast_scopes: HashMap::new(),
            defines: HashMap::new(),
            pattern_defines: HashMap::new(),
            references: HashMap::new(),
        }
    }

    pub fn define(&self, ast: AstId) -> DefId {
        *self.defines.get(&ast).unwrap()
    }

    pub fn pattern_define(&self, id: PatternId) -> DefId {
        *self
            .pattern_defines
            .get(&id)
            .expect("pattern ID should map to a valid def")
    }

    pub fn refer(&self, ast: AstId) -> DefId {
        *self.references.get(&ast).unwrap()
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

fn opens_scope(ast: &Ast) -> bool {
    matches!(ast, Ast::Let(..) | Ast::Lambda { .. } | Ast::Binding { .. })
}

impl<'a> AstVisitor for Declare<'a> {
    type Error = String;

    fn enter_ast(&mut self, id: AstId) -> Result<(), Self::Error> {
        let this_scope_id = *self
            .scope_stack
            .last()
            .expect("scope stack shouldn't be empty");

        let ast = self.asts.get(id).expect("got an invalid AST id");

        // If identifier, stamp with the surrounding scope
        if let Ast::Identifier(..) = ast {
            self.elab.ast_scopes.insert(id, this_scope_id);
        }

        // If binding, add the def to the current scope
        if let Ast::Binding { name, params, rhs } = ast {
            let def_id =
                self.elab
                    .defs
                    .create_def(params.len() as u32, DefKind::Function, Some(*rhs));
            self.elab.scopes.insert(this_scope_id, *name, def_id);
            self.elab.defines.insert(id, def_id);
        }

        // If this AST defines a new lexical scope, push it to the stack
        // Do this after Binding so that params dont leak
        if opens_scope(ast) {
            let new_scope = self.elab.scopes.new_scope(Some(this_scope_id));
            self.scope_stack.push(new_scope);
        }

        Ok(())
    }

    fn leave_ast(&mut self, id: AstId) -> Result<(), Self::Error> {
        let ast = self.asts.get(id).expect("got an invalid AST id");

        // If this AST defined a new lexical scope, pop it
        if opens_scope(ast) {
            self.scope_stack.pop();
        }

        Ok(())
    }

    fn enter_pattern(&mut self, id: PatternId) -> Result<(), Self::Error> {
        let this_scope_id = *self
            .scope_stack
            .last()
            .expect("scope stack shouldn't be empty");

        let pattern = self.patterns.get(id).expect("pattern should exist");

        match pattern {
            Pattern::Identifier(name) => {
                let param_def_id = self.elab.defs.create_def(0, DefKind::Parameter, None);
                self.elab.scopes.insert(this_scope_id, *name, param_def_id);
                self.elab.pattern_defines.insert(id, param_def_id);
            }
        }

        Ok(())
    }
}

pub struct Resolve<'a> {
    asts: &'a AstHeap,
    symbols: &'a SymbolTable,
    elab: &'a mut Elaboration,
}

impl<'a> Resolve<'a> {
    pub fn new(asts: &'a AstHeap, symbols: &'a SymbolTable, elab: &'a mut Elaboration) -> Self {
        Self {
            asts,
            symbols,
            elab,
        }
    }
}

impl<'a> AstVisitor for Resolve<'a> {
    type Error = String;

    fn enter_ast(&mut self, id: AstId) -> Result<(), Self::Error> {
        let ast = self.asts.get(id).expect("got an invalid AST id");

        if let Ast::Identifier(sym) = ast {
            let ast_scope_id = *self
                .elab
                .ast_scopes
                .get(&id)
                .expect("should've been scoped during Declare");
            let def_id = self
                .elab
                .scopes
                .lookup_ident(*sym, ast_scope_id)
                .ok_or(format!("undefined reference to {}", self.symbols.get(*sym)))?;
            self.elab.references.insert(id, def_id);
        }

        Ok(())
    }
}
