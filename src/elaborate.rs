use std::collections::HashMap;

use crate::{
    ast::{Ast, AstHeap, AstId},
    error::{ErrorKind, KartaError},
    interner::SymbolTable,
    pattern::{Pattern, PatternHeap, PatternId},
    scope::{DefArena, DefId, DefKind, Definition, ScopeArena, ScopeId},
    walker::AstVisitor,
};

#[derive(Debug)]
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

    /// Get the DefId that `ast` defines
    pub fn define(&self, ast: AstId) -> DefId {
        *self.defines.get(&ast).unwrap()
    }

    pub fn pattern_define(&self, id: PatternId) -> Option<DefId> {
        self.pattern_defines.get(&id).copied()
    }

    pub fn refer(&self, ast: AstId) -> DefId {
        *self.references.get(&ast).unwrap()
    }

    pub fn defines(&self) -> &HashMap<AstId, DefId> {
        &self.defines
    }

    pub fn pattern_defines(&self) -> &HashMap<PatternId, DefId> {
        &self.pattern_defines
    }

    pub fn references(&self) -> &HashMap<AstId, DefId> {
        &self.references
    }

    pub fn def(&self, def: DefId) -> &Definition {
        self.defs.get(def)
    }

    pub fn def_mut(&mut self, def: DefId) -> &mut Definition {
        self.defs.get_mut(def)
    }
}

pub struct Declare<'a> {
    asts: &'a AstHeap,
    patterns: &'a PatternHeap,
    elab: &'a mut Elaboration,

    scope_stack: Vec<ScopeId>,
    errors: Vec<KartaError>,
    last_declared: Option<(ScopeId, DefId)>,
}

impl<'a> Declare<'a> {
    pub fn new(asts: &'a AstHeap, patterns: &'a PatternHeap, elab: &'a mut Elaboration) -> Self {
        let root_scope_id = elab.scopes.new_scope(None);

        Self {
            asts,
            patterns,
            elab,
            scope_stack: vec![root_scope_id],
            errors: vec![],
            last_declared: None,
        }
    }

    pub fn errors(&self) -> &[KartaError] {
        &self.errors
    }
}

fn opens_scope(ast: &Ast) -> bool {
    matches!(ast, Ast::Let(..) | Ast::Lambda { .. } | Ast::Binding { .. })
}

impl<'a> AstVisitor for Declare<'a> {
    type Error = KartaError;

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
        if let Ast::Binding { name, params, .. } = ast {
            let def_id = match self.elab.scopes.lookup_ident_local(*name, this_scope_id) {
                Some(def_id) => {
                    // Check adjacency
                    if let Some((last_scope, last_def)) = self.last_declared {
                        if last_scope == this_scope_id && last_def != def_id {
                            // Non-adjacent, an err
                            self.errors.push(KartaError {
                                span: self.asts.span(id),
                                kind: ErrorKind::DivisionByZero, // TODO: Unique error
                            });
                        }
                    }

                    let definition = self.elab.def_mut(def_id);
                    let def_arity = definition.arity();
                    if def_arity == 0 {
                        // Redefinition, always an err
                        self.errors.push(KartaError {
                            span: self.asts.span(id),
                            kind: ErrorKind::DivisionByZero, // TODO: Unique error
                        });
                    } else if def_arity != params.len() as u32 {
                        // Mismatched arity, an err
                        self.errors.push(KartaError {
                            span: self.asts.span(id),
                            kind: ErrorKind::DivisionByZero, // TODO: Unique error
                        });
                    } else {
                        // All good, add the clause
                        definition.push_clause(id);
                    }
                    def_id
                }
                None => {
                    let def_id =
                        self.elab
                            .defs
                            .create_def(params.len() as u32, DefKind::Function, Some(id));
                    self.elab.scopes.insert(this_scope_id, *name, def_id);
                    def_id
                }
            };

            self.elab.defines.insert(id, def_id);
            self.last_declared = Some((this_scope_id, def_id));
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
            Pattern::Int(_) | Pattern::Char(_) | Pattern::Atom(_) | Pattern::Map(_) => {}

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
    errors: Vec<KartaError>,
}

impl<'a> Resolve<'a> {
    pub fn new(asts: &'a AstHeap, symbols: &'a SymbolTable, elab: &'a mut Elaboration) -> Self {
        Self {
            asts,
            symbols,
            elab,
            errors: vec![],
        }
    }

    pub fn errors(&self) -> &[KartaError] {
        &self.errors
    }
}

impl<'a> AstVisitor for Resolve<'a> {
    type Error = KartaError;

    fn enter_ast(&mut self, id: AstId) -> Result<(), Self::Error> {
        let ast = self.asts.get(id).expect("got an invalid AST id");

        if let Ast::Identifier(sym) = ast {
            let ast_scope_id = *self
                .elab
                .ast_scopes
                .get(&id)
                .expect("should've been scoped during Declare");

            match self.elab.scopes.lookup_ident(*sym, ast_scope_id) {
                Some(def_id) => {
                    self.elab.references.insert(id, def_id);
                }
                None => self.errors.push(KartaError {
                    span: self.asts.span(id),
                    kind: ErrorKind::UnresolvedIdentifier {
                        symbol_name: String::from(self.symbols.get(*sym)),
                    },
                }),
            };
        }

        Ok(())
    }
}
