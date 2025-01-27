use std::collections::HashMap;

use crate::{
    ast::{Ast, AstHeap, AstId},
    atom::{AtomId, AtomKind, AtomMap},
    scope::{ScopeId, SymbolTable},
};

impl AstHeap {
    /// Takes in an AST heap & atoms, and a root AstId and returns the evaluated AstId, or an error if something went wrong
    pub(crate) fn eval(
        &mut self,
        root: AstId,
        scope: ScopeId,
        atoms: &mut AtomMap,
        symbols: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let ast = self.get(root).unwrap().clone();

        match ast {
            Ast::Int(_)
            | Ast::Float(_)
            | Ast::Char(_)
            | Ast::String(_)
            | Ast::Atom(_)
            | Ast::BuiltinFunction(_)
            | Ast::File(_)
            | Ast::Closure(_, _, _) => Ok(root),
            Ast::Lambda(arg_name, expr) => Ok(self.create_closure(arg_name, expr, scope)),
            Ast::Map(hash_map) => {
                let mut new_map: HashMap<AtomId, AstId> = HashMap::new();
                for (k, v) in hash_map {
                    new_map.insert(k, self.eval(v, scope, atoms, symbols)?);
                }
                Ok(self.create_map(new_map))
            }
            Ast::Let(new_scope, expr) => self.eval(expr, new_scope, atoms, symbols),
            Ast::Identifier(id) => {
                let mut curr_scope: Option<ScopeId> = Some(scope);
                loop {
                    if let Some(some_curr_scope) = curr_scope {
                        let scope_ref = symbols.get_scope(some_curr_scope);

                        if let Some(ast_id) = scope_ref.get(id) {
                            return self.eval(ast_id, some_curr_scope, atoms, symbols);
                        } else {
                            curr_scope = scope_ref.parent();
                        }
                    } else {
                        return Err(format!(
                            "use of undefined identifier `{}`",
                            atoms.string_from_atom(id).unwrap()
                        ));
                    }
                }
            }
            Ast::Apply(functor_id, arg_id) => {
                let eval_functor_id = self.eval(functor_id, scope, atoms, symbols)?;
                let eval_arg_id = self.eval(arg_id, scope, atoms, symbols)?;
                let functor = self.get(eval_functor_id).unwrap();
                let arg = self.get(eval_arg_id).unwrap();

                match functor {
                    Ast::Map(hash_map) => {
                        self.println_ast_id(&eval_functor_id, atoms);
                        let atom_id = match arg {
                            Ast::Atom(atom_id) => atom_id,
                            Ast::Int(n) => atoms.get(AtomKind::Int(*n)).unwrap(),
                            Ast::Char(c) => atoms.get(AtomKind::Char(*c)).unwrap(),
                            _ => {
                                self.println_ast_id(&eval_functor_id, atoms);
                                self.println_ast_id(&eval_arg_id, atoms);
                                return Err(format!("cannot apply those ^"));
                            }
                        };
                        let value_id = hash_map.get(atom_id).unwrap();
                        Ok(self.eval(*value_id, scope, atoms, symbols)?)
                    }
                    Ast::File(file_scope_id) => {
                        let atom_id = match arg {
                            Ast::Atom(atom_id) => atom_id,
                            _ => {
                                self.println_ast_id(&eval_functor_id, atoms);
                                self.println_ast_id(&eval_arg_id, atoms);
                                return Err(format!("cannot apply those ^"));
                            }
                        };
                        let atom_name = atoms.string_from_atom(*atom_id).unwrap();
                        let value_atom =
                            atoms.put_atoms_in_set(AtomKind::NamedAtom(atom_name[1..].to_string()));
                        let file_scope_id = *file_scope_id;
                        let value_id = self.create_identifier(value_atom);
                        Ok(self.eval(value_id, file_scope_id, atoms, symbols)?)
                    }
                    Ast::BuiltinFunction(atom_id) => {
                        let name = atoms.string_from_atom(*atom_id).unwrap();
                        let bif = self.builtin_function_methods.get(&name).unwrap();
                        bif(self, arg_id, scope, atoms, symbols)
                    }
                    Ast::Closure(arg_name, expr_id, closure_scope) => {
                        let new_scope = symbols.new_scope(Some(*closure_scope));
                        let key = atoms.get(AtomKind::NamedAtom(arg_name.clone())).unwrap();
                        symbols.insert(new_scope, *key, eval_arg_id);
                        self.eval(*expr_id, new_scope, atoms, symbols)
                    }
                    _ => {
                        self.println_ast_id(&eval_functor_id, atoms);
                        return Err(format!("not a functor ^"));
                    }
                }
            }
            Ast::If(cond_id, then_id, else_id) => {
                if self.truthy(cond_id, scope, atoms, symbols)? {
                    self.eval(then_id, scope, atoms, symbols)
                } else {
                    self.eval(else_id, scope, atoms, symbols)
                }
            }
        }
    }

    pub(crate) fn and(
        &mut self,
        tuple_id: AstId,
        scope: ScopeId,
        atoms: &mut AtomMap,
        symbols: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let (lhs_id, rhs_id) = self.get_pair(tuple_id, scope, atoms, symbols)?;
        let value = self.truthy(lhs_id, scope, atoms, symbols)?
            && self.truthy(rhs_id, scope, atoms, symbols)?;
        Ok(self.create_boolean(value))
    }

    pub(crate) fn or(
        &mut self,
        tuple_id: AstId,
        scope: ScopeId,
        atoms: &mut AtomMap,
        symbols: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let (lhs_id, rhs_id) = self.get_pair(tuple_id, scope, atoms, symbols)?;
        let value = self.truthy(lhs_id, scope, atoms, symbols)?
            || self.truthy(rhs_id, scope, atoms, symbols)?;
        Ok(self.create_boolean(value))
    }

    pub(crate) fn not(
        &mut self,
        expr_id: AstId,
        scope: ScopeId,
        atoms: &mut AtomMap,
        symbols: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let value = !self.truthy(expr_id, scope, atoms, symbols)?;
        Ok(self.create_boolean(value))
    }

    pub(crate) fn truthy(
        &mut self,
        ast_id: AstId,
        scope: ScopeId,
        atoms: &mut AtomMap,
        symbols: &mut SymbolTable,
    ) -> Result<bool, String> {
        let evald_id = self.eval(ast_id, scope, atoms, symbols)?;
        let ast = self.get(evald_id).expect("couldn't get Ast for AstId");
        match ast {
            Ast::Atom(x) => Ok(x.as_usize() != self.nil_id.as_usize()),
            _ => Ok(true),
        }
    }

    pub(crate) fn equal(
        &mut self,
        tuple_id: AstId,
        scope: ScopeId,
        atoms: &mut AtomMap,
        symbols: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let (lhs_id, rhs_id) = self.get_pair(tuple_id, scope, atoms, symbols)?;
        let asts = self.binop_eval(lhs_id, rhs_id, scope, atoms, symbols)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_boolean(x == y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_boolean(x == y)),
            (Ast::Atom(x), Ast::Atom(y)) => Ok(self.create_boolean(x == y)),
            (lhs, rhs) => Err(format!("cannot compare {:?} and {:?}", lhs, rhs)),
        }
    }

    pub(crate) fn not_equal(
        &mut self,
        tuple_id: AstId,
        scope: ScopeId,
        atoms: &mut AtomMap,
        symbols: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let (lhs_id, rhs_id) = self.get_pair(tuple_id, scope, atoms, symbols)?;
        let asts = self.binop_eval(lhs_id, rhs_id, scope, atoms, symbols)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_boolean(x != y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_boolean(x != y)),
            (Ast::Atom(x), Ast::Atom(y)) => Ok(self.create_boolean(x != y)),
            (lhs, rhs) => Err(format!("cannot compare {:?} and {:?}", lhs, rhs)),
        }
    }

    pub(crate) fn greater(
        &mut self,
        tuple_id: AstId,
        scope: ScopeId,
        atoms: &mut AtomMap,
        symbols: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let (lhs_id, rhs_id) = self.get_pair(tuple_id, scope, atoms, symbols)?;
        let asts = self.binop_eval(lhs_id, rhs_id, scope, atoms, symbols)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_boolean(x > y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_boolean(x > y)),
            (lhs, rhs) => Err(format!("cannot compare {:?} and {:?}", lhs, rhs)),
        }
    }

    pub(crate) fn lesser(
        &mut self,
        tuple_id: AstId,
        scope: ScopeId,
        atoms: &mut AtomMap,
        symbols: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let (lhs_id, rhs_id) = self.get_pair(tuple_id, scope, atoms, symbols)?;
        let asts = self.binop_eval(lhs_id, rhs_id, scope, atoms, symbols)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_boolean(x < y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_boolean(x < y)),
            (lhs, rhs) => Err(format!("cannot compare {:?} and {:?}", lhs, rhs)),
        }
    }

    pub(crate) fn greater_equal(
        &mut self,
        tuple_id: AstId,
        scope: ScopeId,
        atoms: &mut AtomMap,
        symbols: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let (lhs_id, rhs_id) = self.get_pair(tuple_id, scope, atoms, symbols)?;
        let asts = self.binop_eval(lhs_id, rhs_id, scope, atoms, symbols)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_boolean(x >= y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_boolean(x >= y)),
            (lhs, rhs) => Err(format!("cannot compare {:?} and {:?}", lhs, rhs)),
        }
    }

    pub(crate) fn lesser_equal(
        &mut self,
        tuple_id: AstId,
        scope: ScopeId,
        atoms: &mut AtomMap,
        symbols: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let (lhs_id, rhs_id) = self.get_pair(tuple_id, scope, atoms, symbols)?;
        let asts = self.binop_eval(lhs_id, rhs_id, scope, atoms, symbols)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_boolean(x <= y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_boolean(x <= y)),
            (lhs, rhs) => Err(format!("cannot compare {:?} and {:?}", lhs, rhs)),
        }
    }

    pub(crate) fn add(
        &mut self,
        tuple_id: AstId,
        scope: ScopeId,
        atoms: &mut AtomMap,
        symbols: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let (lhs_id, rhs_id) = self.get_pair(tuple_id, scope, atoms, symbols)?;
        let asts = self.binop_eval(lhs_id, rhs_id, scope, atoms, symbols)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_int(x + y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_float(x + y)),
            (lhs, rhs) => Err(format!("cannot add {:?} and {:?}", lhs, rhs)),
        }
    }

    pub(crate) fn subtract(
        &mut self,
        tuple_id: AstId,
        scope: ScopeId,
        atoms: &mut AtomMap,
        symbols: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let (lhs_id, rhs_id) = self.get_pair(tuple_id, scope, atoms, symbols)?;
        let asts = self.binop_eval(lhs_id, rhs_id, scope, atoms, symbols)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_int(x - y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_float(x - y)),
            (lhs, rhs) => Err(format!("cannot subtract {:?} and {:?}", lhs, rhs)),
        }
    }

    pub(crate) fn multiply(
        &mut self,
        tuple_id: AstId,
        scope: ScopeId,
        atoms: &mut AtomMap,
        symbols: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let (lhs_id, rhs_id) = self.get_pair(tuple_id, scope, atoms, symbols)?;
        let asts = self.binop_eval(lhs_id, rhs_id, scope, atoms, symbols)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_int(x * y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_float(x * y)),
            (lhs, rhs) => Err(format!("cannot multiply {:?} and {:?}", lhs, rhs)),
        }
    }

    pub(crate) fn divide(
        &mut self,
        tuple_id: AstId,
        scope: ScopeId,
        atoms: &mut AtomMap,
        symbols: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let (lhs_id, rhs_id) = self.get_pair(tuple_id, scope, atoms, symbols)?;
        let asts = self.binop_eval(lhs_id, rhs_id, scope, atoms, symbols)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_int(x / y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_float(x / y)),
            (lhs, rhs) => Err(format!("cannot divide {:?} and {:?}", lhs, rhs)),
        }
    }

    pub(crate) fn modulus(
        &mut self,
        tuple_id: AstId,
        scope: ScopeId,
        atoms: &mut AtomMap,
        symbols: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let (lhs_id, rhs_id) = self.get_pair(tuple_id, scope, atoms, symbols)?;
        let asts = self.binop_eval(lhs_id, rhs_id, scope, atoms, symbols)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_int(x % y)),
            (lhs, rhs) => Err(format!("cannot modulus {:?} and {:?}", lhs, rhs)),
        }
    }

    pub(crate) fn negate(
        &mut self,
        expr_id: AstId,
        scope: ScopeId,
        atoms: &mut AtomMap,
        symbols: &mut SymbolTable,
    ) -> Result<AstId, String> {
        let evald_expr = self.eval(expr_id, scope, atoms, symbols)?;
        let expr = self.get(evald_expr).unwrap();
        match expr {
            Ast::Int(x) => Ok(self.create_int(-x)),
            Ast::Float(x) => Ok(self.create_float(-x)),
            expr => Err(format!("cannot negate {:?}", expr)),
        }
    }

    fn create_boolean(&self, value: bool) -> AstId {
        if value {
            self.truthy_id
        } else {
            self.nil_id
        }
    }

    fn get_pair(
        &mut self,
        tuple_id: AstId,
        scope: ScopeId,
        atoms: &mut AtomMap,
        symbols: &mut SymbolTable,
    ) -> Result<(AstId, AstId), String> {
        let zero = self.create_int(0);
        let one = self.create_int(1);

        let deref_0 = self.create_apply(tuple_id, zero);
        let deref_1 = self.create_apply(tuple_id, one);

        let evald_lhs = self.eval(deref_0, scope, atoms, symbols)?;
        let evald_rhs = self.eval(deref_1, scope, atoms, symbols)?;

        Ok((evald_lhs, evald_rhs))
    }

    fn binop_eval(
        &mut self,
        lhs_id: AstId,
        rhs_id: AstId,
        scope: ScopeId,
        atoms: &mut AtomMap,
        symbols: &mut SymbolTable,
    ) -> Result<(AstId, AstId), String> {
        let evald_lhs = self.eval(lhs_id, scope, atoms, symbols)?;
        let evald_rhs = self.eval(rhs_id, scope, atoms, symbols)?;

        Ok((evald_lhs, evald_rhs))
    }

    fn binop_get(&self, asts: (AstId, AstId)) -> Result<(&Ast, &Ast), String> {
        let lhs = self.get(asts.0).unwrap();
        let rhs = self.get(asts.1).unwrap();

        Ok((lhs, rhs))
    }
}
