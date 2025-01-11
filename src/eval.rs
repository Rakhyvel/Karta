use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use crate::{
    ast::{Ast, AstHeap, AstId},
    atom::{AtomId, AtomMap},
    scope::Scope,
};

impl AstHeap {
    /// Takes in an AST heap & atoms, and a root AstId and returns the evaluated AstId, or an error if something went wrong
    /// TODO: Accept a scope.
    pub(crate) fn eval(
        &mut self,
        root: AstId,
        scope: &Arc<Mutex<Scope>>,
        atoms: &AtomMap,
    ) -> Result<AstId, String> {
        let ast = self.get(root).unwrap().clone();

        match ast {
            Ast::Int(_) | Ast::Float(_) | Ast::Char(_) | Ast::String(_) | Ast::Atom(_) => Ok(root),

            // Replace all values with an updated version
            Ast::Map(hash_map) => {
                let mut new_map: HashMap<AtomId, AstId> = HashMap::new();
                for (k, v) in hash_map {
                    new_map.insert(k, self.eval(v, scope, atoms)?);
                }
                Ok(self.create_map(new_map))
            }

            Ast::And(lhs_id, rhs_id) => self.and(lhs_id, rhs_id, scope, atoms),
            Ast::Or(lhs_id, rhs_id) => self.or(lhs_id, rhs_id, scope, atoms),
            Ast::Not(expr_id) => self.not(expr_id, scope, atoms),
            Ast::Equals(lhs_id, rhs_id) => self.equal(lhs_id, rhs_id, scope, atoms),
            Ast::NotEquals(lhs_id, rhs_id) => self.not_equal(lhs_id, rhs_id, scope, atoms),
            Ast::Greater(lhs_id, rhs_id) => self.greater(lhs_id, rhs_id, scope, atoms),
            Ast::Lesser(lhs_id, rhs_id) => self.lesser(lhs_id, rhs_id, scope, atoms),
            Ast::GreaterEqual(lhs_id, rhs_id) => self.greater_equal(lhs_id, rhs_id, scope, atoms),
            Ast::LesserEqual(lhs_id, rhs_id) => self.lesser_equal(lhs_id, rhs_id, scope, atoms),
            Ast::Add(lhs_id, rhs_id) => self.add(lhs_id, rhs_id, scope, atoms),
            Ast::Subtract(lhs_id, rhs_id) => self.subtract(lhs_id, rhs_id, scope, atoms),
            Ast::Multiply(lhs_id, rhs_id) => self.multiply(lhs_id, rhs_id, scope, atoms),
            Ast::Divide(lhs_id, rhs_id) => self.divide(lhs_id, rhs_id, scope, atoms),
            Ast::Modulus(lhs_id, rhs_id) => self.modulus(lhs_id, rhs_id, scope, atoms),
            Ast::Negate(expr_id) => self.negate(expr_id, scope, atoms),
            Ast::Let(new_scope, expr) => self.eval(expr, &new_scope, atoms),
            Ast::Identifier(id) => {
                let mut curr_scope: Option<Arc<Mutex<Scope>>> = Some(scope.clone());
                loop {
                    if let Some(some_curr_scope) = curr_scope {
                        let s = some_curr_scope.lock().unwrap();
                        if let Some(ast_id) = s.get(id) {
                            return self.eval(ast_id, scope, atoms);
                        } else {
                            curr_scope = s.parent().cloned()
                        }
                    } else {
                        panic!("cannot find identifier");
                    }
                }
            }
        }
    }

    fn and(
        &mut self,
        lhs_id: AstId,
        rhs_id: AstId,
        scope: &Arc<Mutex<Scope>>,
        atoms: &AtomMap,
    ) -> Result<AstId, String> {
        let value = self.truthy(lhs_id, scope, atoms)? && self.truthy(rhs_id, scope, atoms)?;
        Ok(self.create_boolean(value))
    }

    fn or(
        &mut self,
        lhs_id: AstId,
        rhs_id: AstId,
        scope: &Arc<Mutex<Scope>>,
        atoms: &AtomMap,
    ) -> Result<AstId, String> {
        let value = self.truthy(lhs_id, scope, atoms)? || self.truthy(rhs_id, scope, atoms)?;
        Ok(self.create_boolean(value))
    }

    fn not(
        &mut self,
        expr_id: AstId,
        scope: &Arc<Mutex<Scope>>,
        atoms: &AtomMap,
    ) -> Result<AstId, String> {
        let value = !self.truthy(expr_id, scope, atoms)?;
        Ok(self.create_boolean(value))
    }

    fn truthy(
        &mut self,
        ast_id: AstId,
        scope: &Arc<Mutex<Scope>>,
        atoms: &AtomMap,
    ) -> Result<bool, String> {
        let evald_id = self.eval(ast_id, scope, atoms)?;
        let ast = self.get(evald_id).expect("couldn't get Ast for AstId");
        match ast {
            Ast::Atom(x) => Ok(x.as_usize() != 0),
            _ => Ok(true),
        }
    }

    fn equal(
        &mut self,
        lhs_id: AstId,
        rhs_id: AstId,
        scope: &Arc<Mutex<Scope>>,
        atoms: &AtomMap,
    ) -> Result<AstId, String> {
        let asts = self.binop_eval(lhs_id, rhs_id, scope, atoms)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_boolean(x == y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_boolean(x == y)),
            (Ast::Atom(x), Ast::Atom(y)) => Ok(self.create_boolean(x == y)),
            (lhs, rhs) => Err(format!("cannot compare {:?} and {:?}", lhs, rhs)),
        }
    }

    fn not_equal(
        &mut self,
        lhs_id: AstId,
        rhs_id: AstId,
        scope: &Arc<Mutex<Scope>>,
        atoms: &AtomMap,
    ) -> Result<AstId, String> {
        let asts = self.binop_eval(lhs_id, rhs_id, scope, atoms)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_boolean(x != y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_boolean(x != y)),
            (Ast::Atom(x), Ast::Atom(y)) => Ok(self.create_boolean(x != y)),
            (lhs, rhs) => Err(format!("cannot compare {:?} and {:?}", lhs, rhs)),
        }
    }

    fn greater(
        &mut self,
        lhs_id: AstId,
        rhs_id: AstId,
        scope: &Arc<Mutex<Scope>>,
        atoms: &AtomMap,
    ) -> Result<AstId, String> {
        let asts = self.binop_eval(lhs_id, rhs_id, scope, atoms)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_boolean(x > y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_boolean(x > y)),
            (lhs, rhs) => Err(format!("cannot compare {:?} and {:?}", lhs, rhs)),
        }
    }

    fn lesser(
        &mut self,
        lhs_id: AstId,
        rhs_id: AstId,
        scope: &Arc<Mutex<Scope>>,
        atoms: &AtomMap,
    ) -> Result<AstId, String> {
        let asts = self.binop_eval(lhs_id, rhs_id, scope, atoms)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_boolean(x < y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_boolean(x < y)),
            (lhs, rhs) => Err(format!("cannot compare {:?} and {:?}", lhs, rhs)),
        }
    }

    fn greater_equal(
        &mut self,
        lhs_id: AstId,
        rhs_id: AstId,
        scope: &Arc<Mutex<Scope>>,
        atoms: &AtomMap,
    ) -> Result<AstId, String> {
        let asts = self.binop_eval(lhs_id, rhs_id, scope, atoms)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_boolean(x >= y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_boolean(x >= y)),
            (lhs, rhs) => Err(format!("cannot compare {:?} and {:?}", lhs, rhs)),
        }
    }

    fn lesser_equal(
        &mut self,
        lhs_id: AstId,
        rhs_id: AstId,
        scope: &Arc<Mutex<Scope>>,
        atoms: &AtomMap,
    ) -> Result<AstId, String> {
        let asts = self.binop_eval(lhs_id, rhs_id, scope, atoms)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_boolean(x <= y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_boolean(x <= y)),
            (lhs, rhs) => Err(format!("cannot compare {:?} and {:?}", lhs, rhs)),
        }
    }

    fn add(
        &mut self,
        lhs_id: AstId,
        rhs_id: AstId,
        scope: &Arc<Mutex<Scope>>,
        atoms: &AtomMap,
    ) -> Result<AstId, String> {
        let asts = self.binop_eval(lhs_id, rhs_id, scope, atoms)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_int(x + y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_float(x + y)),
            (lhs, rhs) => Err(format!("cannot add {:?} and {:?}", lhs, rhs)),
        }
    }

    fn subtract(
        &mut self,
        lhs_id: AstId,
        rhs_id: AstId,
        scope: &Arc<Mutex<Scope>>,
        atoms: &AtomMap,
    ) -> Result<AstId, String> {
        let asts = self.binop_eval(lhs_id, rhs_id, scope, atoms)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_int(x - y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_float(x - y)),
            (lhs, rhs) => Err(format!("cannot subtract {:?} and {:?}", lhs, rhs)),
        }
    }

    fn multiply(
        &mut self,
        lhs_id: AstId,
        rhs_id: AstId,
        scope: &Arc<Mutex<Scope>>,
        atoms: &AtomMap,
    ) -> Result<AstId, String> {
        let asts = self.binop_eval(lhs_id, rhs_id, scope, atoms)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_int(x * y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_float(x * y)),
            (lhs, rhs) => Err(format!("cannot multiply {:?} and {:?}", lhs, rhs)),
        }
    }

    fn divide(
        &mut self,
        lhs_id: AstId,
        rhs_id: AstId,
        scope: &Arc<Mutex<Scope>>,
        atoms: &AtomMap,
    ) -> Result<AstId, String> {
        let asts = self.binop_eval(lhs_id, rhs_id, scope, atoms)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_int(x / y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_float(x / y)),
            (lhs, rhs) => Err(format!("cannot divide {:?} and {:?}", lhs, rhs)),
        }
    }

    fn modulus(
        &mut self,
        lhs_id: AstId,
        rhs_id: AstId,
        scope: &Arc<Mutex<Scope>>,
        atoms: &AtomMap,
    ) -> Result<AstId, String> {
        let asts = self.binop_eval(lhs_id, rhs_id, scope, atoms)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_int(x % y)),
            (lhs, rhs) => Err(format!("cannot modulus {:?} and {:?}", lhs, rhs)),
        }
    }

    fn negate(
        &mut self,
        expr_id: AstId,
        scope: &Arc<Mutex<Scope>>,
        atoms: &AtomMap,
    ) -> Result<AstId, String> {
        let evald_expr = self.eval(expr_id, scope, atoms)?;
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

    fn binop_eval(
        &mut self,
        lhs_id: AstId,
        rhs_id: AstId,
        scope: &Arc<Mutex<Scope>>,
        atoms: &AtomMap,
    ) -> Result<(AstId, AstId), String> {
        let evald_lhs = self.eval(lhs_id, scope, atoms)?;
        let evald_rhs = self.eval(rhs_id, scope, atoms)?;

        Ok((evald_lhs, evald_rhs))
    }

    fn binop_get(&self, asts: (AstId, AstId)) -> Result<(&Ast, &Ast), String> {
        let lhs = self.get(asts.0).unwrap();
        let rhs = self.get(asts.1).unwrap();

        Ok((lhs, rhs))
    }
}
