use std::collections::HashMap;

use crate::{
    ast::{Ast, AstHeap, AstId},
    atom::{AtomId, AtomMap},
};

impl AstHeap {
    /// Takes in an AST heap & atoms, and a root AstId and returns the evaluated AstId, or an error if something went wrong
    pub(crate) fn eval(&mut self, root: AstId, atoms: &AtomMap) -> Result<AstId, String> {
        let ast = self.get(root).unwrap().clone();

        match ast {
            Ast::Int(_) | Ast::Float(_) | Ast::Char(_) | Ast::String(_) | Ast::Atom(_) => Ok(root),

            // Replace all values with an updated version
            Ast::Map(hash_map) => {
                let mut new_map: HashMap<AtomId, AstId> = HashMap::new();
                for (k, v) in hash_map {
                    new_map.insert(k, self.eval(v, atoms)?);
                }
                Ok(self.create_map(new_map))
            }

            Ast::And(lhs_id, rhs_id) => self.and(lhs_id, rhs_id, atoms),
            Ast::Or(lhs_id, rhs_id) => self.or(lhs_id, rhs_id, atoms),
            Ast::Not(expr_id) => self.not(expr_id, atoms),
            Ast::Equals(lhs_id, rhs_id) => self.equal(lhs_id, rhs_id, atoms),
            Ast::NotEquals(lhs_id, rhs_id) => self.not_equal(lhs_id, rhs_id, atoms),
            Ast::Greater(lhs_id, rhs_id) => self.greater(lhs_id, rhs_id, atoms),
            Ast::Lesser(lhs_id, rhs_id) => self.lesser(lhs_id, rhs_id, atoms),
            Ast::GreaterEqual(lhs_id, rhs_id) => self.greater_equal(lhs_id, rhs_id, atoms),
            Ast::LesserEqual(lhs_id, rhs_id) => self.lesser_equal(lhs_id, rhs_id, atoms),
            Ast::Add(lhs_id, rhs_id) => self.add(lhs_id, rhs_id, atoms),
            Ast::Subtract(lhs_id, rhs_id) => self.subtract(lhs_id, rhs_id, atoms),
            Ast::Multiply(lhs_id, rhs_id) => self.multiply(lhs_id, rhs_id, atoms),
            Ast::Divide(lhs_id, rhs_id) => self.divide(lhs_id, rhs_id, atoms),
            Ast::Modulus(lhs_id, rhs_id) => self.modulus(lhs_id, rhs_id, atoms),
            Ast::Negate(expr_id) => self.negate(expr_id, atoms),
        }
    }

    fn and(&mut self, lhs_id: AstId, rhs_id: AstId, atoms: &AtomMap) -> Result<AstId, String> {
        let value = self.truthy(lhs_id, atoms)? && self.truthy(rhs_id, atoms)?;
        Ok(self.create_boolean(value))
    }

    fn or(&mut self, lhs_id: AstId, rhs_id: AstId, atoms: &AtomMap) -> Result<AstId, String> {
        let value = self.truthy(lhs_id, atoms)? || self.truthy(rhs_id, atoms)?;
        Ok(self.create_boolean(value))
    }

    fn not(&mut self, expr_id: AstId, atoms: &AtomMap) -> Result<AstId, String> {
        let value = !self.truthy(expr_id, atoms)?;
        Ok(self.create_boolean(value))
    }

    fn truthy(&mut self, ast_id: AstId, atoms: &AtomMap) -> Result<bool, String> {
        let evald_id = self.eval(ast_id, atoms)?;
        let ast = self.get(evald_id).expect("couldn't get Ast for AstId");
        match ast {
            Ast::Atom(x) => Ok(x.as_usize() != 0),
            _ => Ok(true),
        }
    }

    fn equal(&mut self, lhs_id: AstId, rhs_id: AstId, atoms: &AtomMap) -> Result<AstId, String> {
        let asts = self.binop_eval(lhs_id, rhs_id, atoms)?;
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
        atoms: &AtomMap,
    ) -> Result<AstId, String> {
        let asts = self.binop_eval(lhs_id, rhs_id, atoms)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_boolean(x != y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_boolean(x != y)),
            (Ast::Atom(x), Ast::Atom(y)) => Ok(self.create_boolean(x != y)),
            (lhs, rhs) => Err(format!("cannot compare {:?} and {:?}", lhs, rhs)),
        }
    }

    fn greater(&mut self, lhs_id: AstId, rhs_id: AstId, atoms: &AtomMap) -> Result<AstId, String> {
        let asts = self.binop_eval(lhs_id, rhs_id, atoms)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_boolean(x > y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_boolean(x > y)),
            (lhs, rhs) => Err(format!("cannot compare {:?} and {:?}", lhs, rhs)),
        }
    }

    fn lesser(&mut self, lhs_id: AstId, rhs_id: AstId, atoms: &AtomMap) -> Result<AstId, String> {
        let asts = self.binop_eval(lhs_id, rhs_id, atoms)?;
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
        atoms: &AtomMap,
    ) -> Result<AstId, String> {
        let asts = self.binop_eval(lhs_id, rhs_id, atoms)?;
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
        atoms: &AtomMap,
    ) -> Result<AstId, String> {
        let asts = self.binop_eval(lhs_id, rhs_id, atoms)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_boolean(x <= y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_boolean(x <= y)),
            (lhs, rhs) => Err(format!("cannot compare {:?} and {:?}", lhs, rhs)),
        }
    }

    fn add(&mut self, lhs_id: AstId, rhs_id: AstId, atoms: &AtomMap) -> Result<AstId, String> {
        let asts = self.binop_eval(lhs_id, rhs_id, atoms)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_int(x + y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_float(x + y)),
            (lhs, rhs) => Err(format!("cannot add {:?} and {:?}", lhs, rhs)),
        }
    }

    fn subtract(&mut self, lhs_id: AstId, rhs_id: AstId, atoms: &AtomMap) -> Result<AstId, String> {
        let asts = self.binop_eval(lhs_id, rhs_id, atoms)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_int(x - y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_float(x - y)),
            (lhs, rhs) => Err(format!("cannot subtract {:?} and {:?}", lhs, rhs)),
        }
    }

    fn multiply(&mut self, lhs_id: AstId, rhs_id: AstId, atoms: &AtomMap) -> Result<AstId, String> {
        let asts = self.binop_eval(lhs_id, rhs_id, atoms)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_int(x * y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_float(x * y)),
            (lhs, rhs) => Err(format!("cannot multiply {:?} and {:?}", lhs, rhs)),
        }
    }

    fn divide(&mut self, lhs_id: AstId, rhs_id: AstId, atoms: &AtomMap) -> Result<AstId, String> {
        let asts = self.binop_eval(lhs_id, rhs_id, atoms)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_int(x / y)),
            (Ast::Float(x), Ast::Float(y)) => Ok(self.create_float(x / y)),
            (lhs, rhs) => Err(format!("cannot divide {:?} and {:?}", lhs, rhs)),
        }
    }

    fn modulus(&mut self, lhs_id: AstId, rhs_id: AstId, atoms: &AtomMap) -> Result<AstId, String> {
        let asts = self.binop_eval(lhs_id, rhs_id, atoms)?;
        match self.binop_get(asts)? {
            (Ast::Int(x), Ast::Int(y)) => Ok(self.create_int(x % y)),
            (lhs, rhs) => Err(format!("cannot modulus {:?} and {:?}", lhs, rhs)),
        }
    }

    fn negate(&mut self, expr_id: AstId, atoms: &AtomMap) -> Result<AstId, String> {
        let evald_expr = self.eval(expr_id, atoms)?;
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
        atoms: &AtomMap,
    ) -> Result<(AstId, AstId), String> {
        let evald_lhs = self.eval(lhs_id, atoms)?;
        let evald_rhs = self.eval(rhs_id, atoms)?;

        Ok((evald_lhs, evald_rhs))
    }

    fn binop_get(&self, asts: (AstId, AstId)) -> Result<(&Ast, &Ast), String> {
        let lhs = self.get(asts.0).unwrap();
        let rhs = self.get(asts.1).unwrap();

        Ok((lhs, rhs))
    }
}
