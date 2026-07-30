use std::collections::HashMap;

use crate::{
    ast::{Ast, AstHeap, AstId},
    builtin::Builtin,
    elaborate::Elaboration,
    interner::AtomId,
    scope::DefId,
};

pub struct Code {
    pub instructions: Vec<Instr>,
    pub slots_used: u32,
    pub result: Slot,
}

impl Code {
    pub fn debug(&self) {
        println!("Slots used: {}", self.slots_used);
        println!("Result: {:?}", self.result);
        println!("Instructions:");
        for instr in self.instructions.iter() {
            println!("{instr:?}");
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instr {
    Const { dst: Slot, value: Value },

    MakeMap { dst: Slot, pairs: Vec<(Slot, Slot)> },

    Apply { dst: Slot, lhs: Slot, rhs: Slot },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Undefined,
    Int(i64),
    Float(f64),
    Char(char),
    Atom(AtomId),
    Builtin(Builtin),
    Map(Vec<(Value, Value)>),
}

impl Value {
    /// Interpret this value as an integer. Does some basic conversions
    pub fn as_int<T>(&self) -> Result<T, String>
    where
        T: From<i64>,
    {
        match self {
            Value::Int(x) => Ok(T::from(*x)),
            Value::Float(x) => Ok(T::from(*x as i64)),
            Value::Char(x) => Ok(T::from(*x as i64)),
            _ => Err(format!("cannot convert {:?} to int", self)),
        }
    }

    /// Interpret this value as an integer. Does some basic conversions
    pub fn as_float<T>(&self) -> Result<T, String>
    where
        T: From<f64>,
    {
        match self {
            Value::Int(x) => Ok(T::from(*x as f64)),
            Value::Float(x) => Ok(T::from(*x)),
            _ => Err(format!("cannot convert {:?} to float", self)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Slot(u32);

impl Slot {
    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

pub struct Lowerer<'a> {
    asts: &'a AstHeap,
    elab: &'a Elaboration,

    def_map: HashMap<DefId, Slot>,
    instructions: Vec<Instr>,
    slots_used: u32,
}

impl<'a> Lowerer<'a> {
    pub fn new(asts: &'a AstHeap, elab: &'a Elaboration) -> Self {
        Self {
            asts,
            elab,
            def_map: HashMap::new(),
            instructions: Vec::new(),
            slots_used: 0,
        }
    }

    pub fn lower(mut self, id: AstId) -> Code {
        let result = self.lower_ast(id);
        Code {
            slots_used: self.slots_used,
            instructions: self.instructions,
            result,
        }
    }

    pub fn lower_ast(&mut self, id: AstId) -> Slot {
        let ast = self.asts.get(id).expect("invalid AST id");

        match ast {
            Ast::Int(n) => {
                let slot = self.new_slot();
                self.emit(Instr::Const {
                    dst: slot,
                    value: Value::Int(*n),
                });
                slot
            }
            Ast::Float(n) => {
                let slot = self.new_slot();
                self.emit(Instr::Const {
                    dst: slot,
                    value: Value::Float(*n),
                });
                slot
            }
            Ast::Atom(id) => {
                let slot = self.new_slot();
                self.emit(Instr::Const {
                    dst: slot,
                    value: Value::Atom(*id),
                });
                slot
            }
            Ast::BuiltinFunction(builtin) => {
                let slot = self.new_slot();
                self.emit(Instr::Const {
                    dst: slot,
                    value: Value::Builtin(*builtin),
                });
                slot
            }
            Ast::Map(pairs) => {
                let dst = self.new_slot();
                let pairs = pairs
                    .iter()
                    .map(|(k, v)| (self.lower_ast(*k), self.lower_ast(*v)))
                    .collect();
                self.emit(Instr::MakeMap { dst, pairs });
                dst
            }
            Ast::Tuple(elems) => {
                let dst = self.new_slot();
                let pairs = elems
                    .iter()
                    .enumerate()
                    .map(|(i, elem)| {
                        let index: Slot = self.new_slot();
                        self.emit(Instr::Const {
                            dst: index,
                            value: Value::Int(i as i64),
                        });
                        let elem_val = self.lower_ast(*elem);
                        (index, elem_val)
                    })
                    .collect();
                self.emit(Instr::MakeMap { dst, pairs });
                dst
            }
            Ast::Identifier(_) => {
                let def = self.elab.refer(id).expect("should exist");
                *self
                    .def_map
                    .get(def)
                    .expect("def should be put by some binding")
            }
            Ast::Apply(lhs, rhs) => {
                let dst = self.new_slot();
                let lhs = self.lower_ast(*lhs);
                let rhs = self.lower_ast(*rhs);
                self.emit(Instr::Apply { dst, lhs, rhs });
                dst
            }
            Ast::Let(bindings, expr) => {
                for binding in bindings {
                    _ = self.lower_ast(*binding)
                }
                self.lower_ast(*expr)
            }
            Ast::Binding { rhs, .. } => {
                let def = self.elab.define(id).expect("asts gotta define something!");
                let slot = self.lower_ast(*rhs);
                self.def_map.insert(*def, slot);
                slot
            }
            _ => todo!("not implemented: {:?}", ast),
        }
    }

    fn new_slot(&mut self) -> Slot {
        let retval = Slot(self.slots_used);
        self.slots_used += 1;
        retval
    }

    fn emit(&mut self, instr: Instr) {
        self.instructions.push(instr);
    }
}
