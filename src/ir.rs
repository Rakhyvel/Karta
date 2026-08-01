use std::{cell::RefCell, collections::HashMap};

use crate::{
    ast::{Ast, AstHeap, AstId},
    builtin::Builtin,
    elaborate::Elaboration,
    interner::AtomId,
    ir::Instr::RetEval,
    scope::DefId,
};

pub struct Function {
    pub instructions: Vec<Instr>,
    pub slots_used: u32,
    pub result: Slot,

    pub params: Vec<Slot>,
    pub captures: Vec<DefId>,
}

impl Function {
    pub fn debug(&self) {
        println!("Slots used: {}", self.slots_used);
        println!("Result: {:?}", self.result);
        println!("Instructions:");
        for instr in self.instructions.iter() {
            println!("{instr:?}");
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FunctionId(u32);

impl FunctionId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone)]
pub enum Instr {
    Const { dst: Slot, value: Value },

    MakeMap { dst: Slot, pairs: Vec<(Slot, Slot)> },

    MakeClosure { dst: Slot, function: FunctionId },

    Apply { dst: Slot, lhs: Slot, rhs: Slot },

    Ret,
    RetEval,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Value {
    Undefined,
    Int(i64),
    Float(f64),
    Char(char),
    Atom(AtomId),
    Builtin(Builtin),
    Map(HeapAddr),
    Closure(HeapAddr),
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
    pub fn new(x: u32) -> Slot {
        Slot(x)
    }

    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HeapObjKind {
    Map,
    Closure,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HeapAddr(pub HeapObjKind, pub u32);

impl HeapAddr {
    pub fn new(kind: HeapObjKind, id: u32) -> Self {
        Self(kind, id)
    }

    pub fn as_usize(self) -> usize {
        self.1 as usize
    }
}

pub struct Program {
    pub funcs: Vec<Function>,
    pub entry: FunctionId,
}

pub struct Lowerer<'a> {
    asts: &'a AstHeap,
    elab: &'a Elaboration,
    funcs: RefCell<Vec<Function>>,
}

impl<'a> Lowerer<'a> {
    pub fn new(asts: &'a AstHeap, elab: &'a Elaboration) -> Self {
        Self {
            asts,
            elab,
            funcs: RefCell::new(Vec::new()),
        }
    }

    pub fn lower(&mut self, root: AstId) -> Program {
        let mut f = FunctionLowerer::new(self.asts, self.elab, &self.funcs);

        let result = f.lower_ast(root);

        f.emit(RetEval);

        let root_function = Function {
            instructions: f.instructions,
            slots_used: f.slots_used,
            result,
            params: f.params,
            captures: f.captures,
        };

        let mut funcs = self.funcs.borrow_mut();

        let entry = FunctionId(funcs.len() as u32);
        funcs.push(root_function);

        Program {
            funcs: funcs.drain(..).collect(),
            entry,
        }
    }
}

struct FunctionLowerer<'a> {
    asts: &'a AstHeap,
    elab: &'a Elaboration,
    funcs: &'a RefCell<Vec<Function>>,

    def_map: HashMap<DefId, Slot>,
    instructions: Vec<Instr>,
    slots_used: u32,
    params: Vec<Slot>,
    captures: Vec<DefId>,
}

impl<'a> FunctionLowerer<'a> {
    pub fn new(
        asts: &'a AstHeap,
        elab: &'a Elaboration,
        funcs: &'a RefCell<Vec<Function>>,
    ) -> Self {
        Self {
            asts,
            elab,
            funcs,
            def_map: HashMap::new(),
            instructions: Vec::new(),
            slots_used: 0,
            params: Vec::new(),
            captures: Vec::new(),
        }
    }

    pub fn lower_ast(&mut self, id: AstId) -> Slot {
        let ast = self.asts.get(id).expect("invalid AST id");

        match ast {
            Ast::Int(n) => self.lower_const(Value::Int(*n)),
            Ast::Float(n) => self.lower_const(Value::Float(*n)),
            Ast::Atom(id) => self.lower_const(Value::Atom(*id)),
            Ast::BuiltinFunction(builtin) => self.lower_const(Value::Builtin(*builtin)),

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

            Ast::Lambda(param, body) => {
                let function = {
                    let mut f = Self::new(self.asts, self.elab, self.funcs);

                    let param_slot = f.new_slot();

                    let def = self.elab.pattern_define(*param);
                    f.def_map.insert(def, param_slot);
                    f.params.push(param_slot);

                    let result = f.lower_ast(*body);
                    f.emit(Instr::Ret);

                    Function {
                        slots_used: f.slots_used,
                        instructions: f.instructions,
                        result,
                        params: f.params,
                        captures: f.captures,
                    }
                };

                let func_id = {
                    let mut funcs = self.funcs.borrow_mut();
                    let id = FunctionId(funcs.len() as u32);
                    funcs.push(function);
                    id
                };

                let dst = self.new_slot();
                self.emit(Instr::MakeClosure {
                    dst,
                    function: func_id,
                });
                dst
            }

            _ => todo!("not implemented: {:?}", ast),
        }
    }

    fn lower_const(&mut self, value: Value) -> Slot {
        let dst = self.new_slot();
        self.emit(Instr::Const { dst, value });
        dst
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
