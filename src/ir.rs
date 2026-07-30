use std::collections::HashMap;

use crate::{
    ast::{Ast, AstHeap, AstId},
    elaborate::Elaboration,
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

#[derive(Debug, Clone, Copy)]
pub enum Instr {
    Const { dst: Slot, value: Value },

    Move { dst: Slot, src: Slot },
    // TODO: Add more
}

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Undefined,
    Int(i64),
    // TODO: Add more
}

#[derive(Debug, Clone, Copy)]
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
            Ast::Identifier(_) => {
                let def = self.elab.refer(id).expect("should exist");
                *self
                    .def_map
                    .get(def)
                    .expect("def should be put by some binding")
            }
            Ast::Binding { rhs, .. } => {
                let def = self.elab.define(id).expect("asts gotta define something!");
                let slot = self.lower_ast(*rhs);
                self.def_map.insert(*def, slot);
                slot
            }
            Ast::Let(bindings, expr) => {
                for binding in bindings {
                    _ = self.lower_ast(*binding)
                }
                self.lower_ast(*expr)
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
