use crate::ast::AstId;

pub enum Instr {
    Const { dst: Slot, value: Value },

    Move { dst: Slot, src: Slot },
    // TODO: Add more
}

pub enum Value {
    Int(i64),
    // TODO: Add more
}

pub struct Slot(u32);

pub struct Lowerer {
    instructions: Vec<Instr>,
    slots_used: u32,
}

impl Lowerer {
    pub fn lower(&mut self, ast: AstId) -> Slot {
        Slot(3)
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
