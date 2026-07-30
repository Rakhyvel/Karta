use crate::ir::{Code, Instr, Slot, Value};

pub struct Eval {
    code: Code,
}

struct Env {
    slots: Vec<Value>,
}

impl Env {
    fn new(slot_count: u32) -> Self {
        Self {
            slots: (0..slot_count).map(|_| Value::Undefined).collect(),
        }
    }

    fn store(&mut self, dst: Slot, val: Value) {
        self.slots[dst.as_usize()] = val
    }

    fn load(&self, dst: Slot) -> Value {
        self.slots[dst.as_usize()]
    }
}

impl Eval {
    pub fn new(code: Code) -> Self {
        Self { code }
    }

    pub fn eval(&mut self) -> Value {
        let mut env = Env::new(self.code.slots_used);

        for instr in self.code.instructions.iter().copied() {
            match instr {
                Instr::Const { dst, value } => {
                    env.store(dst, value);
                }
                Instr::Move { dst, src } => {
                    env.store(dst, env.load(src));
                }
            }
        }

        env.load(self.code.result)
    }
}
