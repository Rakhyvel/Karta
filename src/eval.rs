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

    fn load(&self, dst: Slot) -> &Value {
        &self.slots[dst.as_usize()]
    }
}

impl Eval {
    pub fn new(code: Code) -> Self {
        Self { code }
    }

    pub fn eval(&mut self) -> Value {
        let mut env = Env::new(self.code.slots_used);

        for instr in self.code.instructions.iter() {
            match instr {
                Instr::Const { dst, value } => {
                    env.store(*dst, value.clone());
                }
                Instr::Apply { dst, lhs, rhs } => {
                    let lhs = env.load(*lhs);
                    let rhs = env.load(*rhs);
                    match lhs {
                        Value::Map(pairs) => {
                            let pair = pairs.iter().find(|(k, _)| {
                                let k_val = env.load(*k);
                                k_val == rhs
                            });
                            if let Some((_, v)) = pair {
                                let v_val = env.load(*v);
                                env.store(*dst, v_val.clone())
                            } else {
                                panic!("map didnt contain the key!")
                            }
                        }
                        _ => panic!("can't apply to a {lhs:?}"),
                    }
                }
            }
        }

        env.load(self.code.result).clone()
    }
}
