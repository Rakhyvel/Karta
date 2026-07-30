use crate::{
    builtin::{self, Builtin},
    ir::{Code, Instr, Slot, Value},
};

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

    pub fn eval(&mut self) -> Result<Value, String> {
        let mut env = Env::new(self.code.slots_used);

        for instr in self.code.instructions.iter() {
            match instr {
                Instr::Const { dst, value } => {
                    env.store(*dst, value.clone());
                }
                Instr::MakeMap { dst, pairs } => {
                    let map = pairs
                        .iter()
                        .map(|(k, v)| (env.load(*k).clone(), env.load(*v).clone()))
                        .collect();
                    env.store(*dst, Value::Map(map))
                }
                Instr::Apply { dst, lhs, rhs } => {
                    let lhs = env.load(*lhs);
                    let rhs = env.load(*rhs);

                    match lhs {
                        Value::Map(_) => env.store(*dst, Self::map_lookup(lhs, rhs)?),
                        Value::Builtin(builtin) => match builtin {
                            Builtin::Add => env.store(*dst, Self::add(rhs)?),
                        },
                        _ => panic!("can't apply to a {lhs:?}"),
                    }
                }
            }
        }

        Ok(env.load(self.code.result).clone())
    }

    fn map_lookup(map: &Value, key: &Value) -> Result<Value, String> {
        let Value::Map(pairs) = map else {
            return Err(String::from("not a map"));
        };

        let pair = pairs.iter().find(|(k, _)| k == key);

        if let Some((_, v)) = pair {
            Ok(v.clone())
        } else {
            panic!("map didnt contain the key!")
        }
    }

    fn add(args: &Value) -> Result<Value, String> {
        let (lhs, rhs) = Self::get_pair(args)?;

        match (lhs, rhs) {
            (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x + y)),
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x + y)),
            (lhs, rhs) => Err(format!("cannot add {lhs:?} and {rhs:?}")),
        }
    }

    fn get_pair(value: &Value) -> Result<(Value, Value), String> {
        let Value::Map(_) = value else {
            return Err(String::from("not a tuple"));
        };

        let lhs = Self::map_lookup(value, &Value::Int(0))?;
        let rhs = Self::map_lookup(value, &Value::Int(1))?;

        Ok((lhs, rhs))
    }
}
