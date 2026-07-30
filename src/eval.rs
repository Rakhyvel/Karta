use crate::{
    builtin::Builtin,
    ir::{Code, HeapSlot, Instr, Slot, Value},
};

pub struct Eval {
    code: Code,
    env: Env,
    heap: Heap,
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

struct MapObj {
    pairs: Vec<(Value, Value)>,
}

impl MapObj {
    fn map_lookup(&self, key: Value) -> Result<Value, String> {
        self.pairs
            .iter()
            .find_map(|(k, v)| (*k == key).then_some(*v))
            .ok_or_else(|| format!("map didn't contain key {key:?}"))
    }
}

struct Heap {
    maps: Vec<MapObj>,
}

impl Heap {
    fn new() -> Self {
        Self { maps: Vec::new() }
    }

    fn alloc_map(&mut self, obj: MapObj) -> HeapSlot {
        let retval = HeapSlot::new(self.maps.len() as u32);
        self.maps.push(obj);
        retval
    }

    fn deref(&self, slot: HeapSlot) -> &MapObj {
        &self.maps[slot.as_usize()]
    }
}

impl Eval {
    pub fn new(code: Code) -> Self {
        Self {
            env: Env::new(code.slots_used),
            code,
            heap: Heap::new(),
        }
    }

    pub fn eval(&mut self) -> Result<Value, String> {
        for instr in self.code.instructions.iter() {
            match instr {
                Instr::Const { dst, value } => self.env.store(*dst, *value),

                Instr::MakeMap { dst, pairs } => {
                    let map = MapObj {
                        pairs: pairs
                            .iter()
                            .map(|(k, v)| (self.env.load(*k), self.env.load(*v)))
                            .collect(),
                    };
                    let map_slot = self.heap.alloc_map(map);
                    self.env.store(*dst, Value::Map(map_slot))
                }

                Instr::Apply { dst, lhs, rhs } => {
                    let lhs = self.env.load(*lhs);
                    let rhs = self.env.load(*rhs);

                    let result = match lhs {
                        Value::Map(addr) => self.heap.deref(addr).map_lookup(rhs)?,
                        Value::Builtin(builtin) => match builtin {
                            Builtin::Add => self.add(rhs)?,
                        },
                        _ => panic!("can't apply to a {lhs:?}"),
                    };

                    self.env.store(*dst, result)
                }
            }
        }

        Ok(self.env.load(self.code.result))
    }

    fn add(&self, args: Value) -> Result<Value, String> {
        let (lhs, rhs) = self.get_pair(args)?;

        match (lhs, rhs) {
            (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x + y)),
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x + y)),
            (lhs, rhs) => Err(format!("cannot add {lhs:?} and {rhs:?}")),
        }
    }

    fn get_pair(&self, value: Value) -> Result<(Value, Value), String> {
        let Value::Map(addr) = value else {
            return Err(String::from("not a tuple"));
        };

        let map_obj = self.heap.deref(addr);

        let lhs = map_obj.map_lookup(Value::Int(0))?;
        let rhs = map_obj.map_lookup(Value::Int(1))?;

        Ok((lhs, rhs))
    }
}
