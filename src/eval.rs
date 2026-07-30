use crate::{
    builtin::Builtin,
    ir::{Code, HeapSlot, Instr, Slot, Value},
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
        Self { code }
    }

    pub fn eval(&mut self) -> Result<Value, String> {
        let mut env = Env::new(self.code.slots_used);
        let mut heap = Heap::new();

        for instr in self.code.instructions.iter() {
            match instr {
                Instr::Const { dst, value } => env.store(*dst, *value),
                Instr::MakeMap { dst, pairs } => {
                    let map = MapObj {
                        pairs: pairs
                            .iter()
                            .map(|(k, v)| (env.load(*k), env.load(*v)))
                            .collect(),
                    };
                    let map_slot = heap.alloc_map(map);
                    env.store(*dst, Value::Map(map_slot))
                }
                Instr::Apply { dst, lhs, rhs } => {
                    let lhs = env.load(*lhs);
                    let rhs = env.load(*rhs);

                    match lhs {
                        Value::Map(addr) => env.store(*dst, heap.deref(addr).map_lookup(rhs)?),
                        Value::Builtin(builtin) => match builtin {
                            Builtin::Add => env.store(*dst, Self::add(rhs, &heap)?),
                        },
                        _ => panic!("can't apply to a {lhs:?}"),
                    }
                }
            }
        }

        Ok(env.load(self.code.result))
    }

    fn add(args: Value, heap: &Heap) -> Result<Value, String> {
        let (lhs, rhs) = Self::get_pair(args, heap)?;

        match (lhs, rhs) {
            (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x + y)),
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x + y)),
            (lhs, rhs) => Err(format!("cannot add {lhs:?} and {rhs:?}")),
        }
    }

    fn get_pair(value: Value, heap: &Heap) -> Result<(Value, Value), String> {
        let Value::Map(addr) = value else {
            return Err(String::from("not a tuple"));
        };

        let map_obj = heap.deref(addr);

        let lhs = map_obj.map_lookup(Value::Int(0))?;
        let rhs = map_obj.map_lookup(Value::Int(1))?;

        Ok((lhs, rhs))
    }
}
