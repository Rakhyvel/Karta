use crate::{
    builtin::Builtin,
    ir::{FunctionId, HeapAddr, HeapObjKind, Instr, Program, Slot, Value},
};

pub struct Eval {
    program: Program,
    frame_stack: Vec<Frame>,
    heap: Heap,
}

struct Frame {
    func_id: FunctionId,
    slots: Vec<Value>,
    ip: usize,
}

impl Frame {
    fn new(func_id: FunctionId, slot_count: u32) -> Self {
        Self {
            func_id,
            slots: (0..slot_count).map(|_| Value::Undefined).collect(),
            ip: 0,
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

    fn alloc_map(&mut self, obj: MapObj) -> HeapAddr {
        let retval = HeapAddr::new(HeapObjKind::Map, self.maps.len() as u32);
        self.maps.push(obj);
        retval
    }

    fn alloc_closure(&mut self, obj: MapObj) -> HeapAddr {
        let retval = HeapAddr::new(HeapObjKind::Map, self.maps.len() as u32);
        self.maps.push(obj);
        retval
    }

    fn deref(&self, slot: HeapAddr) -> &MapObj {
        &self.maps[slot.as_usize()]
    }
}

impl Eval {
    pub fn new(program: Program) -> Self {
        let entry = program.entry;
        let mut retval = Self {
            frame_stack: Vec::new(),
            program,
            heap: Heap::new(),
        };
        retval.push_frame(entry);
        retval
    }

    pub fn eval(&mut self) -> Result<Value, String> {
        loop {
            let frame_idx = self.frame_stack.len() - 1;

            let (func_id, ip) = {
                let frame = &self.frame_stack[frame_idx];
                (frame.func_id, frame.ip)
            };

            let instructions = &self.program.funcs[func_id.as_usize()].instructions;
            if ip >= instructions.len() {
                if self.frame_stack.len() == 1 {
                    break Ok(self.load_retval());
                }
                self.pop_frame();
                continue;
            }
            let instr = instructions[ip].clone();

            self.frame_stack[frame_idx].ip += 1;

            self.execute_instruction(instr)?;
        }
    }

    fn execute_instruction(&mut self, instr: Instr) -> Result<(), String> {
        match instr {
            Instr::Const { dst, value } => self.store(dst, value),

            Instr::MakeMap { dst, pairs } => {
                let map = MapObj {
                    pairs: pairs
                        .iter()
                        .map(|(k, v)| (self.load(*k), self.load(*v)))
                        .collect(),
                };
                let map_slot = self.heap.alloc_map(map);
                self.store(dst, Value::Map(map_slot))
            }

            Instr::MakeClosure { dst, function } => {}

            Instr::Apply { dst, lhs, rhs } => {
                let lhs = self.load(lhs);
                let rhs = self.load(rhs);

                let result = self.apply(lhs, rhs)?;

                self.store(dst, result)
            }
        };
        Ok(())
    }

    fn load(&self, slot: Slot) -> Value {
        let frame_idx = self.frame_stack.len() - 1;
        self.frame_stack[frame_idx].load(slot)
    }

    fn store(&mut self, slot: Slot, value: Value) {
        let frame_idx = self.frame_stack.len() - 1;
        self.frame_stack[frame_idx].store(slot, value);
    }

    fn load_retval(&self) -> Value {
        let frame_idx = self.frame_stack.len() - 1;
        let func_id = self.frame_stack[frame_idx].func_id;
        let result_slot = self.program.funcs[func_id.as_usize()].result;
        self.frame_stack[frame_idx].load(result_slot)
    }

    fn push_frame(&mut self, func_id: FunctionId) {
        let slot_count = self.program.funcs[func_id.as_usize()].slots_used;
        self.frame_stack.push(Frame::new(func_id, slot_count));
    }

    fn pop_frame(&mut self) {
        self.frame_stack.pop();
    }

    fn apply(&self, lhs: Value, rhs: Value) -> Result<Value, String> {
        match lhs {
            Value::Map(addr) => self.heap.deref(addr).map_lookup(rhs),
            Value::Builtin(Builtin::Add) => self.add(rhs),
            _ => panic!("can't apply to a {lhs:?}"),
        }
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
