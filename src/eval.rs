use crate::{
    builtin::Builtin,
    ir::{FunctionId, HeapAddr, HeapObjKind, Instr, Program, Slot, Value},
};

pub struct Eval {
    program: Program,
    frame_stack: Vec<Frame>,
    heap: Heap,
    result: Value,
}

struct Frame {
    func_id: FunctionId,
    // TODO: captured closure heap addr
    slots: Vec<Value>,
    ip: usize,
    /// Relative to the _CALLER_'s frame
    return_slot: Slot,
}

impl Frame {
    fn new(func_id: FunctionId, slot_count: u32, return_slot: Slot) -> Self {
        Self {
            func_id,
            slots: (0..slot_count).map(|_| Value::Undefined).collect(),
            ip: 0,
            return_slot,
        }
    }

    fn store(&mut self, dst: Slot, val: Value) {
        self.slots[dst.as_usize()] = val
    }

    fn load(&self, dst: Slot) -> Value {
        self.slots[dst.as_usize()]
    }
}

// TODO: Make this an enum `HeapObj`, then HeapAddr just refers to one of these
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
    funcs: Vec<FunctionId>,
}

impl Heap {
    fn new() -> Self {
        Self {
            maps: Vec::new(),
            funcs: Vec::new(),
        }
    }

    fn alloc_map(&mut self, obj: MapObj) -> HeapAddr {
        let retval = HeapAddr::new(HeapObjKind::Map, self.maps.len() as u32);
        self.maps.push(obj);
        retval
    }

    fn alloc_closure(&mut self, func_id: FunctionId) -> HeapAddr {
        let retval = HeapAddr::new(HeapObjKind::Closure, self.funcs.len() as u32);
        self.funcs.push(func_id);
        retval
    }

    fn deref(&self, slot: HeapAddr) -> &MapObj {
        &self.maps[slot.as_usize()]
    }

    fn deref_func(&self, slot: HeapAddr) -> &FunctionId {
        &self.funcs[slot.as_usize()]
    }
}

impl Eval {
    pub fn new(program: Program) -> Self {
        let entry = program.entry;
        let mut retval = Self {
            frame_stack: Vec::new(),
            program,
            heap: Heap::new(),
            result: Value::Undefined,
        };
        retval.push_frame(entry, Value::Undefined, Slot::new(0));
        retval
    }

    pub fn eval(&mut self) -> Result<Value, String> {
        while !self.frame_stack.is_empty() {
            let frame_idx = self.frame_stack.len() - 1;

            let (func_id, ip) = {
                let frame = &self.frame_stack[frame_idx];
                (frame.func_id, frame.ip)
            };

            let instructions = &self.program.funcs[func_id.as_usize()].instructions;
            let instr = instructions[ip].clone();

            self.frame_stack[frame_idx].ip += 1;

            self.execute_instruction(instr)?;
        }

        Ok(self.result)
    }

    fn execute_instruction(&mut self, instr: Instr) -> Result<(), String> {
        println!("{instr:?}");
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

            Instr::MakeClosure { dst, function } => {
                let func_addr = self.heap.alloc_closure(function);
                self.store(dst, Value::Closure(func_addr))
            }

            Instr::Apply { dst, lhs, rhs } => {
                let lhs = self.load(lhs);
                let rhs = self.load(rhs);

                self.apply(dst, lhs, rhs)?;
            }

            Instr::Ret => {
                let frame = self.frame_stack.pop().unwrap();
                let retval = frame.slots[1]; // retval = slot 1
                self.store(frame.return_slot, retval);
            }

            Instr::RetEval => {
                let frame = self.frame_stack.pop().unwrap();
                self.result = frame.slots[0]; // evalval = slot 0 (no args)
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

    fn push_frame(&mut self, func_id: FunctionId, arg: Value, return_slot: Slot) {
        let slot_count = self.program.funcs[func_id.as_usize()].slots_used;
        let mut frame = Frame::new(func_id, slot_count, return_slot);
        frame.slots[0] = arg; // Store arg
        self.frame_stack.push(frame);
    }

    fn apply(&mut self, dst: Slot, lhs: Value, rhs: Value) -> Result<(), String> {
        match lhs {
            Value::Map(addr) => self.store(dst, self.heap.deref(addr).map_lookup(rhs)?),
            Value::Closure(addr) => {
                let func_id = self.heap.deref_func(addr);
                self.push_frame(*func_id, rhs, dst);
            }
            Value::Builtin(Builtin::Add) => self.store(dst, self.add(rhs)?),
            _ => panic!("can't apply to a {lhs:?}"),
        }
        Ok(())
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
