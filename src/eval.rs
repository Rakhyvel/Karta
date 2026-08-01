use std::rc::Rc;

use crate::{
    builtin::Builtin,
    ir::{Function, FunctionId, HeapAddr, Instr, Program, Slot, Value},
};

pub struct Eval {
    program: Program,
    frame_stack: Vec<Frame>,
    heap: Heap,
    result: Value,
}

struct Frame {
    /// Shared-ptr to the corresponding Function's instructions
    instrs: Rc<[Instr]>,
    /// The local slots allocated for this frame
    slots: Vec<Value>,
    /// The instruction pointer into this frame's function's instructions
    ip: usize,
    /// Relative to the _CALLER_'s frame
    return_slot: Slot,
}

impl Frame {
    fn new(func: &Function, arg: Value, return_slot: Slot) -> Self {
        let mut slots = vec![Value::Undefined; func.slots_used as usize];
        slots[0] = arg;

        Self {
            instrs: func.instructions.clone(),
            slots,
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

enum HeapObj {
    Map(Vec<(Value, Value)>),
    Closure(FunctionId, Vec<Value>),
}

impl HeapObj {
    fn map_lookup(&self, key: Value) -> Result<Value, String> {
        self.as_map()?
            .iter()
            .find_map(|(k, v)| (*k == key).then_some(*v))
            .ok_or_else(|| format!("map didn't contain key {key:?}"))
    }

    fn as_map(&self) -> Result<&[(Value, Value)], String> {
        match self {
            HeapObj::Map(pairs) => Ok(pairs),
            HeapObj::Closure(..) => Err(String::from("expected a map, found a closure")),
        }
    }
}

struct Heap {
    objs: Vec<HeapObj>,
}

impl Heap {
    fn new() -> Self {
        Self { objs: Vec::new() }
    }

    fn alloc(&mut self, obj: HeapObj) -> HeapAddr {
        let retval = HeapAddr::new(self.objs.len() as u32);
        self.objs.push(obj);
        retval
    }

    fn deref(&self, addr: HeapAddr) -> &HeapObj {
        &self.objs[addr.as_usize()]
    }
}

impl Eval {
    pub fn new(program: Program) -> Self {
        let entry = program.entry;
        let function = &program.funcs[entry.as_usize()];

        Self {
            frame_stack: vec![Frame::new(function, Value::Undefined, Slot::new(0))],
            program,
            heap: Heap::new(),
            result: Value::Undefined,
        }
    }

    pub fn eval(&mut self) -> Result<Value, String> {
        while let Some(frame) = self.frame_stack.last_mut() {
            let instrs = frame.instrs.clone();

            let ip = frame.ip;
            frame.ip += 1;

            self.execute_instruction(&instrs[ip])?;
        }

        Ok(self.result)
    }

    fn execute_instruction(&mut self, instr: &Instr) -> Result<(), String> {
        match instr {
            Instr::Const { dst, value } => self.store(*dst, *value),

            Instr::MakeMap { dst, pairs } => {
                let map = HeapObj::Map(
                    pairs
                        .iter()
                        .map(|(k, v)| (self.load(*k), self.load(*v)))
                        .collect(),
                );
                let map_slot = self.heap.alloc(map);
                self.store(*dst, Value::Map(map_slot))
            }

            Instr::MakeClosure { dst, func_id } => {
                let captures = &self.program.funcs[func_id.as_usize()].captures;
                let values = captures.iter().map(|(_, src)| self.load(*src)).collect();
                let func_addr = self.heap.alloc(HeapObj::Closure(*func_id, values));
                self.store(*dst, Value::Closure(func_addr))
            }

            Instr::Apply { dst, lhs, rhs } => {
                let lhs = self.load(*lhs);
                let rhs = self.load(*rhs);

                self.apply(*dst, lhs, rhs)?;
            }

            Instr::Ret { src } => {
                let frame = self.frame_stack.pop().unwrap();
                let retval = frame.load(*src);
                match self.frame_stack.last_mut() {
                    Some(caller) => caller.store(frame.return_slot, retval),
                    None => self.result = retval,
                }
            }
        };
        Ok(())
    }

    fn load(&self, slot: Slot) -> Value {
        self.frame_stack.last().unwrap().load(slot)
    }

    fn store(&mut self, slot: Slot, value: Value) {
        self.frame_stack.last_mut().unwrap().store(slot, value);
    }

    fn apply(&mut self, dst: Slot, lhs: Value, rhs: Value) -> Result<(), String> {
        match lhs {
            Value::Map(addr) => self.store(dst, self.heap.deref(addr).map_lookup(rhs)?),
            Value::Closure(addr) => {
                let HeapObj::Closure(func_id, values) = self.heap.deref(addr) else {
                    unreachable!("closure value pointed at {:?}", addr)
                };

                let func = &self.program.funcs[func_id.as_usize()];
                let mut frame = Frame::new(func, rhs, dst);

                for (i, (capture_dst, _)) in func.captures.iter().enumerate() {
                    frame.store(*capture_dst, values[i]);
                }

                self.frame_stack.push(frame);
            }
            Value::Builtin(builtin) => self.store(dst, self.call_builtin(builtin, rhs)?),
            _ => return Err(format!("can't apply to a {lhs:?}")),
        }
        Ok(())
    }

    fn call_builtin(&self, builtin: Builtin, args: Value) -> Result<Value, String> {
        match builtin {
            Builtin::Add => self.add(args),
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
            return Err(format!("expected a tuple, got {value:?}"));
        };

        let map_obj = self.heap.deref(addr);

        let lhs = map_obj.map_lookup(Value::Int(0))?;
        let rhs = map_obj.map_lookup(Value::Int(1))?;

        Ok((lhs, rhs))
    }
}
