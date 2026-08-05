use std::{collections::HashMap, rc::Rc};

use crate::{
    builtin::Builtin,
    error::{ErrorKind, KartaError},
    interner::{AtomTable, StringLiteralId, StringLiteralTable},
    ir::{Function, FunctionId, HeapAddr, Instr, Program, Slot, Value},
    span::Span,
};

pub struct Eval<'a> {
    heap: &'a mut Heap,
    string_literal_table: &'a StringLiteralTable,
    program: Program,
    frame_stack: Vec<Frame>,
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

pub struct Heap {
    objs: Vec<HeapObj>,
    // Memoized string literal ID => addr map
    strings: HashMap<StringLiteralId, HeapAddr>,
}

impl Heap {
    pub fn new() -> Self {
        Self {
            objs: vec![HeapObj::Map(vec![])],
            strings: HashMap::new(),
        }
    }

    // pretty-please don't call from outside Heap
    fn alloc(&mut self, obj: HeapObj) -> HeapAddr {
        let retval = HeapAddr::new(self.objs.len() as u32);
        self.objs.push(obj);
        retval
    }

    fn alloc_map(&mut self, pairs: Vec<(Value, Value)>) -> HeapAddr {
        if pairs.is_empty() {
            HeapAddr::EMPTY_MAP
        } else {
            self.alloc(HeapObj::Map(pairs))
        }
    }

    fn alloc_closure(&mut self, func_id: FunctionId, values: Vec<Value>) -> HeapAddr {
        self.alloc(HeapObj::Closure(func_id, values))
    }

    fn deref(&self, addr: HeapAddr) -> &HeapObj {
        &self.objs[addr.as_usize()]
    }

    fn empty_map(&self) -> Value {
        Value::Map(HeapAddr::EMPTY_MAP)
    }

    fn map_lookup(&self, addr: HeapAddr, key: Value) -> Result<Value, KartaError> {
        Ok(self
            .as_map(addr)?
            .iter()
            .find_map(|(k, v)| (*k == key).then_some(*v))
            .unwrap_or(self.empty_map()))
    }

    fn as_map(&self, addr: HeapAddr) -> Result<&[(Value, Value)], KartaError> {
        match self.deref(addr) {
            HeapObj::Map(pairs) => Ok(pairs),
            HeapObj::Closure(..) => Err(KartaError {
                span: Span { start: 67, end: 67 },
                kind: ErrorKind::Unexpected {
                    expected: String::from("map"),
                    got: String::from("closure"),
                },
            }),
        }
    }

    fn materialize_string(
        &mut self,
        id: StringLiteralId,
        string_literal_table: &StringLiteralTable,
    ) -> HeapAddr {
        if !self.strings.contains_key(&id) {
            let str = string_literal_table.get(id);

            let mut addr = self.alloc_map(vec![]);
            for c in str.chars().rev() {
                addr = self.alloc_map(vec![
                    (Value::Atom(AtomTable::HEAD), Value::Char(c)),
                    (Value::Atom(AtomTable::TAIL), Value::Map(addr)),
                ])
            }

            self.strings.insert(id, addr);
        }

        self.strings[&id]
    }
}

pub struct ValueRef<'a> {
    heap: &'a Heap,
    value: Value,
}

impl<'a> ValueRef<'a> {
    /// Interpret this value as an integer.
    pub fn as_i64(&self) -> Option<i64> {
        self.value.as_i64()
    }

    /// Interpret this value as a float
    pub fn as_f64(&self) -> Option<f64> {
        self.value.as_f64()
    }

    /// Interpret this value as a char
    pub fn as_char(&self) -> Option<char> {
        self.value.as_char()
    }

    /// Determine whether this value is truthy
    pub fn is_truthy(&self) -> bool {
        self.value.is_truthy()
    }

    /// Interpret this value as a string.
    pub fn as_string(&self) -> Result<String, KartaError> {
        match self.value {
            Value::Map(_) => {
                let mut curr = self.value;
                let mut retval = String::from("");

                while curr.is_truthy() {
                    let Value::Map(addr) = curr else {
                        return Err(KartaError {
                            span: Span { start: 67, end: 67 },
                            kind: ErrorKind::Unexpected {
                                expected: String::from("a string"),
                                got: format!("{:?}", self.value),
                            },
                        });
                    };
                    let c = self
                        .heap
                        .map_lookup(addr, Value::Atom(AtomTable::HEAD))?
                        .as_char()
                        .ok_or(KartaError {
                            span: Span { start: 67, end: 67 },
                            kind: ErrorKind::Unexpected {
                                expected: String::from("a char"),
                                got: format!("{:?}", self.value),
                            },
                        })?;
                    retval.push(c);
                    curr = self.heap.map_lookup(addr, Value::Atom(AtomTable::TAIL))?;
                }

                Ok(retval)
            }
            _ => Err(KartaError {
                span: Span { start: 67, end: 67 },
                kind: ErrorKind::Unexpected {
                    expected: String::from("a string"),
                    got: format!("{:?}", self.value),
                },
            }),
        }
    }
}

impl<'a> Eval<'a> {
    pub fn new(
        heap: &'a mut Heap,
        string_literal_table: &'a StringLiteralTable,
        program: Program,
    ) -> Self {
        let entry = program.entry;
        let function = &program.funcs[entry.as_usize()];

        Self {
            frame_stack: vec![Frame::new(function, Value::Undefined, Slot::new(0))],
            program,
            heap,
            string_literal_table,
            result: Value::Undefined,
        }
    }

    pub fn eval(mut self) -> Result<ValueRef<'a>, KartaError> {
        while let Some(frame) = self.frame_stack.last_mut() {
            let instrs = frame.instrs.clone();

            let ip = frame.ip;
            frame.ip += 1;

            self.execute_instruction(&instrs[ip])?;
        }

        Ok(ValueRef {
            heap: self.heap,
            value: self.result,
        })
    }

    fn execute_instruction(&mut self, instr: &Instr) -> Result<(), KartaError> {
        match instr {
            Instr::Const { dst, value } => self.store(*dst, *value),

            Instr::Move { dst, src } => {
                self.store(*dst, self.load(*src));
            }

            Instr::MakeString { dst, id } => {
                let addr = self.heap.materialize_string(*id, self.string_literal_table);
                self.store(*dst, Value::Map(addr));
            }

            Instr::MakeMap { dst, pairs } => {
                let map_addr = self.heap.alloc_map(
                    pairs
                        .iter()
                        .map(|(k, v)| (self.load(*k), self.load(*v)))
                        .collect(),
                );
                self.store(*dst, Value::Map(map_addr))
            }

            Instr::MakeClosure { dst, func_id } => {
                let captures = &self.program.funcs[func_id.as_usize()].captures;
                let values = captures.iter().map(|(_, src)| self.load(*src)).collect();
                let func_addr = self.heap.alloc_closure(*func_id, values);
                self.store(*dst, Value::Closure(func_addr))
            }

            Instr::Apply { dst, lhs, rhs } => {
                let lhs = self.load(*lhs);
                let rhs = self.load(*rhs);

                self.apply(*dst, lhs, rhs)?;
            }

            Instr::Jump { target } => self.jump(*target),

            Instr::JumpIfFalse { target, cond } => {
                let cond_val = self.load(*cond);
                if !cond_val.is_truthy() {
                    self.jump(*target);
                }
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

    fn jump(&mut self, ip: usize) {
        self.frame_stack.last_mut().unwrap().ip = ip;
    }

    fn apply(&mut self, dst: Slot, lhs: Value, rhs: Value) -> Result<(), KartaError> {
        match lhs {
            Value::Map(addr) => self.store(dst, self.heap.map_lookup(addr, rhs)?),
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
            _ => {
                return Err(KartaError {
                    span: Span { start: 67, end: 67 },
                    kind: ErrorKind::CannotBinop {
                        verb: "apply",
                        lhs: format!("{lhs:?}"), // TODO: An eval-aware value renderer
                        rhs: format!("{rhs:?}"), // TODO: An eval-aware value renderer
                    },
                });
            }
        }
        Ok(())
    }

    fn call_builtin(&self, builtin: Builtin, args: Value) -> Result<Value, KartaError> {
        match builtin {
            Builtin::Eql => self.eql(args),
            Builtin::Neq => todo!("@neq"),
            Builtin::Lsr => todo!("@lsr"),
            Builtin::Lte => todo!("@lte"),
            Builtin::Gtr => todo!("@gtr"),
            Builtin::Gte => todo!("@gte"),
            Builtin::Add => self.arith(args, "add", |a, b| Ok(a + b), |a, b| a + b),
            Builtin::Sub => self.arith(args, "subtract", |a, b| Ok(a - b), |a, b| a - b),
            Builtin::Mul => self.arith(args, "multiply", |a, b| Ok(a * b), |a, b| a * b),
            Builtin::Div => self.arith(
                args,
                "divide",
                |a, b| {
                    a.checked_div(b).ok_or(KartaError {
                        span: Span { start: 67, end: 67 },
                        kind: ErrorKind::DivisionByZero,
                    })
                },
                |a, b| a / b,
            ),
            Builtin::Mod => todo!("@mod"),
            Builtin::And => todo!("@and"),
            Builtin::Or => todo!("@or"),
            Builtin::Not => todo!("@not"),
        }
    }

    fn eql(&self, args: Value) -> Result<Value, KartaError> {
        let (lhs, rhs) = self.get_pair(args)?;

        match (lhs, rhs) {
            (Value::Int(_), Value::Int(_))
            | (Value::Float(_), Value::Float(_))
            | (Value::Char(_), Value::Char(_))
            | (Value::Atom(_), Value::Atom(_))
            | (Value::Builtin(_), Value::Builtin(_))
            | (Value::Closure(_), Value::Closure(_)) => Ok(self.make_bool(lhs == rhs)),

            // TODO: actual structural map object equality, for now just compare heap addrs
            (Value::Map(l_addr), Value::Map(r_addr)) => Ok(self.make_bool(l_addr == r_addr)),

            (Value::Undefined, _) => unreachable!("lhs was undefined"),
            (_, Value::Undefined) => unreachable!("rhs was undefined"),

            (lhs, rhs) => Err(KartaError {
                span: Span { start: 67, end: 67 },
                kind: ErrorKind::CannotBinop {
                    verb: "compare",
                    lhs: format!("{lhs:?}"), // TODO: An eval-aware value renderer
                    rhs: format!("{rhs:?}"), // TODO: An eval-aware value renderer
                },
            }),
        }
    }

    fn arith(
        &self,
        args: Value,
        verb: &'static str,
        int_op: impl FnOnce(i64, i64) -> Result<i64, KartaError>,
        float_op: impl FnOnce(f64, f64) -> f64,
    ) -> Result<Value, KartaError> {
        let (lhs, rhs) = self.get_pair(args)?;

        match (lhs, rhs) {
            (Value::Int(x), Value::Int(y)) => Ok(Value::Int(int_op(x, y)?)),
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(float_op(x, y))),

            (Value::Undefined, _) => unreachable!("lhs was undefined"),
            (_, Value::Undefined) => unreachable!("rhs was undefined"),

            _ => Err(KartaError {
                span: Span { start: 67, end: 67 },
                kind: ErrorKind::CannotBinop {
                    verb,
                    lhs: format!("{lhs:?}"), // TODO: An eval-aware value renderer
                    rhs: format!("{rhs:?}"), // TODO: An eval-aware value renderer
                },
            }),
        }
    }

    fn get_pair(&self, value: Value) -> Result<(Value, Value), KartaError> {
        let Value::Map(addr) = value else {
            return Err(KartaError {
                span: Span { start: 67, end: 67 },
                kind: ErrorKind::Unexpected {
                    expected: String::from("tuple"),
                    got: format!("{value:?}"), // TODO: An eval-aware value renderer
                },
            });
        };

        let lhs = self.heap.map_lookup(addr, Value::Int(0))?;
        let rhs = self.heap.map_lookup(addr, Value::Int(1))?;

        Ok((lhs, rhs))
    }

    fn make_bool(&self, cond: bool) -> Value {
        if cond {
            Value::Atom(AtomTable::TRUE)
        } else {
            self.heap.empty_map()
        }
    }
}
