use std::{collections::HashMap, fmt::Display, rc::Rc};

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
    atoms: &'a AtomTable,
    program: Program,
    frame_stack: Vec<Frame>,
    result: Value,
}

#[derive(Clone, Copy)]
enum EvalMode {
    /// We want to eval this function fully, and run its body
    Normal,
    /// We just want to see if this function accepts the args
    Probe,
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
    /// Whether we're fully eval-ing or just probing
    mode: EvalMode,
}

impl Frame {
    fn new(func: &Function, arg: Value, return_slot: Slot, mode: EvalMode) -> Self {
        let mut slots = vec![Value::Undefined; func.slots_used as usize];
        slots[0] = arg;

        Self {
            instrs: func.instructions.clone(),
            slots,
            ip: 0,
            return_slot,
            mode,
        }
    }

    fn store(&mut self, dst: Slot, val: Value) {
        self.slots[dst.as_usize()] = val
    }

    fn load(&self, dst: Slot) -> Value {
        self.slots[dst.as_usize()]
    }
}

#[derive(Debug)]
enum HeapObj {
    Map(Vec<(Value, Value)>),
    Closure(FunctionId, Vec<Value>),
}

#[derive(Debug)]
pub struct Heap {
    objs: Vec<HeapObj>,
    // Memoized string literal ID => addr map
    strings: HashMap<StringLiteralId, HeapAddr>,
}

impl Heap {
    const EMPTY_MAP: Value = Value::Map(HeapAddr::EMPTY_MAP);

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
            return HeapAddr::EMPTY_MAP;
        }

        let mut deduped = Vec::with_capacity(pairs.len());
        for (k, v) in pairs {
            match deduped.iter().position(|(dk, _)| self.values_eq(*dk, k)) {
                Some(i) => deduped[i] = (k, v),
                None => deduped.push((k, v)),
            }
        }
        self.alloc(HeapObj::Map(deduped))
    }

    fn alloc_closure(&mut self, func_id: FunctionId, values: Vec<Value>) -> HeapAddr {
        self.alloc(HeapObj::Closure(func_id, values))
    }

    fn deref(&self, addr: HeapAddr) -> &HeapObj {
        &self.objs[addr.as_usize()]
    }

    /// Returns the value, if present, or None
    fn map_get(&self, addr: HeapAddr, key: Value) -> Result<Option<Value>, KartaError> {
        Ok(self
            .as_map(addr)?
            .iter()
            .find_map(|(k, v)| self.values_eq(*k, key).then_some(*v)))
    }

    /// Wraps `map_get`, if the value isn't present, returns the empy map
    fn map_lookup(&self, addr: HeapAddr, key: Value) -> Result<Value, KartaError> {
        Ok(self.map_get(addr, key)?.unwrap_or(Self::EMPTY_MAP))
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

    fn values_eq(&self, a: Value, b: Value) -> bool {
        match (a, b) {
            (Value::Map(a_addr), Value::Map(b_addr)) => {
                if a_addr == b_addr {
                    // early out for identical maps
                    return true;
                }

                // get their objs
                let (HeapObj::Map(a_pairs), HeapObj::Map(b_pairs)) =
                    (self.deref(a_addr), self.deref(b_addr))
                else {
                    unreachable!("map addr didnt refer to map");
                };

                // check that b has every one of a's keys, and that they map to the same thing
                a_pairs.len() == b_pairs.len()
                    && a_pairs.iter().all(|(ak, av)| {
                        b_pairs
                            .iter()
                            .any(|(bk, bv)| self.values_eq(*ak, *bk) && self.values_eq(*av, *bv))
                    })
            }

            (Value::Undefined, _) => unreachable!("lhs was undefined"),
            (_, Value::Undefined) => unreachable!("rhs was undefined"),

            _ => a == b,
        }
    }

    fn closure_func(&self, addr: HeapAddr) -> Option<FunctionId> {
        match self.deref(addr) {
            HeapObj::Closure(func_id, _) => Some(*func_id),
            HeapObj::Map(_) => None,
        }
    }

    fn set_captures(&mut self, addr: HeapAddr, values: Vec<Value>) {
        match &mut self.objs[addr.as_usize()] {
            HeapObj::Closure(_, vals) => *vals = values,
            HeapObj::Map(_) => unreachable!("set_captures on a map"),
        }
    }

    fn materialize_string(
        &mut self,
        id: StringLiteralId,
        string_literal_table: &StringLiteralTable,
    ) -> HeapAddr {
        if let Some(addr) = self.strings.get(&id) {
            return *addr;
        }

        let str = string_literal_table.get(id);

        let mut addr = HeapAddr::EMPTY_MAP;
        for c in str.chars().rev() {
            addr = self.alloc_map(vec![
                (Value::HEAD, Value::Char(c)),
                (Value::TAIL, Value::Map(addr)),
            ])
        }

        self.strings.insert(id, addr);
        addr
    }

    fn cons_iter(&self, addr: HeapAddr) -> impl Iterator<Item = Value> + '_ {
        std::iter::successors(Some(addr), move |a| {
            match self.map_lookup(*a, Value::TAIL).ok()? {
                Value::Map(next) if next != HeapAddr::EMPTY_MAP => Some(next),
                _ => None,
            }
        })
        .filter(|a| *a != HeapAddr::EMPTY_MAP)
        .filter_map(|a| self.map_lookup(a, Value::HEAD).ok())
    }

    fn list_keys(&self, addr: HeapAddr) -> bool {
        let HeapObj::Map(pairs) = self.deref(addr) else {
            return false;
        };

        pairs.len() == 2
            && pairs.iter().any(|(k, _)| matches!(*k, Value::HEAD))
            && pairs.iter().any(|(k, _)| matches!(*k, Value::TAIL))
    }

    fn all_char_values(&self, addr: HeapAddr) -> bool {
        self.cons_iter(addr).all(|v| matches!(v, Value::Char(_)))
    }

    fn tuple_keys(&self, addr: HeapAddr) -> bool {
        let HeapObj::Map(pairs) = self.deref(addr) else {
            return false;
        };

        let len = pairs.len();

        (0..len).into_iter().all(|i| {
            pairs
                .iter()
                .any(|(k, _)| matches!(*k, Value::Int(j) if j as usize == i))
        })
    }

    fn set_values(&self, addr: HeapAddr) -> bool {
        let HeapObj::Map(pairs) = self.deref(addr) else {
            return false;
        };

        pairs.iter().all(|(_, v)| matches!(*v, Value::TRUE))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ValueRef<'a> {
    heap: &'a Heap,
    atoms: &'a AtomTable,
    value: Value,
}

impl<'a> ValueRef<'a> {
    fn fmt_depth(&self, f: &mut std::fmt::Formatter<'_>, depth: usize) -> std::fmt::Result {
        if depth > 10 {
            return write!(f, "...");
        }

        match self.value {
            Value::Undefined => write!(f, "undefined"),
            Value::Int(n) => write!(f, "{n}"),
            Value::Float(n) => write!(f, "{n:?}"), // Print with :? to get the .0 decimal
            Value::Char(c) => write!(f, "'{c}'"),
            Value::Atom(id) => {
                let str = self.atoms.get(id);
                write!(f, "{str}") // already does the .
            }
            Value::Builtin(builtin) => write!(f, "{}", builtin.repr()), // already does the @
            Value::Closure(heap_addr) => write!(f, "<closure:{heap_addr}>"),
            Value::Map(heap_addr) => {
                if heap_addr == HeapAddr::EMPTY_MAP {
                    return write!(f, "{{}}");
                }

                if self.heap.list_keys(heap_addr) {
                    if self.heap.all_char_values(heap_addr) {
                        return self.fmt_string(f, heap_addr);
                    } else {
                        return self.fmt_list(f, heap_addr, depth);
                    }
                }

                if self.heap.tuple_keys(heap_addr) {
                    return self.fmt_tuple(f, heap_addr, depth);
                }

                if self.heap.set_values(heap_addr) {
                    return self.fmt_set(f, heap_addr, depth);
                }

                self.fmt_map(f, heap_addr, depth)
            }
        }
    }

    fn fmt_string(&self, f: &mut std::fmt::Formatter<'_>, addr: HeapAddr) -> std::fmt::Result {
        write!(f, "\"")?;

        for elem in self.heap.cons_iter(addr) {
            let Value::Char(c) = elem else {
                unreachable!("already checked");
            };
            write!(f, "{c}")?;
        }

        write!(f, "\"")
    }

    fn fmt_list(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        addr: HeapAddr,
        depth: usize,
    ) -> std::fmt::Result {
        write!(f, "[")?;

        for (i, elem) in self.heap.cons_iter(addr).enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            self.child(elem).fmt_depth(f, depth + 1)?;
        }
        write!(f, "]")
    }

    fn fmt_tuple(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        addr: HeapAddr,
        depth: usize,
    ) -> std::fmt::Result {
        write!(f, "(")?;

        let HeapObj::Map(pairs) = self.heap.deref(addr) else {
            unreachable!("already checked");
        };

        for i in 0..pairs.len() {
            if i > 0 {
                write!(f, ", ")?;
            }
            let elem = self.heap.map_lookup(addr, Value::Int(i as i64)).unwrap();

            self.child(elem).fmt_depth(f, depth + 1)?;
        }

        write!(f, ")")
    }

    fn fmt_set(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        addr: HeapAddr,
        depth: usize,
    ) -> std::fmt::Result {
        write!(f, "{{")?;

        let HeapObj::Map(pairs) = self.heap.deref(addr) else {
            unreachable!("already checked");
        };

        for (i, (k, _)) in pairs.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            self.child(*k).fmt_depth(f, depth + 1)?;
        }

        write!(f, "}}")
    }

    fn fmt_map(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        addr: HeapAddr,
        depth: usize,
    ) -> std::fmt::Result {
        write!(f, "{{")?;

        let HeapObj::Map(pairs) = self.heap.deref(addr) else {
            unreachable!("already checked");
        };

        for (i, (k, v)) in pairs.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            self.child(*k).fmt_depth(f, depth + 1)?;

            write!(f, " = ")?;

            self.child(*v).fmt_depth(f, depth + 1)?;
        }

        write!(f, "}}")
    }
}

impl<'a> Display for ValueRef<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_depth(f, 0)
    }
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
        let mut retval = String::new();

        let Value::Map(addr) = self.value else {
            return Err(KartaError {
                span: Span { start: 67, end: 67 },
                kind: ErrorKind::Unexpected {
                    expected: String::from("a string"),
                    got: format!("{:?}", self.value),
                },
            });
        };

        for c in self.heap.cons_iter(addr) {
            let c = c.as_char().ok_or(KartaError {
                span: Span { start: 67, end: 67 },
                kind: ErrorKind::Unexpected {
                    expected: String::from("a char"),
                    got: format!("{:?}", c),
                },
            })?;
            retval.push(c);
        }

        Ok(retval)
    }

    fn child(&'_ self, value: Value) -> ValueRef<'_> {
        ValueRef { value, ..*self }
    }
}

impl<'a> Eval<'a> {
    pub fn new(
        heap: &'a mut Heap,
        string_literal_table: &'a StringLiteralTable,
        atoms: &'a AtomTable,
        program: Program,
    ) -> Self {
        let entry = program.entry;
        let function = &program.funcs[entry.as_usize()];

        Self {
            frame_stack: vec![Frame::new(
                function,
                Value::Undefined,
                Slot::new(0),
                EvalMode::Normal,
            )],
            program,
            atoms,
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
            atoms: self.atoms,
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
                let values = self.capture_values(*func_id);
                let func_addr = self.heap.alloc_closure(*func_id, values);
                self.store(*dst, Value::Closure(func_addr))
            }

            Instr::FillCaptures { slot } => {
                if let Value::Closure(addr) = self.load(*slot) {
                    if let Some(func_id) = self.heap.closure_func(addr) {
                        let values = self.capture_values(func_id);
                        self.heap.set_captures(addr, values);
                    }
                }
            }

            Instr::Apply { dst, lhs, rhs } => {
                let lhs = self.load(*lhs);
                let rhs = self.load(*rhs);

                self.apply(*dst, lhs, rhs, EvalMode::Normal)?;
            }

            Instr::GetKey { dst, src, key } => {
                let src_val = self.load(*src);
                let key_val = self.load(*key);
                self.apply(*dst, src_val, key_val, EvalMode::Normal)?;
            }

            Instr::TestConst { dst, src, value } => {
                let a = self.load(*src);
                let b = self.load(*value);
                self.store(*dst, self.make_bool(self.heap.values_eq(a, b)));
            }

            Instr::TestHasKey { dst, src, key } => {
                let src_val = self.load(*src);
                let key_val = self.load(*key);
                match self.apply(*dst, src_val, key_val, EvalMode::Probe) {
                    Ok(_) => {}

                    // If applying failed (like trying to apply to a non-applicable) then store false, it doesn't have the key
                    Err(_) => self.store(*dst, Heap::EMPTY_MAP),
                }
            }

            Instr::TestTupleLength { dst, src, len } => {
                let length_fits = match self.load(*src) {
                    Value::Map(addr) => match self.heap.deref(addr) {
                        HeapObj::Map(pairs) => {
                            // check length and contiguity
                            pairs.len() == *len
                                && (0..*len)
                                    .all(|i| pairs.iter().any(|(k, _)| *k == Value::Int(i as i64)))
                        }
                        HeapObj::Closure(_, _) => false,
                    },

                    _ => false, // If not even a map, then store falsey
                };

                self.store(*dst, self.make_bool(length_fits))
            }

            Instr::Jump { target } => self.jump(*target),

            Instr::JumpIfFalse { target, cond } => {
                let cond_val = self.load(*cond);
                if !cond_val.is_truthy() {
                    self.jump(*target);
                }
            }

            Instr::Accept => {
                match self.mode() {
                    // Normal eval, clause acceptance is a no-op
                    EvalMode::Normal => {}

                    // Probing for acceptance and we got it, return .t
                    EvalMode::Probe => self.ret(Value::TRUE),
                }
            }

            Instr::Reject => {
                match self.mode() {
                    // Attempting to eval normally, but no clause accepted. Panic
                    EvalMode::Normal => {
                        return Err(KartaError {
                            span: Span { start: 67, end: 67 },
                            kind: ErrorKind::NonTotal,
                        })
                    }

                    // Just checking if this any clause of this function accepted, none did. Return `{}`
                    EvalMode::Probe => self.ret(Heap::EMPTY_MAP),
                }
            }

            Instr::Ret { src } => {
                let retval = self.load(*src);
                self.ret(retval);
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

    fn ret(&mut self, retval: Value) {
        let frame = self.frame_stack.pop().unwrap();
        match self.frame_stack.last_mut() {
            Some(caller) => caller.store(frame.return_slot, retval),
            None => self.result = retval,
        }
    }

    fn mode(&self) -> EvalMode {
        self.frame_stack.last().unwrap().mode
    }

    fn capture_values(&self, func_id: FunctionId) -> Vec<Value> {
        self.program.funcs[func_id.as_usize()]
            .captures
            .iter()
            .map(|(_, src)| self.load(*src))
            .collect()
    }

    fn apply(
        &mut self,
        dst: Slot,
        lhs: Value,
        rhs: Value,
        mode: EvalMode,
    ) -> Result<(), KartaError> {
        match lhs {
            Value::Map(addr) => match mode {
                EvalMode::Normal => self.store(dst, self.heap.map_lookup(addr, rhs)?),
                EvalMode::Probe => {
                    let res = self.heap.map_get(addr, rhs)?.is_some();
                    self.store(dst, self.make_bool(res))
                }
            },

            Value::Closure(addr) => {
                let HeapObj::Closure(func_id, values) = self.heap.deref(addr) else {
                    unreachable!("closure value pointed at {:?}", addr)
                };

                let func = &self.program.funcs[func_id.as_usize()];
                let mut frame = Frame::new(func, rhs, dst, mode);

                for (i, (capture_dst, _)) in func.captures.iter().enumerate() {
                    frame.store(*capture_dst, values[i]);
                }

                self.frame_stack.push(frame);
            }

            Value::Builtin(Builtin::Accepts) => {
                let (f, x) = self.get_pair(rhs)?;
                match mode {
                    EvalMode::Normal => self.apply(dst, f, x, EvalMode::Probe)?,

                    EvalMode::Probe => self.store(dst, self.make_bool(true)),
                }
            }

            Value::Builtin(builtin) => match mode {
                EvalMode::Normal => self.store(dst, self.call_builtin(builtin, rhs)?),
                EvalMode::Probe => {
                    let res = self.call_builtin(builtin, rhs).is_ok();
                    self.store(dst, self.make_bool(res))
                }
            },

            _ => {
                return Err(KartaError {
                    span: Span { start: 67, end: 67 },
                    kind: ErrorKind::CannotBinop {
                        verb: "apply",
                        lhs: format!("{}", self.make_value_ref(lhs)),
                        rhs: format!("{}", self.make_value_ref(rhs)),
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
            Builtin::Lsr => self.arith(
                args,
                "compare",
                |a, b| Ok(self.make_bool(a < b)),
                |a, b| self.make_bool(a < b),
            ),
            Builtin::Lte => self.arith(
                args,
                "compare",
                |a, b| Ok(self.make_bool(a <= b)),
                |a, b| self.make_bool(a <= b),
            ),
            Builtin::Gtr => self.arith(
                args,
                "compare",
                |a, b| Ok(self.make_bool(a > b)),
                |a, b| self.make_bool(a > b),
            ),
            Builtin::Gte => self.arith(
                args,
                "compare",
                |a, b| Ok(self.make_bool(a >= b)),
                |a, b| self.make_bool(a >= b),
            ),
            Builtin::Neg => self.neg(args),
            Builtin::Add => self.arith(
                args,
                "add",
                |a, b| Ok(Value::Int(a + b)),
                |a, b| Value::Float(a + b),
            ),
            Builtin::Sub => self.arith(
                args,
                "subtract",
                |a, b| Ok(Value::Int(a - b)),
                |a, b| Value::Float(a - b),
            ),
            Builtin::Mul => self.arith(
                args,
                "multiply",
                |a, b| Ok(Value::Int(a * b)),
                |a, b| Value::Float(a * b),
            ),
            Builtin::Div => self.arith(
                args,
                "divide",
                |a, b| {
                    a.checked_div(b)
                        .ok_or(KartaError {
                            span: Span { start: 67, end: 67 },
                            kind: ErrorKind::DivisionByZero,
                        })
                        .map(Value::Int)
                },
                |a, b| Value::Float(a / b),
            ),
            Builtin::Mod => todo!("@mod"),
            Builtin::And => todo!("@and"),
            Builtin::Or => todo!("@or"),
            Builtin::Not => todo!("@not"),
            Builtin::Accepts => unreachable!("intercepted in apply"),
        }
    }

    fn eql(&self, args: Value) -> Result<Value, KartaError> {
        let (lhs, rhs) = self.get_pair(args)?;
        Ok(self.make_bool(self.heap.values_eq(lhs, rhs)))
    }

    fn arith(
        &self,
        args: Value,
        verb: &'static str,
        int_op: impl FnOnce(i64, i64) -> Result<Value, KartaError>,
        float_op: impl FnOnce(f64, f64) -> Value,
    ) -> Result<Value, KartaError> {
        let (lhs, rhs) = self.get_pair(args)?;

        match (lhs, rhs) {
            (Value::Int(x), Value::Int(y)) => Ok(int_op(x, y)?),
            (Value::Float(x), Value::Float(y)) => Ok(float_op(x, y)),

            (Value::Undefined, _) => unreachable!("lhs was undefined"),
            (_, Value::Undefined) => unreachable!("rhs was undefined"),

            _ => Err(KartaError {
                span: Span { start: 67, end: 67 },
                kind: ErrorKind::CannotBinop {
                    verb,
                    lhs: format!("{}", self.make_value_ref(lhs)),
                    rhs: format!("{}", self.make_value_ref(rhs)),
                },
            }),
        }
    }

    fn neg(&self, arg: Value) -> Result<Value, KartaError> {
        match arg {
            Value::Int(x) => Ok(Value::Int(-x)),
            Value::Float(x) => Ok(Value::Float(-x)),

            Value::Undefined => unreachable!("arg was undefined"),

            _ => Err(KartaError {
                span: Span { start: 67, end: 67 },
                kind: ErrorKind::CannotUnop {
                    verb: "negate",
                    expr: format!("{}", self.make_value_ref(arg)),
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
                    got: format!("{}", self.make_value_ref(value)),
                },
            });
        };

        let lhs = self.heap.map_lookup(addr, Value::Int(0))?;
        let rhs = self.heap.map_lookup(addr, Value::Int(1))?;

        Ok((lhs, rhs))
    }

    fn make_bool(&self, cond: bool) -> Value {
        if cond {
            Value::TRUE
        } else {
            Heap::EMPTY_MAP
        }
    }

    fn make_value_ref(&'_ self, value: Value) -> ValueRef<'_> {
        ValueRef {
            heap: self.heap,
            atoms: self.atoms,
            value,
        }
    }
}
