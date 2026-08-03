use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::{Ast, AstHeap, AstId},
    builtin::Builtin,
    elaborate::Elaboration,
    error::{ErrorKind, KartaError},
    interner::AtomId,
    pattern::PatternId,
    scope::DefId,
    span::Span,
};

pub struct Function {
    pub instructions: Rc<[Instr]>,
    pub slots_used: u32,
    /// Vec of `(dst, src)`, where dst is the slot in this function's frame that the value is copied into at
    /// call time, and src is the slot in the enclosing frame it is read from at `MakeClosure` time.
    pub captures: Vec<(Slot, Slot)>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FunctionId(u32);

impl FunctionId {
    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug)]
pub enum Instr {
    Const { dst: Slot, value: Value },

    Move { dst: Slot, src: Slot },

    MakeMap { dst: Slot, pairs: Vec<(Slot, Slot)> },

    MakeClosure { dst: Slot, func_id: FunctionId },

    Apply { dst: Slot, lhs: Slot, rhs: Slot },

    Jump { target: usize },

    JumpIfFalse { target: usize, cond: Slot },

    Ret { src: Slot },
}

#[must_use]
struct PatchSite(usize);

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Value {
    Undefined,
    Int(i64),
    Float(f64),
    Char(char),
    Atom(AtomId),
    Builtin(Builtin),
    Map(HeapAddr),
    Closure(HeapAddr),
}

impl Value {
    /// Interpret this value as an integer. Does some basic conversions
    pub fn as_int<T>(&self) -> Result<T, KartaError>
    where
        T: From<i64>,
    {
        match self {
            Value::Int(x) => Ok(T::from(*x)),
            Value::Float(x) => Ok(T::from(*x as i64)),
            Value::Char(x) => Ok(T::from(*x as i64)),
            _ => Err(KartaError {
                span: Span { start: 67, end: 67 },
                kind: ErrorKind::CannotBinop {
                    verb: "convert between",
                    lhs: format!("{:?}", *self),
                    rhs: String::from("int"),
                },
            }),
        }
    }

    /// Interpret this value as an integer. Does some basic conversions
    pub fn as_float<T>(&self) -> Result<T, KartaError>
    where
        T: From<f64>,
    {
        match self {
            Value::Int(x) => Ok(T::from(*x as f64)),
            Value::Float(x) => Ok(T::from(*x)),
            _ => Err(KartaError {
                span: Span { start: 67, end: 67 },
                kind: ErrorKind::CannotBinop {
                    verb: "convert between",
                    lhs: format!("{:?}", *self),
                    rhs: String::from("float"),
                },
            }),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Slot(u32);

impl Slot {
    pub fn new(x: u32) -> Slot {
        Slot(x)
    }

    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HeapAddr(u32);

impl HeapAddr {
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

pub struct Program {
    pub funcs: Vec<Function>,
    pub entry: FunctionId,
}

pub struct Lowerer<'a> {
    asts: &'a AstHeap,
    elab: &'a Elaboration,
    funcs: Vec<Function>,
    stack: Vec<FnState>,
}

struct FnState {
    scope: HashMap<DefId, Slot>,
    slots_used: u32,
    captures: Vec<(Slot, Slot)>,
    instructions: Vec<Instr>,
}

impl FnState {
    fn into_function(self) -> Function {
        Function {
            instructions: self.instructions.into(),
            slots_used: self.slots_used,
            captures: self.captures,
        }
    }
}

impl<'a> Lowerer<'a> {
    pub fn new(asts: &'a AstHeap, elab: &'a Elaboration) -> Self {
        Self {
            asts,
            elab,
            funcs: Vec::new(),
            stack: Vec::new(),
        }
    }

    pub fn lower(mut self, root: AstId) -> Program {
        self.push_fn();
        let body_slot = self.lower_ast(root);
        self.emit(Instr::Ret { src: body_slot });

        let entry = self.finish_fn();

        Program {
            funcs: self.funcs,
            entry,
        }
    }

    pub fn lower_ast(&mut self, id: AstId) -> Slot {
        let ast = self.asts.get(id).expect("invalid AST id");

        match ast {
            Ast::Int(n) => self.lower_const(Value::Int(*n)),
            Ast::Float(n) => self.lower_const(Value::Float(*n)),
            Ast::Atom(id) => self.lower_const(Value::Atom(*id)),
            Ast::BuiltinFunction(builtin) => self.lower_const(Value::Builtin(*builtin)),
            Ast::Error => unreachable!("I only lower the finest of ASTs"),

            Ast::Map(pairs) => {
                let pairs = pairs
                    .iter()
                    .map(|(k, v)| (self.lower_ast(*k), self.lower_ast(*v)))
                    .collect();
                self.lower_map(pairs)
            }

            Ast::Tuple(elems) => {
                let pairs = elems
                    .iter()
                    .enumerate()
                    .map(|(i, elem)| {
                        let index = self.lower_const(Value::Int(i as i64));
                        let elem_val = self.lower_ast(*elem);
                        (index, elem_val)
                    })
                    .collect();
                self.lower_map(pairs)
            }

            Ast::Identifier(_) => {
                let def = self.elab.refer(id);
                self.resolve(def, self.stack.len() - 1)
            }

            Ast::Apply(lhs, rhs) => {
                let dst = self.new_slot();
                let lhs = self.lower_ast(*lhs);
                let rhs = self.lower_ast(*rhs);
                self.emit(Instr::Apply { dst, lhs, rhs });
                dst
            }

            Ast::Let(bindings, expr) => {
                for binding in bindings {
                    self.lower_ast(*binding);
                }
                self.lower_ast(*expr)
            }

            Ast::Binding { params, rhs, .. } => {
                let def = self.elab.define(id);
                let slot = self.lower_curried(params, *rhs);
                self.top_fn_mut().scope.insert(def, slot);
                slot
            }

            Ast::Lambda { arg, body } => self.lower_lambda(*arg, |this| this.lower_ast(*body)),

            Ast::If(conds, else_body) => {
                let result = self.new_slot();
                let mut end_sites = Vec::new();

                for (cond_ast, body_ast) in conds {
                    let cond = self.lower_ast(*cond_ast);
                    let else_site = self.emit_patchable(Instr::JumpIfFalse {
                        target: usize::MAX,
                        cond,
                    });

                    let body_slot = self.lower_ast(*body_ast);
                    self.emit(Instr::Move {
                        dst: result,
                        src: body_slot,
                    });
                    end_sites.push(self.emit_patchable(Instr::Jump { target: usize::MAX }));

                    self.patch_here(else_site);
                }

                let else_slot = self.lower_ast(*else_body);
                self.emit(Instr::Move {
                    dst: result,
                    src: else_slot,
                });

                for site in end_sites {
                    self.patch_here(site);
                }

                result
            }

            _ => todo!("not implemented: {:?}", ast),
        }
    }

    fn lower_const(&mut self, value: Value) -> Slot {
        let dst = self.new_slot();
        self.emit(Instr::Const { dst, value });
        dst
    }

    fn lower_map(&mut self, pairs: Vec<(Slot, Slot)>) -> Slot {
        let dst = self.new_slot();
        self.emit(Instr::MakeMap { dst, pairs });
        dst
    }

    /// Takes a bunch of params [p0, p1, ..., pn], a body, and lowers it to `\p0 -> \p1 -> ... -> \pn -> body`
    fn lower_curried(&mut self, params: &[PatternId], body: AstId) -> Slot {
        match params.split_first() {
            None => self.lower_ast(body),
            Some((first, rest)) => self.lower_lambda(*first, |this| this.lower_curried(rest, body)),
        }
    }

    fn lower_lambda(&mut self, arg: PatternId, lower_body: impl FnOnce(&mut Self) -> Slot) -> Slot {
        // Push a new function to the stack, fill it in
        self.push_fn();
        let def = self.elab.pattern_define(arg);
        let param_slot = self.new_slot();
        self.top_fn_mut().scope.insert(def, param_slot);
        let body_slot = lower_body(self);
        self.emit(Instr::Ret { src: body_slot });

        let func_id = self.finish_fn();

        let dst = self.new_slot();
        self.emit(Instr::MakeClosure { dst, func_id });
        dst
    }

    fn push_fn(&mut self) {
        self.stack.push(FnState {
            scope: HashMap::new(),
            slots_used: 0,
            captures: Vec::new(),
            instructions: Vec::new(),
        })
    }

    fn finish_fn(&mut self) -> FunctionId {
        let function = self.stack.pop().unwrap().into_function();
        let id = FunctionId(self.funcs.len() as u32);
        self.funcs.push(function);
        id
    }

    fn top_fn(&self) -> &FnState {
        self.stack.last().unwrap()
    }

    fn top_fn_mut(&mut self) -> &mut FnState {
        self.stack.last_mut().unwrap()
    }

    fn resolve(&mut self, def: DefId, level: usize) -> Slot {
        if let Some(slot) = self.stack[level].scope.get(&def) {
            return *slot;
        }

        let Some(parent) = level.checked_sub(1) else {
            panic!("unresolved identifier: {def:?}");
        };
        let parent_slot = self.resolve(def, parent);
        let dst = self.new_slot_at(level);
        self.stack[level].scope.insert(def, dst);
        self.stack[level].captures.push((dst, parent_slot));
        dst
    }

    fn new_slot(&mut self) -> Slot {
        self.new_slot_at(self.stack.len() - 1)
    }

    fn new_slot_at(&mut self, level: usize) -> Slot {
        let retval = Slot(self.stack[level].slots_used);
        self.stack[level].slots_used += 1;
        retval
    }

    fn emit(&mut self, instr: Instr) {
        self.top_fn_mut().instructions.push(instr);
    }

    fn emit_patchable(&mut self, instr: Instr) -> PatchSite {
        let idx = self.top_fn().instructions.len();
        self.emit(instr);
        PatchSite(idx)
    }

    fn patch_here(&mut self, site: PatchSite) {
        let frame = self.top_fn_mut();
        let curr_idx = frame.instructions.len();

        match &mut frame.instructions[site.0] {
            Instr::JumpIfFalse { target, .. } | Instr::Jump { target } => *target = curr_idx,
            other => unreachable!("patch site pointed at {other:?}"),
        }
    }
}
