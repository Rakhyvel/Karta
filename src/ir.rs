use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::{Ast, AstHeap, AstId},
    builtin::Builtin,
    elaborate::Elaboration,
    interner::{AtomId, AtomTable, StringLiteralId},
    pattern::{Pattern, PatternHeap, PatternId},
    scope::DefId,
};

#[derive(Debug)]
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
    Const {
        dst: Slot,
        value: Value,
    },

    Move {
        dst: Slot,
        src: Slot,
    },

    MakeString {
        dst: Slot,
        id: StringLiteralId,
    },

    MakeMap {
        dst: Slot,
        pairs: Vec<(Slot, Slot)>,
    },

    MakeClosure {
        dst: Slot,
        func_id: FunctionId,
    },

    FillCaptures {
        slot: Slot,
    },

    Apply {
        dst: Slot,
        lhs: Slot,
        rhs: Slot,
    },

    GetKey {
        dst: Slot,
        src: Slot,
        key: Slot,
    },

    TestConst {
        dst: Slot,
        src: Slot,
        value: Slot,
    },

    TestHasKey {
        dst: Slot,
        src: Slot,
        key: Slot,
    },

    TestTupleLength {
        dst: Slot,
        src: Slot,
        len: usize,
    },

    Jump {
        target: usize,
    },

    JumpIfFalse {
        target: usize,
        cond: Slot,
    },

    /// Function accepts the arg for one of its clauses, no-op in normal eval mode
    Accept,

    /// Function rejects the arg for all clauses, panic in normal eval mode
    Reject,

    Ret {
        src: Slot,
    },
}

#[must_use]
struct PatchSite(usize);

#[derive(Debug, PartialEq, Clone, Copy)]
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
    /// Interpret this value as an integer
    pub fn as_i64(&self) -> Option<i64> {
        debug_assert!(!matches!(*self, Value::Undefined));
        match self {
            Value::Int(x) => Some(*x),
            _ => None,
        }
    }

    /// Interpret this value as a float
    pub fn as_f64(&self) -> Option<f64> {
        debug_assert!(!matches!(*self, Value::Undefined));
        match self {
            Value::Float(x) => Some(*x),
            _ => None,
        }
    }

    /// Interpret this value as a char
    pub fn as_char(&self) -> Option<char> {
        debug_assert!(!matches!(*self, Value::Undefined));
        match self {
            Value::Char(x) => Some(*x),
            _ => None,
        }
    }

    /// Determine whether this value is truthy
    pub fn is_truthy(&self) -> bool {
        debug_assert!(!matches!(*self, Value::Undefined));
        !matches!(*self, Value::Map(HeapAddr::EMPTY_MAP))
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
    pub const EMPTY_MAP: HeapAddr = HeapAddr::new(0);

    pub const fn new(id: u32) -> Self {
        Self(id)
    }

    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug)]
pub struct Program {
    pub funcs: Vec<Function>,
    pub entry: FunctionId,
}

pub struct Lowerer<'a> {
    asts: &'a AstHeap,
    patterns: &'a PatternHeap,
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
    pub fn new(asts: &'a AstHeap, patterns: &'a PatternHeap, elab: &'a Elaboration) -> Self {
        Self {
            asts,
            patterns,
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
            Ast::Char(id) => self.lower_const(Value::Char(*id)),
            Ast::Atom(id) => self.lower_const(Value::Atom(*id)),
            Ast::String(id) => self.lower_string(*id),
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
                // Reserve slots for each binding, before lowering them
                let mut group_slots = Vec::with_capacity(bindings.len());
                for binding in bindings {
                    let def = self.elab.define(*binding);
                    if !self.top_fn().scope.contains_key(&def) {
                        let slot = self.new_slot();
                        self.top_fn_mut().scope.insert(def, slot);
                        group_slots.push(slot);
                    }
                }

                for binding in bindings {
                    let def_id = self.elab.define(*binding);
                    let def = self.elab.def(def_id);
                    assert!(!def.clauses().is_empty()); // should have at least one clause
                    if def.clauses()[0] == *binding {
                        self.lower_ast(*binding);
                    }
                }

                for slot in group_slots {
                    self.emit(Instr::FillCaptures { slot });
                }

                self.lower_ast(*expr)
            }

            Ast::Binding { .. } => {
                let def_id = self.elab.define(id);
                let def = self.elab.def(def_id);

                assert!(!def.clauses().is_empty()); // should have at least one clause
                assert!(def.clauses()[0] == id); // Only ever called for the first clause of a definition
                let dst = *self
                    .top_fn()
                    .scope
                    .get(&def_id)
                    .expect("should be reserved by Let");
                let clauses = def.clauses();
                let src = self.lower_clauses(clauses);
                self.emit(Instr::Move { dst, src });
                dst
            }

            Ast::Lambda { arg, body } => {
                let def = self
                    .elab
                    .pattern_define(*arg)
                    .expect("TODO: support lambda patterns");
                self.lower_anon_lambda(def, |this| {
                    this.emit(Instr::Accept); // lambdas always accept (TODO: lambda patterns)
                    this.lower_ast(*body)
                })
            }

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

    fn lower_pattern(&mut self, id: PatternId) -> Slot {
        let pattern = self.patterns.get(id).expect("invalid pattern id");

        match pattern {
            Pattern::Identifier(_) | Pattern::Wildcard => unreachable!("not a valid pattern"),

            Pattern::Int(n) => self.lower_const(Value::Int(*n)),
            Pattern::Char(c) => self.lower_const(Value::Char(*c)),
            Pattern::Atom(id) => self.lower_const(Value::Atom(*id)),

            Pattern::Map(items) => {
                let pairs = items
                    .iter()
                    .map(|(k, v)| {
                        let key = self.lower_pattern(*k);
                        let val = match v {
                            Some(v) => self.lower_pattern(*v),
                            None => self.lower_const(Value::Atom(AtomTable::TRUE)),
                        };
                        (key, val)
                    })
                    .collect();
                self.lower_map(pairs)
            }

            Pattern::Tuple(pattern_ids) => {
                let pairs = pattern_ids
                    .iter()
                    .enumerate()
                    .map(|(i, elem)| {
                        let index = self.lower_const(Value::Int(i as i64));
                        let elem_val = self.lower_pattern(*elem);
                        (index, elem_val)
                    })
                    .collect();
                self.lower_map(pairs)
            }
        }
    }

    fn lower_const(&mut self, value: Value) -> Slot {
        let dst = self.new_slot();
        self.emit(Instr::Const { dst, value });
        dst
    }

    fn lower_string(&mut self, id: StringLiteralId) -> Slot {
        let dst = self.new_slot();
        self.emit(Instr::MakeString { dst, id });
        dst
    }

    fn lower_map(&mut self, pairs: Vec<(Slot, Slot)>) -> Slot {
        let dst = self.new_slot();
        self.emit(Instr::MakeMap { dst, pairs });
        dst
    }

    /// Lowers
    fn lower_clauses(&mut self, clauses: &[AstId]) -> Slot {
        let def = self.elab.def(self.elab.define(clauses[0]));
        let param_defs = def.param_defs().to_vec();
        self.lower_param_chain(clauses, &param_defs, 0)
    }

    /// Emits \p_0 -> \p_1 -> ... \p_n -> dispatch
    fn lower_param_chain(&mut self, clauses: &[AstId], param_defs: &[DefId], i: usize) -> Slot {
        match param_defs.get(i) {
            None => self.lower_dispatch(clauses, param_defs),
            Some(def) => {
                let def = *def;
                self.lower_anon_lambda(def, |this| {
                    this.lower_param_chain(clauses, param_defs, i + 1)
                })
            }
        }
    }

    fn lower_dispatch(&mut self, clauses: &[AstId], param_defs: &[DefId]) -> Slot {
        let level = self.stack.len() - 1;
        let anon_params: Vec<Slot> = param_defs
            .iter()
            .map(|d| self.resolve(*d, level)) // captures happen here
            .collect();

        let result = self.new_slot();
        let mut end_sites = Vec::new();

        for clause in clauses {
            let Some(Ast::Binding {
                params: pats,
                rhs,
                guard,
                ..
            }) = self.asts.get(*clause)
            else {
                unreachable!("clause wasn't a binding")
            };

            // emit tests, jump to next clause on failure
            let mut fail_sites = Vec::new();
            for (pat, anon_param) in pats.iter().zip(&anon_params) {
                fail_sites.extend(self.lower_pattern_match(*pat, *anon_param));
            }

            // do the guard
            if let Some(guard) = guard {
                let guard_cond = self.lower_ast(*guard);
                fail_sites.push(self.emit_patchable(Instr::JumpIfFalse {
                    target: usize::MAX,
                    cond: guard_cond,
                }))
            }

            self.emit(Instr::Accept); // this clause accepts

            let body = self.lower_ast(*rhs);
            self.emit(Instr::Move {
                dst: result,
                src: body,
            });
            end_sites.push(self.emit_patchable(Instr::Jump { target: usize::MAX }));

            for site in fail_sites {
                self.patch_here(site);
            }
        }

        self.emit(Instr::Reject); // non-totality

        for site in end_sites {
            self.patch_here(site);
        }

        result
    }

    fn lower_pattern_match(&mut self, pat: PatternId, src: Slot) -> Vec<PatchSite> {
        match self.patterns.get(pat).expect("invalid pattern id") {
            Pattern::Wildcard => vec![], // irrefutable, but defines nothing

            Pattern::Identifier(_) => {
                let def = self.elab.pattern_define(pat).expect("identifier defines");
                self.top_fn_mut().scope.insert(def, src);
                vec![]
            }

            Pattern::Int(_) | Pattern::Char(_) | Pattern::Atom(_) => {
                let slot = self.lower_pattern(pat);
                vec![self.lower_test_const(src, slot)]
            }

            Pattern::Map(pairs) => {
                let mut sites = Vec::new();
                for (key, value_pat) in pairs {
                    let key_slot = self.lower_pattern(*key);
                    sites.push(self.lower_test_has_key(src, key_slot));

                    if let Some(value_pat) = value_pat {
                        let extracted = self.lower_get_key(src, key_slot);
                        sites.extend(self.lower_pattern_match(*value_pat, extracted));
                    }
                }

                sites
            }

            Pattern::Tuple(elems) => {
                let mut sites = Vec::new();
                sites.push(self.lower_test_tuple_len(src, elems.len()));

                for (i, elem) in elems.iter().enumerate() {
                    let key_slot = self.lower_const(Value::Int(i as i64));
                    let extracted = self.lower_get_key(src, key_slot);
                    sites.extend(self.lower_pattern_match(*elem, extracted));
                }

                sites
            }
        }
    }

    fn lower_test(&mut self, make: impl FnOnce(Slot) -> Instr) -> PatchSite {
        let cond = self.new_slot();
        self.emit(make(cond));
        self.emit_patchable(Instr::JumpIfFalse {
            target: usize::MAX,
            cond,
        })
    }

    fn lower_test_const(&mut self, anon_param: Slot, value: Slot) -> PatchSite {
        self.lower_test(|cond| Instr::TestConst {
            dst: cond,
            src: anon_param,
            value,
        })
    }

    fn lower_test_has_key(&mut self, src: Slot, key: Slot) -> PatchSite {
        self.lower_test(|cond| Instr::TestHasKey {
            dst: cond,
            src,
            key,
        })
    }

    fn lower_test_tuple_len(&mut self, src: Slot, len: usize) -> PatchSite {
        self.lower_test(|cond| Instr::TestTupleLength {
            dst: cond,
            src,
            len,
        })
    }

    fn lower_get_key(&mut self, src: Slot, key: Slot) -> Slot {
        let extracted = self.new_slot();
        self.emit(Instr::GetKey {
            dst: extracted,
            src,
            key,
        });
        extracted
    }

    fn lower_anon_lambda(
        &mut self,
        arg: DefId,
        lower_body: impl FnOnce(&mut Self) -> Slot,
    ) -> Slot {
        // Push a new function to the stack, fill it in
        self.push_fn();
        let param_slot = self.new_slot();
        self.top_fn_mut().scope.insert(arg, param_slot);
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
