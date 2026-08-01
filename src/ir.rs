use std::{cell::RefCell, collections::HashMap};

use crate::{
    ast::{Ast, AstHeap, AstId},
    builtin::Builtin,
    elaborate::Elaboration,
    interner::AtomId,
    ir::Instr::RetEval,
    scope::DefId,
};

pub struct Function {
    pub instructions: Vec<Instr>,
    pub slots_used: u32,
    /// The slots in the parent scope where these can be found
    pub captures: Vec<(Slot, Slot)>,
}

impl Function {
    pub fn debug(&self) {
        println!("Slots used: {}", self.slots_used);
        println!("Captures: {:?}", self.captures);
        println!("Instructions:");
        for instr in self.instructions.iter() {
            println!("{instr:?}");
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FunctionId(u32);

impl FunctionId {
    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone)]
pub enum Instr {
    Const { dst: Slot, value: Value },

    MakeMap { dst: Slot, pairs: Vec<(Slot, Slot)> },

    MakeClosure { dst: Slot, function: FunctionId },

    Apply { dst: Slot, lhs: Slot, rhs: Slot },

    Ret { src: Slot },
    RetEval { src: Slot },
}

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
    pub fn as_int<T>(&self) -> Result<T, String>
    where
        T: From<i64>,
    {
        match self {
            Value::Int(x) => Ok(T::from(*x)),
            Value::Float(x) => Ok(T::from(*x as i64)),
            Value::Char(x) => Ok(T::from(*x as i64)),
            _ => Err(format!("cannot convert {:?} to int", self)),
        }
    }

    /// Interpret this value as an integer. Does some basic conversions
    pub fn as_float<T>(&self) -> Result<T, String>
    where
        T: From<f64>,
    {
        match self {
            Value::Int(x) => Ok(T::from(*x as f64)),
            Value::Float(x) => Ok(T::from(*x)),
            _ => Err(format!("cannot convert {:?} to float", self)),
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

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct LowerScopeId(u32);

impl LowerScopeId {
    /// Convert a LowerScopeId to a u32
    pub(crate) fn as_u32(&self) -> u32 {
        self.0
    }
}

struct LowerScopeArena {
    lower_scopes: Vec<LowerScope>,
}

impl LowerScopeArena {
    fn new() -> Self {
        Self {
            lower_scopes: Vec::new(),
        }
    }

    fn new_scope(&mut self, parent: Option<LowerScopeId>) -> LowerScopeId {
        let id = LowerScopeId(self.lower_scopes.len() as u32);
        self.lower_scopes.push(LowerScope::new(parent));
        id
    }

    fn get_scope(&self, scope_id: LowerScopeId) -> &LowerScope {
        &self.lower_scopes[scope_id.as_u32() as usize]
    }

    fn get_scope_mut(&mut self, scope_id: LowerScopeId) -> &mut LowerScope {
        &mut self.lower_scopes[scope_id.as_u32() as usize]
    }

    fn lookup(&self, def: DefId, scope: LowerScopeId) -> Option<(Slot, u32)> {
        let mut depth = 0;
        let mut curr_scope: Option<LowerScopeId> = Some(scope);

        while let Some(some_curr_scope) = curr_scope {
            let scope = self.get_scope(some_curr_scope);

            if let Some(def) = scope.get_slot(def) {
                return Some((*def, depth));
            }

            curr_scope = scope.parent();
            depth += 1;
        }

        None
    }

    fn insert(&mut self, scope_id: LowerScopeId, key: DefId, slot: Slot) {
        let scope_ref = self.get_scope_mut(scope_id);
        scope_ref.def_map.insert(key, slot);
    }
}

#[derive(Default)]
struct LowerScope {
    def_map: HashMap<DefId, Slot>,
    parent: Option<LowerScopeId>,
}

impl LowerScope {
    fn new(parent: Option<LowerScopeId>) -> Self {
        Self {
            def_map: HashMap::new(),
            parent,
        }
    }

    fn parent(&self) -> Option<LowerScopeId> {
        self.parent
    }

    fn get_slot(&self, def: DefId) -> Option<&Slot> {
        self.def_map.get(&def)
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
    scopes: LowerScopeArena,
    stack: Vec<FnState>,
}

struct FnState {
    scope: LowerScopeId,
    slots_used: u32,
    captures: Vec<(Slot, Slot)>,
    instructions: Vec<Instr>,
}

impl FnState {
    fn into_function(self) -> Function {
        Function {
            instructions: self.instructions,
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
            scopes: LowerScopeArena::new(),
            stack: Vec::new(),
        }
    }

    pub fn lower(&mut self, root: AstId) -> Program {
        self.push_fn(None);
        let body_slot = self.lower_ast(root);
        self.emit(RetEval { src: body_slot });

        let root_function = self.pop_fn().into_function();

        let entry = FunctionId(self.funcs.len() as u32);
        self.funcs.push(root_function);

        println!("\nThem all lowered:");
        for func in &self.funcs {
            func.debug();
        }

        Program {
            funcs: self.funcs.drain(..).collect(),
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

            Ast::Map(pairs) => {
                let dst = self.new_slot();
                let pairs = pairs
                    .iter()
                    .map(|(k, v)| (self.lower_ast(*k), self.lower_ast(*v)))
                    .collect();
                self.emit(Instr::MakeMap { dst, pairs });
                dst
            }

            Ast::Tuple(elems) => {
                let dst = self.new_slot();
                let pairs = elems
                    .iter()
                    .enumerate()
                    .map(|(i, elem)| {
                        let index: Slot = self.new_slot();
                        self.emit(Instr::Const {
                            dst: index,
                            value: Value::Int(i as i64),
                        });
                        let elem_val = self.lower_ast(*elem);
                        (index, elem_val)
                    })
                    .collect();
                self.emit(Instr::MakeMap { dst, pairs });
                dst
            }

            Ast::Identifier(_) => {
                let def = self.elab.refer(id).expect("should exist");
                self.resolve(*def, self.stack.len() - 1)
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
                    _ = self.lower_ast(*binding)
                }
                self.lower_ast(*expr)
            }

            Ast::Binding { rhs, .. } => {
                let def = self.elab.define(id).expect("asts gotta define something!");
                let slot = self.lower_ast(*rhs);
                self.scopes.insert(self.top_fn().scope, *def, slot);
                slot
            }

            Ast::Lambda(param, body) => {
                let function = {
                    self.push_fn(Some(self.top_fn().scope));

                    let param_slot = self.new_slot();

                    let def = self.elab.pattern_define(*param);
                    self.scopes.insert(self.top_fn().scope, def, param_slot);

                    let body_slot = self.lower_ast(*body);
                    self.emit(Instr::Ret { src: body_slot });

                    self.pop_fn().into_function()
                };

                let func_id = {
                    let id = FunctionId(self.funcs.len() as u32);
                    self.funcs.push(function);
                    id
                };

                let dst = self.new_slot();
                self.emit(Instr::MakeClosure {
                    dst,
                    function: func_id,
                    // Needs to copy closure heap slots <= locals from here
                });
                dst
            }

            _ => todo!("not implemented: {:?}", ast),
        }
    }

    fn lower_const(&mut self, value: Value) -> Slot {
        let dst = self.new_slot();
        self.emit(Instr::Const { dst, value });
        dst
    }

    fn push_fn(&mut self, parent: Option<LowerScopeId>) {
        self.stack.push(FnState {
            scope: self.scopes.new_scope(parent),
            slots_used: 0,
            captures: Vec::new(),
            instructions: Vec::new(),
        })
    }

    fn pop_fn(&mut self) -> FnState {
        self.stack.pop().unwrap()
    }

    fn top_fn(&self) -> &FnState {
        self.stack.last().unwrap()
    }

    fn top_fn_mut(&mut self) -> &mut FnState {
        self.stack.last_mut().unwrap()
    }

    fn resolve(&mut self, def: DefId, level: usize) -> Slot {
        if let Some(slot) = self.scopes.get_scope(self.stack[level].scope).get_slot(def) {
            return *slot;
        }

        let parent_slot = self.resolve(def, level - 1);
        let dst = self.new_slot_at(level);
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
}
