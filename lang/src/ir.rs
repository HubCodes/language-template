use crate::ast::Symbol;
use std::collections::HashMap;
use std::iter::FromIterator;

#[derive(Clone, Debug)]
pub enum IR {
    Pop,
    PushBool { value: bool },
    PushInt { value: i32 },
    PushDouble { value: f64 },
    PushString { value: String },
    MakeObject { kv_count: usize },
    Load { target: Box<Symbol> },
    LoadMember { target: Box<Symbol> },
    LoadMemberIndex,
    Call { argc: usize },
    FuncRef { symbol: Box<Symbol> },
    JumpCond { on_true: Box<Symbol>, on_false: Option<Box<Symbol>> },
    NewLet { symbol: Box<Symbol> },
    Return,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shl,
    Shr,
    Lt,
    Gt,
    Lte,
    Gte,
    Eq,
    Neq,
    BitAnd,
    Xor,
    BitOr,
    And,
    Or,
    Typeof,
    Assign,
}

#[derive(Clone, Debug)]
pub struct BasicBlock {
    pub parent: Option<Symbol>,
    pub symbol: Symbol,
    pub codes: Vec<IR>,
    pub args: Option<Vec<Symbol>>,
}

impl BasicBlock {
    pub fn new(
        parent: Option<Symbol>,
        symbol: Symbol,
        args: Option<Vec<Symbol>>
    ) -> BasicBlock {
        BasicBlock { parent, symbol, args, codes: Vec::new() }
    }

    pub fn push(&mut self, ir: IR) {
        self.codes.push(ir);
    }
}

#[derive(Clone)]
pub struct ByteCode {
    code: HashMap<Symbol, BasicBlock>,
}

impl ByteCode {
    pub fn new() -> ByteCode {
        ByteCode { code: HashMap::new() }
    }

    pub fn add(&mut self, bb: BasicBlock) {
        self.code.insert(bb.symbol.clone(), bb);
    }

    pub fn serialize(&self) -> Vec<(&Symbol, &BasicBlock)> {
        Vec::from_iter(self.code.iter())
    }
}
