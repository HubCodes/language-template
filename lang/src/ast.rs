use std::collections::HashMap;

#[derive(Debug)]
pub struct Program {
    pub stmt: Stmt,
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    VarDef(Symbol, Option<Expr>),
    If(Expr, Box<Block>, Option<Box<Block>>),
    Block(Block),
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub symbol: Symbol,
    pub body: Vec<Stmt>,
}

impl Block {
    pub fn new(symbol: Symbol, body: Vec<Stmt>) -> Block {
        Block { symbol, body }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Term(Term),
    Index(Box<Expr>, Box<Expr>),
    Member(Box<Expr>, Symbol),
    Assign(Box<Expr>, Box<Expr>),
    Binop(Binop, Box<Expr>, Box<Expr>),
    Typeof(Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Lambda(Symbol, Vec<Symbol>, Box<Stmt>),
}

#[derive(Debug, PartialEq)]
pub enum Binop {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
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
}

#[derive(Debug, PartialEq)]
pub enum Term {
    Obj(Obj),
    Num(Num),
    Str(String),
    Symbol(Symbol),
}

#[derive(Debug, PartialEq)]
pub enum Num {
    Int(i32),
    Double(f64),
}

#[derive(Debug, PartialEq)]
pub struct Obj {
    pub kv: HashMap<String, Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Symbol {
    pub id: i32,
    pub name: String,
}

impl Symbol {
    pub fn new(id: i32, name: &str) -> Symbol {
        Symbol { id, name: name.to_string() }
    }
}

impl Obj {
    pub fn empty() -> Obj {
        Obj { kv: HashMap::new() }
    }
}
