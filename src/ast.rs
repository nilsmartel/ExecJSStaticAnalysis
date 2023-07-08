use crate::{typed::Const, ItemId};

pub struct Ast {
    pub imports: Vec<Import>,
    pub items: Vec<Bodyitem>,
}

impl Ast {
    pub fn get_symbol(&self, symbol: &str) -> Option<(ItemId, &Func)> {
        for (i, item) in self.items.iter().enumerate() {
            let Bodyitem::Func(item) = item;
            if item.name == symbol {
                return Some((i as u32, &item));
            }
        }

        None
    }
}

pub struct Import {
    filepath: String,
    symbols: Vec<String>,
}

pub enum Bodyitem {
    Func(Func),
}

type Ident = String;

pub struct Func {
    pub name: String,
    pub args: Vec<Ident>,
    pub body: Expression,
}

pub enum Expression {
    Call(Ident, Vec<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    Literal(Literal),
    LetIn(Ident, Box<Expression>, Box<Expression>),
}

pub enum Literal {
    Integer(i64),
    Float(f64),
    Bool(bool),
}

impl Literal {
    pub fn to_const(&self) -> Const {
        match self {
            Literal::Integer(i) => Const::Int(*i),
            Literal::Float(f) => Const::Float(*f),
            Literal::Bool(b) => Const::Bool(*b),
        }
    }
}
