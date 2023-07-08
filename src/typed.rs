use crate::ast::Literal;
use crate::{CompiledIndex, FileId, TypeId};

pub type VarIndex = u32;

pub struct StaticExpression {
    pub instr: Instruction,
    pub ty: TypeId,
}

pub enum Instruction {
    Get(VarIndex),
    Call(CompiledIndex, Vec<StaticExpression>),
    If(
        Box<StaticExpression>,
        Box<StaticExpression>,
        Box<StaticExpression>,
    ),
    Const(Const),
    LetIn(VarIndex, Box<StaticExpression>, Box<StaticExpression>),
}

pub enum Const {
    Bool(bool),
    Int(i64),
    Float(f64),
}

pub struct Type {
    size: u16,
    origin: FileId,
}

impl Type {
    pub fn size(&self) -> usize {
        self.size as usize
    }
}
