use crate::ast::Literal;
use crate::{CompiledIndex, TypeId};

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
    Const(Literal),
    LetIn(VarIndex, Box<StaticExpression>, Box<StaticExpression>),
}
