mod ast;
mod id;
mod typed;

use std::collections::HashMap;

use ast::{Ast, Bodyitem, Expression};
use hotel::HotelMap;
pub use id::*;
use typed::{StaticExpression, Type};

use crate::ast::Func;

fn main() {
    let cc: CompilerContext = todo!();

    let _ = cc.resolve((fid, iid, Vec::new()), Vec::new());
}

/// TypeIDs of the buildin types.
/// Needed to create literals and such.
struct BuildTypeIds {
    boolean: TypeId,
    integer: TypeId,
    float: TypeId,
    // string: TypeId,
}

struct CompilerContext {
    files: HotelMap<String, FileContext>,

    buildintypes: BuildTypeIds,

    types: HotelMap<SSID, Type>,
    compiled_funcs: HotelMap<SSID, StaticExpression>,
}

pub type CompiledIndex = usize;

pub fn find_symbol(
    files: &HotelMap<String, FileContext>,
    filename: &str,
    symbol: &str,
) -> Option<SID> {
    let filename = filename.to_string();
    let (fileid, context) = files.get_by_key(&filename)?;

    for (i, item) in context.ast.items.iter().enumerate() {
        let Bodyitem::Func(item) = item;

        if item.name == symbol {
            return Some((fileid as u32, i as u32));
        }
    }

    None
}
impl CompilerContext {
    pub fn setup(files: HotelMap<String, FileContext>) -> Self {
        // Find main function and file of main
        let (fileid, mainid) =
            find_symbol(&files, "main.tys", "main").expect("to find main in main.tys");

        // Resolve imports to find buildin types
        let mainfile = &files.get_by_index(fileid as usize).unwrap();
        let symbols = mainfile.symbols;

        // Compile buildin types to concrete symbols! Yay :)
        let mut types = HotelMap::new();

        let file: FileId = symbols["bool"];
        let ty = Type {
            size: 1,
            origin: file,
        };
        let ssid: SSID = (file, /*TODO choose proper definition */ 0, Vec::new());

        let boolid = types.insert(ssid, ty);

        let file: FileId = symbols["int"];
        let ty = Type {
            size: 8,
            origin: file,
        };
        let ssid: SSID = (file, /*TODO choose proper definition */ 0, Vec::new());

        let intid = types.insert(ssid, ty);

        let file: FileId = symbols["float"];
        let ty = Type {
            size: 8,
            origin: file,
        };
        let ssid: SSID = (file, /*TODO choose proper definition */ 0, Vec::new());

        let floatid = types.insert(ssid, ty);

        let buildintypes = BuildTypeIds {
            boolean: boolid as TypeId,
            integer: intid as TypeId,
            float: floatid as TypeId,
        };

        Self {
            files,
            buildintypes,
            types,
            compiled_funcs: HotelMap::new(),
        }
    }

    /// Resolves a function and builds the compiled_funcs
    /// returning the CompiledIndex of the function, and it's return type.
    fn resolve(&mut self, ssid: SSID, argnames: Vec<String>) -> (CompiledIndex, TypeId) {
        if let Some((id, f)) = self.compiled_funcs.get_by_key(&ssid) {
            let ty = f.ty;
            return (id, ty);
        }

        // function isn't yet compiled.

        let (fileid, itemid, typeids) = ssid;

        let FileContext { symbols, ast } = self
            .files
            .get_by_index(fileid as usize)
            .expect("find fileid");

        let item = &ast.items[itemid as usize];
        let Bodyitem::Func(Func { name, args, body }) = item;

        if args.len() != typeids.len() {
            panic!("function {name} called with more or less args than needed");
        }

        let mut stack = Stack::default();

        let sexpr = self.resolve_expr(body, symbols, &mut stack);

        unimplemented!()
    }

    fn resolve_expr(
        &mut self,
        expr: &Expression,
        symbols: &SymbolTable,
        stack: &mut Stack,
    ) -> StaticExpression {
        match expr {
            ast::Expression::Call(symbol, args) => {
                // resolve symbol
                let fileid = symbols[symbol];
                let fc = self.files.get_by_index(fileid as usize).unwrap();
                let Some((itemid, func)) = fc.ast.get_symbol(&symbol) else {
                    panic!("can't find symbol {symbol} in file with index {fileid}");
                };

                // now, evaluate the arguments!

                let args = args
                    .iter()
                    .map(|expr| self.resolve_expr(expr, &symbols, &mut stack))
                    .collect::<Vec<_>>();

                let tys: Vec<TypeId> = args.iter().map(|a| a.ty).collect();

                let ssid = (fileid, itemid, tys);
                let argnames = func.args;

                // INDEX OF FUNCTION INSIDE THE COMPILER HOTEL MAP
                let (compilerindex, ty) = self.resolve(ssid, argnames);

                StaticExpression {
                    instr: typed::Instruction::Call(compilerindex, args),
                    ty,
                }
            }
            ast::Expression::If(condition, branchtrue, branchfalse) => {
                // these are all unrelated. Maybe this can be exploited for performance gains?
                let condition = self.resolve_expr(&condition, symbols, &mut stack);
                let branchtrue = self.resolve_expr(&branchtrue, symbols, &mut stack);
                let branchfalse = self.resolve_expr(&branchfalse, symbols, &mut stack);

                // TODO Assert, that condition returns a boolean
                if condition.ty != self.buildintypes.boolean {
                    panic!("expect bool in if condition");
                }

                // TODO one can Wrap types in interesting ways here!
                if branchtrue.ty != branchfalse.ty {
                    panic!("if and else block have incompatible types");
                }

                let ty = branchtrue.ty;

                StaticExpression {
                    instr: typed::Instruction::If(
                        Box::new(condition),
                        Box::new(branchtrue),
                        Box::new(branchfalse),
                    ),
                    ty,
                }
            }
            ast::Expression::Literal(l) => {
                let instr = typed::Instruction::Const(l.to_const());

                let ty = match l {
                    ast::Literal::Integer(_) => self.buildintypes.integer,
                    ast::Literal::Float(_) => self.buildintypes.float,
                    ast::Literal::Bool(_) => self.buildintypes.boolean,
                };

                StaticExpression { instr, ty }
            }
            ast::Expression::LetIn(varname, assign, expr) => {
                let assign = self.resolve_expr(&assign, symbols, &mut stack);
                stack.push(varname.to_string(), assign.ty);

                let result = self.resolve_expr(&expr, symbols, &mut stack);

                stack.pop();

                result
            }
        }
    }
}

#[derive(Default)]
pub struct Stack {
    items: Vec<(String, TypeId)>,
}

impl Stack {
    pub fn push(&mut self, name: String, ty: TypeId) {
        self.items.push((name, ty));
    }

    pub fn get(&self, name: &str) -> (CompiledIndex, TypeId) {
        for (i, (n, ty)) in self.items.iter().enumerate() {
            if n == name {
                return (i, *ty);
            }
        }

        panic!("var not found in stack")
    }

    pub fn pop(&mut self) {
        self.items.pop().expect("stack to not be empty");
    }
}

struct FileContext {
    /// Import table
    pub symbols: SymbolTable,
    pub ast: Ast,
}

pub type SymbolTable = HashMap<String, FileId>;
