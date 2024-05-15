use crate::{
    parse::{BinOpKind, Node, NodeKind, UnOpKind},
    util::{into_err, Error, Result},
};

use std::{
    fs::File,
    io::{self, Write as _},
    path::Path,
};

/// Generates assembly code for the given `nodes` and writes it to the file at `asm_path`.
pub fn codegen(nodes: Vec<Node>, asm_path: impl AsRef<Path>) -> Result<()> {
    let mut asm_file = match File::create(&asm_path) {
        Ok(file) => file,
        Err(e) => {
            return Err(Error::Any(format!(
                "{}: {}",
                e,
                asm_path.as_ref().display(),
            )))
        }
    };

    // Write the prolouge.
    writeln!(asm_file, ".global main").map_err(into_err)?;
    writeln!(asm_file, "main:").map_err(into_err)?;

    // Write the body.
    for node in nodes {
        gen(node, &mut asm_file).map_err(into_err)?;
    }

    // Write the epilouge.

    Ok(())
}

/// Generates code for the given `node` and writes it to `file`.
fn gen(node: Node, file: &mut File) -> io::Result<()> {
    match node.data {
        NodeKind::Num(num) => {
            writeln!(file, "  mov ${}, %rax", num)?;
            writeln!(file, "  push %rax")?;
        }
        NodeKind::UnOp { op, operand } => {
            gen(*operand, file)?;
            match op {
                UnOpKind::Pos => {}
                UnOpKind::Neg => {
                    writeln!(file, "  pop %rax")?;
                    writeln!(file, "  neg %rax")?;
                    writeln!(file, "  push %rax")?;
                }
                UnOpKind::Return => {
                    writeln!(file, "  pop %rax")?;
                    writeln!(file, "  ret")?;
                }
                UnOpKind::Expr => {
                    writeln!(file, "  add $8, %rsp")?;
                }
            }
        }
        NodeKind::BinOp { op, lhs, rhs } => {
            gen(*lhs, file)?;
            gen(*rhs, file)?;
            writeln!(file, "  pop %rdi")?;
            writeln!(file, "  pop %rax")?;
            match op {
                BinOpKind::Add => writeln!(file, "  add %rdi, %rax")?,
                BinOpKind::Sub => writeln!(file, "  sub %rdi, %rax")?,
                BinOpKind::Mul => writeln!(file, "  imul %rdi, %rax")?,
                BinOpKind::Div => {
                    writeln!(file, "  cqo")?;
                    writeln!(file, "  idiv %rdi")?;
                }
                BinOpKind::Lt => {
                    writeln!(file, "  cmp %rdi, %rax")?;
                    writeln!(file, "  setl %al")?;
                    writeln!(file, "  movzb %al, %rax")?;
                }
                BinOpKind::Le => {
                    writeln!(file, "  cmp %rdi, %rax")?;
                    writeln!(file, "  setle %al")?;
                    writeln!(file, "  movzb %al, %rax")?;
                }
                BinOpKind::Gt => {
                    writeln!(file, "  cmp %rdi, %rax")?;
                    writeln!(file, "  setg %al")?;
                    writeln!(file, "  movzb %al, %rax")?;
                }
                BinOpKind::Ge => {
                    writeln!(file, "  cmp %rdi, %rax")?;
                    writeln!(file, "  setge %al")?;
                    writeln!(file, "  movzb %al, %rax")?;
                }
                BinOpKind::Eq => {
                    writeln!(file, "  cmp %rdi, %rax")?;
                    writeln!(file, "  sete %al")?;
                    writeln!(file, "  movzb %al, %rax")?;
                }
                BinOpKind::Ne => {
                    writeln!(file, "  cmp %rdi, %rax")?;
                    writeln!(file, "  setne %al")?;
                    writeln!(file, "  movzb %al, %rax")?;
                }
            }
            writeln!(file, "  push %rax")?;
        }
    }
    Ok(())
}
