use crate::{
    parse::{BinOpKind, Node, NodeKind, UnOpKind},
    util::Result,
};

use std::{fs::File, io::Write, path::Path};

/// Represents an assembly code generator that writes to a writer.
pub struct Generator<W: Write> {
    /// Writer to write a generated assembly code.
    writer: W,
}

impl Generator<File> {
    pub fn from_path(path: impl AsRef<Path>) -> Result<Self> {
        let file = File::create(path)?;

        Ok(Self { writer: file })
    }
}

impl<W: Write> Generator<W> {
    /// Generates and writes the assembly code for the given `top_node`.
    pub fn codegen(&mut self, top_node: Node) -> Result<()> {
        // Write the prolouge.
        writeln!(self.writer, ".global main")?;
        writeln!(self.writer, "main:")?;

        // Write the body.
        self.gen(top_node)?;

        Ok(())
    }

    /// Generates and writes the assembly code for the given `node`.
    fn gen(&mut self, node: Node) -> Result<()> {
        match node.data {
            NodeKind::Num(num) => {
                writeln!(self.writer, "  mov ${}, %rax", num)?;
                writeln!(self.writer, "  push %rax")?;
            }
            NodeKind::UnOp { op, operand } => {
                self.gen(*operand)?;
                match op {
                    UnOpKind::Pos => {}
                    UnOpKind::Neg => {
                        writeln!(self.writer, "  pop %rax")?;
                        writeln!(self.writer, "  neg %rax")?;
                        writeln!(self.writer, "  push %rax")?;
                    }
                    UnOpKind::Return => {
                        writeln!(self.writer, "  pop %rax")?;
                        writeln!(self.writer, "  ret")?;
                    }
                    UnOpKind::Expr => {
                        writeln!(self.writer, "  add $8, %rsp")?;
                    }
                }
            }
            NodeKind::BinOp { op, lhs, rhs } => {
                self.gen(*lhs)?;
                self.gen(*rhs)?;
                writeln!(self.writer, "  pop %rdi")?;
                writeln!(self.writer, "  pop %rax")?;
                match op {
                    BinOpKind::Add => writeln!(self.writer, "  add %rdi, %rax")?,
                    BinOpKind::Sub => writeln!(self.writer, "  sub %rdi, %rax")?,
                    BinOpKind::Mul => writeln!(self.writer, "  imul %rdi, %rax")?,
                    BinOpKind::Div => {
                        writeln!(self.writer, "  cqo")?;
                        writeln!(self.writer, "  idiv %rdi")?;
                    }
                    BinOpKind::Lt => {
                        writeln!(self.writer, "  cmp %rdi, %rax")?;
                        writeln!(self.writer, "  setl %al")?;
                        writeln!(self.writer, "  movzb %al, %rax")?;
                    }
                    BinOpKind::Le => {
                        writeln!(self.writer, "  cmp %rdi, %rax")?;
                        writeln!(self.writer, "  setle %al")?;
                        writeln!(self.writer, "  movzb %al, %rax")?;
                    }
                    BinOpKind::Gt => {
                        writeln!(self.writer, "  cmp %rdi, %rax")?;
                        writeln!(self.writer, "  setg %al")?;
                        writeln!(self.writer, "  movzb %al, %rax")?;
                    }
                    BinOpKind::Ge => {
                        writeln!(self.writer, "  cmp %rdi, %rax")?;
                        writeln!(self.writer, "  setge %al")?;
                        writeln!(self.writer, "  movzb %al, %rax")?;
                    }
                    BinOpKind::Eq => {
                        writeln!(self.writer, "  cmp %rdi, %rax")?;
                        writeln!(self.writer, "  sete %al")?;
                        writeln!(self.writer, "  movzb %al, %rax")?;
                    }
                    BinOpKind::Ne => {
                        writeln!(self.writer, "  cmp %rdi, %rax")?;
                        writeln!(self.writer, "  setne %al")?;
                        writeln!(self.writer, "  movzb %al, %rax")?;
                    }
                }
                writeln!(self.writer, "  push %rax")?;
            }
            NodeKind::Program { stmts } => {
                for stmt in stmts {
                    self.gen(stmt)?;
                }
            }
        }
        Ok(())
    }
}
