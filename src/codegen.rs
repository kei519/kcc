use crate::{
    parse::{BinOpKind, Node, NodeKind, UnOpKind, Var},
    util::{Error, Result},
};

use std::{collections::HashSet, fs::File, io::Write, mem::size_of, path::Path};

/// Represents the size of a word in bytes.
const WORD_SIZE: usize = size_of::<usize>();

/// Registers used to pass function variables.
const ARG_REG: [&'static str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

/// Represents an assembly code generator that writes to a writer.
pub struct Generator<W: Write> {
    input: &'static str,
    /// Writer to write a generated assembly code.
    writer: W,
    /// Global variables.
    locals: HashSet<Var>,
    /// The number of jump labels.
    num_label: usize,
}

impl Generator<File> {
    pub fn from_path(input: &'static str, path: impl AsRef<Path>) -> Result<Self> {
        let file = File::create(path)?;

        Ok(Self {
            input,
            writer: file,
            locals: HashSet::new(),
            num_label: 0,
        })
    }
}

impl<W: Write> Generator<W> {
    /// Generates and writes the assembly code for the given `node`.
    pub fn codegen(&mut self, node: Node) -> Result<()> {
        match node.data {
            NodeKind::Num(num) => {
                writeln!(self.writer, "  mov ${}, %rax", num)?;
                writeln!(self.writer, "  push %rax")?;
            }
            NodeKind::UnOp { op, operand } => {
                self.codegen(*operand)?;
                match op {
                    UnOpKind::Pos => {}
                    UnOpKind::Neg => {
                        writeln!(self.writer, "  pop %rax")?;
                        writeln!(self.writer, "  neg %rax")?;
                        writeln!(self.writer, "  push %rax")?;
                    }
                    UnOpKind::Return => {
                        writeln!(self.writer, "  pop %rax")?;

                        // Writes the epilogue.
                        writeln!(self.writer, "  mov %rbp, %rsp")?;
                        writeln!(self.writer, "  pop %rbp")?;

                        writeln!(self.writer, "  ret")?;
                    }
                    UnOpKind::Expr => {
                        writeln!(self.writer, "  add ${}, %rsp", WORD_SIZE)?;
                    }
                }
            }
            NodeKind::BinOp { op, lhs, rhs } => {
                match op {
                    BinOpKind::Assign => {
                        self.gen_addr(*lhs)?;
                        self.codegen(*rhs)?;
                        writeln!(self.writer, "  pop %rax")?;
                        writeln!(self.writer, "  pop %rdi")?;
                        writeln!(self.writer, "  mov %rax, (%rdi)")?;
                        writeln!(self.writer, "  push %rax")?;
                        return Ok(());
                    }
                    _ => {}
                }

                self.codegen(*lhs)?;
                self.codegen(*rhs)?;
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
                    BinOpKind::Assign => {
                        unreachable!("Assign should be handled before reaching here")
                    }
                }
                writeln!(self.writer, "  push %rax")?;
            }
            NodeKind::VarDecl { var, init, .. } => {
                let init = if let Some(init) = init {
                    init
                } else {
                    return Ok(());
                };

                self.gen_addr(*var)?;
                self.codegen(*init)?;
                writeln!(self.writer, "  pop %rax")?;
                writeln!(self.writer, "  pop %rdi")?;
                writeln!(self.writer, "  mov %rax, (%rdi)")?;
            }
            NodeKind::Var(_) => {
                self.gen_addr(node)?;
                writeln!(self.writer, "  pop %rdi")?;
                writeln!(self.writer, "  push (%rdi)")?;
            }
            NodeKind::Block { stmts } => {
                for stmt in stmts {
                    self.codegen(stmt)?;
                }
            }
            NodeKind::While { cond, stmt } => {
                writeln!(self.writer, ".L.start.{}:", self.num_label)?;
                self.codegen(*cond)?;
                writeln!(self.writer, "  pop %rax")?;
                writeln!(self.writer, "  test %rax, %rax")?;
                writeln!(self.writer, "  je .L.end.{}", self.num_label)?;
                self.codegen(*stmt)?;
                writeln!(self.writer, "  jmp .L.start.{}", self.num_label)?;
                writeln!(self.writer, ".L.end.{}:", self.num_label)?;
                self.num_label += 1;
            }
            NodeKind::For {
                init,
                cond,
                inc,
                stmt,
            } => {
                if let Some(init) = init {
                    self.codegen(*init)?;
                }
                writeln!(self.writer, ".L.start.{}:", self.num_label)?;
                if let Some(cond) = cond {
                    self.codegen(*cond)?;
                }
                writeln!(self.writer, "  pop %rax")?;
                writeln!(self.writer, "  test %rax, %rax")?;
                writeln!(self.writer, "  je .L.end.{}", self.num_label)?;
                self.codegen(*stmt)?;
                if let Some(inc) = inc {
                    self.codegen(*inc)?;
                }
                writeln!(self.writer, "  jmp .L.start.{}", self.num_label)?;
                writeln!(self.writer, ".L.end.{}:", self.num_label)?;
                self.num_label += 1;
            }
            NodeKind::If {
                cond,
                stmt,
                elif_conds,
                elif_stmts,
                else_stmt,
            } => {
                self.codegen(*cond)?;
                writeln!(self.writer, "  pop %rax")?;
                writeln!(self.writer, "  test %rax, %rax")?;
                writeln!(self.writer, "  jne .L.if.{}", self.num_label)?;

                for (i, cond) in elif_conds.into_iter().enumerate() {
                    self.codegen(cond)?;
                    writeln!(self.writer, "  pop %rax")?;
                    writeln!(self.writer, "  test %rax, %rax")?;
                    writeln!(self.writer, "  jne .L.elif.{}.{}", i, self.num_label)?;
                }

                if let Some(else_stmt) = else_stmt {
                    self.codegen(*else_stmt)?;
                }
                writeln!(self.writer, "  jmp .L.end.{}", self.num_label)?;

                writeln!(self.writer, ".L.if.{}:", self.num_label)?;
                self.codegen(*stmt)?;
                writeln!(self.writer, "  jmp .L.end.{}", self.num_label)?;

                for (i, stmt) in elif_stmts.into_iter().enumerate() {
                    writeln!(self.writer, ".L.elif.{}.{}:", i, self.num_label)?;
                    self.codegen(stmt)?;
                    writeln!(self.writer, "  jmp .L.end.{}", self.num_label)?;
                }

                writeln!(self.writer, ".L.end.{}:", self.num_label)?;

                self.num_label += 1;
            }
            NodeKind::FnCall { name, args } => {
                // Args passed with the stack is not yet supported.
                if args.len() > ARG_REG.len() {
                    return Err(Error::CompileError {
                        message: format!(
                            "function call with {} or more args is not supported",
                            ARG_REG.len() + 1
                        ),
                        input: self.input,
                        loc: args[ARG_REG.len()].loc,
                    });
                }

                let num_args = args.len();
                for arg in args {
                    self.codegen(arg)?;
                }
                for i in (0..num_args).rev() {
                    writeln!(self.writer, "  pop %{}", ARG_REG[i])?;
                }

                // Ensure that RSP is 16-byte boundary.
                writeln!(self.writer, "  mov %rsp, %rax")?;
                writeln!(self.writer, "  and $15, %rax")?;
                writeln!(self.writer, "  jz .L.call.{}", self.num_label)?;
                writeln!(self.writer, "  call {}", name)?;
                writeln!(self.writer, "  jmp .L.end.{}", self.num_label)?;
                writeln!(self.writer, ".L.call.{}:", self.num_label)?;
                writeln!(self.writer, "  sub $8, %rsp")?;
                writeln!(self.writer, "  call {}", name)?;
                writeln!(self.writer, "  add $8, %rsp")?;
                writeln!(self.writer, ".L.end.{}:", self.num_label)?;
                writeln!(self.writer, "  push %rax")?;

                self.num_label += 1;
            }
            NodeKind::Fn {
                name,
                params,
                stmts,
                locals,
                ..
            } => {
                // Reset the locals.
                self.locals.clear();

                // Write the prolouge.
                writeln!(self.writer, ".global {}", name)?;
                writeln!(self.writer, "{}:", name)?;
                writeln!(self.writer, "  push %rbp")?;
                writeln!(self.writer, "  mov %rsp, %rbp")?;

                // Determins the offset of each local variables.
                let mut offset = 0;

                // Copy the arguments to the stack.
                for (i, mut param) in params.into_iter().enumerate() {
                    offset += WORD_SIZE;
                    param.offset = offset;
                    self.locals.insert(param);
                    writeln!(self.writer, "  mov %{}, -{}(%rbp)", ARG_REG[i], offset)?;
                }

                for mut local in locals {
                    offset += WORD_SIZE;
                    local.offset = offset;
                    self.locals.insert(local);
                }

                // Allocates memory for global variables.
                if offset != 0 {
                    writeln!(self.writer, "  sub ${}, %rsp", offset)?;
                }

                for stmt in stmts {
                    self.codegen(stmt)?;
                }
            }
            NodeKind::Program { funcs } => {
                for func in funcs {
                    self.codegen(func)?;
                }
            }
        }
        Ok(())
    }

    /// Generates the address of the given `node`.
    fn gen_addr(&mut self, node: Node) -> Result<()> {
        match node.data {
            NodeKind::Var(name) => {
                if let Some(var) = (&self.locals).into_iter().find(|v| v.name == name) {
                    writeln!(self.writer, "  lea -{}(%rbp), %rdi", var.offset)?;
                    writeln!(self.writer, "  push %rdi")?;
                    Ok(())
                } else {
                    unreachable!("We don't need declaration of variables");
                }
            }
            _ => Err(Error::CompileError {
                message: "not an lvalue".into(),
                input: self.input,
                loc: node.loc,
            }),
        }
    }
}
