use crate::{
    parse::{BinOpKind, Node, NodeKind, UnOpKind, VarKind},
    typing::{Type, TypeKind},
    util::{Error, Loc, Result, WORD_SIZE},
};

use std::{fs::File, io::Write, path::Path};

/// Registers used to pass function variables.
const ARG_REG1: [&'static str; 6] = ["dil", "sil", "dl", "cl", "r8b", "r9b"];
const ARG_REG8: [&'static str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

/// Represents an assembly code generator that writes to a writer.
pub struct Generator<W: Write> {
    file_name: &'static str,
    input: &'static str,
    /// Writer to write a generated assembly code.
    writer: W,
    /// The number of jump labels.
    num_label: usize,
}

impl Generator<File> {
    pub fn from_path(
        file_name: &'static str,
        input: &'static str,
        path: impl AsRef<Path>,
    ) -> Result<Self> {
        let file = File::create(path)?;

        Ok(Self {
            file_name,
            input,
            writer: file,
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
                match op {
                    UnOpKind::Pos => self.codegen(*operand)?,
                    UnOpKind::Neg => {
                        self.codegen(*operand)?;
                        writeln!(self.writer, "  pop %rax")?;
                        writeln!(self.writer, "  neg %rax")?;
                        writeln!(self.writer, "  push %rax")?;
                    }
                    UnOpKind::Return => {
                        self.codegen(*operand)?;
                        writeln!(self.writer, "  pop %rax")?;

                        // Writes the epilogue.
                        writeln!(self.writer, "  mov %rbp, %rsp")?;
                        writeln!(self.writer, "  pop %rbp")?;

                        writeln!(self.writer, "  ret")?;
                    }
                    UnOpKind::Expr => {
                        self.codegen(*operand)?;
                        writeln!(self.writer, "  add ${}, %rsp", WORD_SIZE)?;
                    }
                    UnOpKind::Addr => {
                        self.gen_addr(*operand)?;
                    }
                    UnOpKind::Deref => {
                        self.codegen(*operand)?;
                        if !matches!(node.ty.kind, TypeKind::Array { .. }) {
                            self.load(&node.ty)?;
                        }
                    }
                }
            }
            NodeKind::BinOp { op, lhs, rhs } => {
                match op {
                    BinOpKind::Assign => {
                        self.gen_lvar(*lhs)?;
                        self.codegen(*rhs)?;
                        self.store(&node.ty)?;
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
                    BinOpKind::PtrAdd(lhs_is_ptr) => {
                        if lhs_is_ptr {
                            writeln!(
                                self.writer,
                                "  imul ${}, %rdi",
                                node.ty.base().unwrap().size
                            )?;
                        } else {
                            writeln!(
                                self.writer,
                                "  imul ${}, %rax",
                                node.ty.base().unwrap().size
                            )?;
                        }
                        writeln!(self.writer, "  add %rdi, %rax")?;
                    }
                    BinOpKind::Sub => writeln!(self.writer, "  sub %rdi, %rax")?,
                    BinOpKind::PtrSub => {
                        writeln!(
                            self.writer,
                            "  imul ${}, %rdi",
                            node.ty.base().unwrap().size
                        )?;
                        writeln!(self.writer, "  sub %rdi, %rax")?;
                    }
                    BinOpKind::PtrDiff => {
                        writeln!(self.writer, "  sub %rdi, %rax")?;
                        writeln!(self.writer, "  cqo")?;
                        writeln!(self.writer, "  mov ${}, %rdi", node.ty.base().unwrap().size)?;
                        writeln!(self.writer, "  idiv %rdi")?;
                    }
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
                self.store(&node.ty)?;
                writeln!(self.writer, "  add ${}, %rsp", WORD_SIZE)?;
            }
            NodeKind::Var(_) => {
                let ty = node.ty.clone();
                self.gen_addr(node)?;
                if !matches!(ty.kind, TypeKind::Array { .. }) {
                    self.load(&ty)?;
                }
            }
            NodeKind::Block { stmts } => {
                for stmt in stmts {
                    self.codegen(stmt)?;
                }
            }
            NodeKind::StmtExpr { stmts } => {
                self.codegen(Node::with_block(stmts, node.loc))?;
                writeln!(self.writer, "  sub ${}, %rsp", WORD_SIZE)?;
            }
            NodeKind::While { cond, stmt } => {
                let index = self.num_label;
                self.num_label += 1;

                writeln!(self.writer, ".L.start.{}:", index)?;
                self.codegen(*cond)?;
                writeln!(self.writer, "  pop %rax")?;
                writeln!(self.writer, "  test %rax, %rax")?;
                writeln!(self.writer, "  je .L.end.{}", index)?;
                self.codegen(*stmt)?;
                writeln!(self.writer, "  jmp .L.start.{}", index)?;
                writeln!(self.writer, ".L.end.{}:", index)?;
            }
            NodeKind::For {
                init,
                cond,
                inc,
                stmt,
            } => {
                let index = self.num_label;
                self.num_label += 1;

                if let Some(init) = init {
                    self.codegen(*init)?;
                }
                writeln!(self.writer, ".L.start.{}:", index)?;
                if let Some(cond) = cond {
                    self.codegen(*cond)?;
                }
                writeln!(self.writer, "  pop %rax")?;
                writeln!(self.writer, "  test %rax, %rax")?;
                writeln!(self.writer, "  je .L.end.{}", index)?;
                self.codegen(*stmt)?;
                if let Some(inc) = inc {
                    self.codegen(*inc)?;
                }
                writeln!(self.writer, "  jmp .L.start.{}", index)?;
                writeln!(self.writer, ".L.end.{}:", index)?;
            }
            NodeKind::If {
                cond,
                stmt,
                elif_conds,
                elif_stmts,
                else_stmt,
            } => {
                let index = self.num_label;
                self.num_label += 1;
                self.codegen(*cond)?;
                writeln!(self.writer, "  pop %rax")?;
                writeln!(self.writer, "  test %rax, %rax")?;
                writeln!(self.writer, "  jne .L.if.{}", index)?;

                for (i, cond) in elif_conds.into_iter().enumerate() {
                    self.codegen(cond)?;
                    writeln!(self.writer, "  pop %rax")?;
                    writeln!(self.writer, "  test %rax, %rax")?;
                    writeln!(self.writer, "  jne .L.elif.{}.{}", i, index)?;
                }

                if let Some(else_stmt) = else_stmt {
                    self.codegen(*else_stmt)?;
                }
                writeln!(self.writer, "  jmp .L.end.{}", index)?;

                writeln!(self.writer, ".L.if.{}:", index)?;
                self.codegen(*stmt)?;
                writeln!(self.writer, "  jmp .L.end.{}", index)?;

                for (i, stmt) in elif_stmts.into_iter().enumerate() {
                    writeln!(self.writer, ".L.elif.{}.{}:", i, index)?;
                    self.codegen(stmt)?;
                    writeln!(self.writer, "  jmp .L.end.{}", index)?;
                }

                writeln!(self.writer, ".L.end.{}:", index)?;
            }
            NodeKind::FnCall { name, args } => {
                // Args passed with the stack is not yet supported.
                if args.len() > ARG_REG8.len() {
                    return self.comp_err(
                        format!(
                            "function call with {} or more args is not supported",
                            ARG_REG8.len() + 1
                        ),
                        args[ARG_REG8.len()].loc,
                    );
                }

                let num_args = args.len();
                for arg in args {
                    self.codegen(arg)?;
                }
                for i in (0..num_args).rev() {
                    writeln!(self.writer, "  pop %{}", ARG_REG8[i])?;
                }

                // Ensure that RSP is 16-byte boundary before calling a function.
                // RAX is set to 0 for variadic function.
                // RAX represents the number of vector registers used.
                let index = self.num_label;
                self.num_label += 1;

                writeln!(self.writer, "  mov %rsp, %rax")?;
                writeln!(self.writer, "  and $15, %rax")?;
                writeln!(self.writer, "  jnz .L.call.{}", index)?;
                writeln!(self.writer, "  mov $0, %rax")?;
                writeln!(self.writer, "  call {}", name)?;
                writeln!(self.writer, "  jmp .L.end.{}", index)?;

                writeln!(self.writer, ".L.call.{}:", index)?;
                writeln!(self.writer, "  sub ${}, %rsp", WORD_SIZE)?;
                writeln!(self.writer, "  mov $0, %rax")?;
                writeln!(self.writer, "  call {}", name)?;
                writeln!(self.writer, "  add ${}, %rsp", WORD_SIZE)?;

                writeln!(self.writer, ".L.end.{}:", index)?;
                writeln!(self.writer, "  push %rax")?;
            }
            NodeKind::Fn {
                name,
                params,
                stmts,
                locals,
                ..
            } => {
                // Write the prolouge.
                writeln!(self.writer, ".global {}", name)?;
                writeln!(self.writer, "{}:", name)?;
                writeln!(self.writer, "  push %rbp")?;
                writeln!(self.writer, "  mov %rsp, %rbp")?;

                // Determins the offset of each local variables.
                let mut offset = 0;

                for local in locals.into_iter().rev() {
                    let mut local = local.borrow_mut();
                    offset += local.ty.size;
                    local.offset = offset;
                }

                // Allocates memory for global variables.
                offset = align_to(offset, WORD_SIZE);
                if offset != 0 {
                    writeln!(self.writer, "  sub ${}, %rsp", offset)?;
                }

                // Copy the arguments into the stack.
                for (i, param) in params.into_iter().enumerate() {
                    let param = param.borrow();
                    let (size, offset) = (param.ty.size, param.offset);
                    if size == 1 {
                        writeln!(self.writer, "  mov %{}, -{}(%rbp)", ARG_REG1[i], offset)?;
                    } else {
                        writeln!(self.writer, "  mov %{}, -{}(%rbp)", ARG_REG8[i], offset)?;
                    }
                }

                for stmt in stmts {
                    self.codegen(stmt)?;
                }

                // Writes the epilogue.
                writeln!(self.writer, ".L.return.{}:", name)?;
                writeln!(self.writer, "  mov %rbp, %rsp")?;
                writeln!(self.writer, "  pop %rbp")?;
                writeln!(self.writer, "  ret")?;
            }
            NodeKind::Program { funcs, globals } => {
                // Emits data
                writeln!(self.writer, ".data")?;
                for var in globals {
                    let var = var.borrow();
                    writeln!(self.writer, ".global {}", var.name)?;
                    writeln!(self.writer, "{}:", var.name)?;
                    match var.kind {
                        VarKind::Others => {
                            writeln!(self.writer, "  .zero {}", var.ty.size)?;
                        }
                        VarKind::Str(s) => {
                            for &b in s.as_bytes() {
                                writeln!(self.writer, "  .byte {}", b)?;
                            }
                        }
                    }
                }

                // Emits text
                writeln!(self.writer, ".text")?;
                for func in funcs {
                    self.codegen(func)?;
                }
            }
        }
        Ok(())
    }

    fn gen_lvar(&mut self, node: Node) -> Result<()> {
        match node.data {
            NodeKind::Var(ref var) => {
                // This unwrapping always succeed because existance of undeclared variable emit
                // an error in parse phase.
                if var.borrow().ty.is_array() {
                    self.comp_err("not an lvalue", node.loc)
                } else {
                    self.gen_addr(node)
                }
            }
            _ => self.gen_addr(node),
        }
    }

    /// Generates the address of the given `node`.
    fn gen_addr(&mut self, node: Node) -> Result<()> {
        match node.data {
            NodeKind::Var(var) => {
                // This unwrapping always succeed because existance of undeclared variable emit
                // an error in parse phase.
                let var = var.borrow();
                if var.is_local {
                    writeln!(self.writer, "  lea -{}(%rbp), %rdi", var.offset)?;
                    writeln!(self.writer, "  push %rdi")?;
                } else {
                    writeln!(self.writer, "  lea {}(%rip), %rdi", var.name)?;
                    writeln!(self.writer, "  push %rdi")?;
                }
                Ok(())
            }
            NodeKind::UnOp {
                op: UnOpKind::Deref,
                operand,
            } => {
                self.codegen(*operand)?;
                Ok(())
            }
            _ => self.comp_err("not an lvalue", node.loc),
        }
    }

    fn load(&mut self, ty: &Type) -> Result<()> {
        writeln!(self.writer, "  pop %rax")?;
        if ty.size == 1 {
            writeln!(self.writer, "  movsbq (%rax), %rax")?;
        } else {
            writeln!(self.writer, "  mov (%rax), %rax")?;
        }
        writeln!(self.writer, "  push %rax")?;
        Ok(())
    }

    fn store(&mut self, ty: &Type) -> Result<()> {
        writeln!(self.writer, "  pop %rax")?;
        writeln!(self.writer, "  pop %rdi")?;
        if ty.size == 1 {
            writeln!(self.writer, "  mov %al, (%rdi)")?;
        } else {
            writeln!(self.writer, "  mov %rax, (%rdi)")?;
        }
        writeln!(self.writer, "  push %rax")?;
        Ok(())
    }

    fn comp_err<T>(&self, msg: impl Into<String>, loc: Loc) -> Result<T> {
        Err(Error::CompileError {
            message: msg.into(),
            file_name: self.file_name,
            input: self.input,
            loc,
        })
    }
}

fn align_to(n: usize, align: usize) -> usize {
    (n + align - 1) & !(align - 1)
}
