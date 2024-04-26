use crate::*;

/// Represents code generator.
pub struct Generator {
    input: &'static str,
    label_num: usize,
}

impl Generator {
    /// Constructor.
    pub fn new(input: &'static str) -> Self {
        Self {
            input,
            label_num: 0,
        }
    }

    /// Generates the assembly code form the AST top node.
    pub fn codegen(&mut self) -> Result<()> {
        let tokens = Tokenizer::new(self.input).tokenize()?;
        let mut parser = Parser::new(tokens);

        // prepare for assembler
        println!(".intel_syntax noprefix");
        println!(".global main");
        println!("main:");
        println!("  push rbp");
        println!("  mov rbp, rsp");

        let (nodes, var_num) = parser.parse()?;
        // take the space for the variables
        println!("  sub rsp, {}", var_num * MEMORY_SIZE);
        for node in nodes {
            self.gen(node)?;
        }

        Ok(())
    }

    /// Generates the assembly code from a top node.
    fn gen(&mut self, node: Node) -> Result<()> {
        let Node { value: node, .. } = node;
        match node {
            // When the node is a number, it is a terminal.
            NodeKind::Num(val) => {
                println!("  push {}", val);
                return Ok(());
            }
            NodeKind::Var { offset, .. } => {
                println!("  lea rdi, [rbp - {}]", offset);
                println!("  mov rax, [rdi]");
                println!("  push rax");
            }
            NodeKind::Return { val } => {
                self.gen(*val)?;

                println!("  pop rax");
                println!("  mov rsp, rbp");
                println!("  pop rbp");
                println!("  ret");
            }
            NodeKind::UnOp { op, arg } => {
                self.gen(*arg)?;
                match op.value {
                    UnOpKind::Pos => {}
                    UnOpKind::Neg => {
                        println!("  pop rax");
                        println!("  neg rax");
                        println!("  push rax");
                    }
                    UnOpKind::ExprStmt => {
                        println!("  add rsp, {}", MEMORY_SIZE);
                    }
                }
            }
            NodeKind::BinOp { op, left, right } => {
                match op.value {
                    BinOpKind::Assign => {
                        lval_gen(*left)?;
                        self.gen(*right)?;
                        println!("  pop rax");
                        println!("  pop rdi");
                        println!("  mov [rdi], rax");
                        println!("  push rax");
                        return Ok(());
                    }
                    _ => {}
                }
                // When the node is a binary operator, it has left and right side.
                self.gen(*left)?;
                self.gen(*right)?;

                println!("  pop rdi");
                println!("  pop rax");

                match op.value {
                    BinOpKind::Add => println!("  add rax, rdi"),
                    BinOpKind::Sub => println!("  sub rax, rdi"),
                    BinOpKind::Mul => println!("  imul rax, rdi"),
                    BinOpKind::Div => {
                        println!("  cqo");
                        println!("  idiv rdi");
                    }
                    BinOpKind::Eq => {
                        println!("  cmp rax, rdi");
                        println!("  sete al");
                        println!("  movzb rax, al");
                    }
                    BinOpKind::Ne => {
                        println!("  cmp rax, rdi");
                        println!("  setne al");
                        println!("  movzb rax, al");
                    }
                    BinOpKind::Lt => {
                        println!("  cmp rax, rdi");
                        println!("  setl al");
                        println!("  movzb rax, al");
                    }
                    BinOpKind::Le => {
                        println!("  cmp rax, rdi");
                        println!("  setle al");
                        println!("  movzb rax, al");
                    }
                    BinOpKind::Gt => {
                        println!("  cmp rax, rdi");
                        println!("  setg al");
                        println!("  movzb rax, al");
                    }
                    BinOpKind::Ge => {
                        println!("  cmp rax, rdi");
                        println!("  setge al");
                        println!("  movzb rax, al");
                    }
                    // never reaches here
                    BinOpKind::Assign => {}
                }

                println!("  push rax");
            }
            NodeKind::Cond(cond) => {
                let label_index = self.label_num;
                self.label_num += 1;
                match cond.value {
                    CondKind::While { cond, then } => {
                        println!(".L.start.{}:", label_index);
                        self.gen(*cond)?;
                        println!("  pop rax");
                        println!("  test rax, rax");
                        println!("  je .L.end.{}", label_index);
                        self.gen(*then)?;
                        println!("  jmp .L.start.{}", label_index);
                        println!(".L.end.{}:", label_index);
                    }
                    CondKind::If { cond, then, els } => {
                        self.gen(*cond)?;
                        println!("  pop rax");
                        println!("  test rax, rax");
                        if els.is_some() {
                            println!("  je .L.else.{}", label_index);
                        } else {
                            println!("  je .L.end.{}", label_index);
                        }
                        self.gen(*then)?;
                        match els {
                            Some(els) => {
                                println!("  jmp .L.end.{}", label_index);
                                println!(".L.else.{}:", label_index);
                                self.gen(*els)?;
                            }
                            None => {}
                        }
                        println!(".L.end.{}:", label_index);
                    }
                    CondKind::For {
                        init,
                        cond,
                        inc,
                        then,
                    } => {
                        if let Some(init) = init {
                            self.gen(*init)?;
                        }
                        println!("  .L.start.{}:", label_index);
                        if let Some(cond) = cond {
                            self.gen(*cond)?;
                            println!("  test rax, rax");
                            println!("  je .L.end.{}", label_index);
                        }
                        self.gen(*then)?;
                        if let Some(inc) = inc {
                            self.gen(*inc)?;
                        }
                        println!(" jmp .L.start.{}", label_index);
                        println!(".L.end.{}:", label_index);
                    }
                }
            }
            NodeKind::Block { stmts } => {
                for stmt in stmts {
                    self.gen(stmt)?;
                }
            }
        }

        Ok(())
    }
}

/// Checks if the lvalue then calculates the address.
fn lval_gen(node: Node) -> Result<()> {
    match node.value {
        NodeKind::Var { offset, .. } => {
            println!("  lea rdi, [rbp - {}]", offset);
            println!("  push rdi");
            Ok(())
        }
        _ => Err(Error {
            value: ErrorKind::Error("not lvalue"),
            loc: node.loc,
        }),
    }
}
