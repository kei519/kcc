use crate::*;

const ARGREG: [&'static str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

/// Represents code generator.
pub struct Generator {
    input: &'static str,
    label_num: usize,
    stack_len: usize,
}

impl Generator {
    /// Constructor.
    pub fn new(input: &'static str) -> Self {
        Self {
            input,
            label_num: 0,
            stack_len: 0,
        }
    }

    /// Generates the assembly code form the AST top node.
    pub fn codegen(&mut self) -> Result<()> {
        let tokens = Tokenizer::new(self.input).tokenize()?;
        let mut parser = Parser::new(tokens);

        // prepare for assembler
        println!(".global main");
        println!("main:");
        self.push("rbp");
        println!("  mov %rsp, %rbp");

        let (nodes, var_num) = parser.parse()?;

        // take the space for the variables
        println!("  sub ${}, %rsp", var_num * MEMORY_SIZE);
        self.stack_len += var_num;

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
                self.push_imm(val);
                return Ok(());
            }
            NodeKind::Var { offset, .. } => {
                println!("  lea -{}(%rbp), %rdi", offset);
                println!("  mov (%rdi), %rax");
                self.push("rax");
            }
            NodeKind::Return { val } => {
                self.gen(*val)?;

                self.pop("rax");
                println!("  mov %rbp, %rsp");
                self.pop("rbp");
                println!("  ret");
            }
            NodeKind::UnOp { op, arg } => {
                self.gen(*arg)?;
                match op.value {
                    UnOpKind::Pos => {}
                    UnOpKind::Neg => {
                        self.pop("rax");
                        println!("  neg %rax");
                        self.push("rax");
                    }
                    UnOpKind::ExprStmt => {
                        self.stack_len -= 1;
                        println!("  add ${}, %rsp", MEMORY_SIZE);
                    }
                }
            }
            NodeKind::BinOp { op, left, right } => {
                match op.value {
                    BinOpKind::Assign => {
                        lval_gen(*left)?;
                        self.gen(*right)?;
                        self.pop("rax");
                        self.pop("rdi");
                        println!("  mov %rax, (%rdi)");
                        self.push("rax");
                        return Ok(());
                    }
                    _ => {}
                }
                // When the node is a binary operator, it has left and right side.
                self.gen(*left)?;
                self.gen(*right)?;

                self.pop("rdi");
                self.pop("rax");

                match op.value {
                    BinOpKind::Add => println!("  add %rdi, %rax"),
                    BinOpKind::Sub => println!("  sub %rdi, %rax"),
                    BinOpKind::Mul => println!("  imul %rdi, %rax"),
                    BinOpKind::Div => {
                        println!("  cqo");
                        println!("  idiv %rdi");
                    }
                    BinOpKind::Eq => {
                        println!("  cmp %rdi, %rax");
                        println!("  sete %al");
                        println!("  movzb %al, %rax");
                    }
                    BinOpKind::Ne => {
                        println!("  cmp %rdi, %rax");
                        println!("  setne %al");
                        println!("  movzb %al, %rax");
                    }
                    BinOpKind::Lt => {
                        println!("  cmp %rdi, %rax");
                        println!("  setl %al");
                        println!("  movzb %al, %rax");
                    }
                    BinOpKind::Le => {
                        println!("  cmp %rdi, %rax");
                        println!("  setle %al");
                        println!("  movzb %al, %rax");
                    }
                    BinOpKind::Gt => {
                        println!("  cmp %rdi, %rax");
                        println!("  setg %al");
                        println!("  movzb %al, %rax");
                    }
                    BinOpKind::Ge => {
                        println!("  cmp %rdi, %rax");
                        println!("  setge %al");
                        println!("  movzb %al, %rax");
                    }
                    // never reaches here
                    BinOpKind::Assign => {}
                }

                self.push("rax");
            }
            NodeKind::Cond(cond) => {
                let label_index = self.label_num;
                self.label_num += 1;
                match cond.value {
                    CondKind::While { cond, then } => {
                        println!(".L.start.{}:", label_index);
                        self.gen(*cond)?;
                        self.pop("rax");
                        println!("  test %rax, %rax");
                        println!("  je .L.end.{}", label_index);
                        self.gen(*then)?;
                        println!("  jmp .L.start.{}", label_index);
                        println!(".L.end.{}:", label_index);
                    }
                    CondKind::If { cond, then, els } => {
                        self.gen(*cond)?;
                        self.pop("rax");
                        println!("  test %rax, %rax");
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
                            println!("  test %rax, %rax");
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
            NodeKind::FnCall(func) => {
                let FuncKind { name, mut args } = func.value;

                // adjust rsp to a multiple of 16.
                let rev_stack_num = if (self.stack_len + args.len()) % 2 == 0 {
                    println!("  sub ${}, %rsp", MEMORY_SIZE);
                    1
                } else {
                    0
                };
                let rev_stack_num = rev_stack_num + if args.len() > 6 { args.len() - 6 } else { 0 };

                // have to put seventh or above arguments on the stack.
                while args.len() > 6 {
                    let arg = args.pop().unwrap();
                    self.gen(arg)?;
                    self.pop("rax");
                    self.push("rax");
                }

                // calculates others.
                let regarg_num = args.len();
                while args.len() != 0 {
                    let arg = args.pop().unwrap();
                    self.gen(arg)?;
                }

                for i in 0..regarg_num {
                    self.pop(ARGREG[i]);
                }
                println!("  call {}", name);

                // revert the stack
                println!("  add ${}, %rsp", rev_stack_num * MEMORY_SIZE);
                self.push("rax");
            }
        }

        Ok(())
    }

    /// Pushes a register value.
    fn push(&mut self, reg: &str) {
        self.stack_len += 1;
        println!("  push %{}", reg);
    }

    /// Pushed a number.
    fn push_imm(&mut self, num: u64) {
        self.stack_len += 1;
        println!("  push ${}", num);
    }

    fn pop(&mut self, reg: &str) {
        println!("  pop %{}", reg);
        self.stack_len -= 1;
    }
}

/// Checks if the lvalue then calculates the address.
fn lval_gen(node: Node) -> Result<()> {
    match node.value {
        NodeKind::Var { offset, .. } => {
            println!("  lea -{}(%rbp), %rdi", offset);
            println!("  push %rdi");
            Ok(())
        }
        _ => Err(Error {
            value: ErrorKind::Error("not lvalue"),
            loc: node.loc,
        }),
    }
}
