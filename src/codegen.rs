use crate::*;

const ARGREG: [&'static str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

/// Represents variable.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    /// The name of the variable.
    pub name: &'static str,
    /// The offset from the function frame top (rbp).
    pub offset: isize,
}

impl Var {
    /// Constructor.
    pub fn new(name: &'static str, offset: isize) -> Self {
        Self { name, offset }
    }
}

/// Represents code generator.
pub struct Generator {
    input: &'static str,
    label_num: usize,
    stack_len: usize,
    vars: Vec<Var>,
}

impl Generator {
    /// Constructor.
    pub fn new(input: &'static str) -> Self {
        Self {
            input,
            label_num: 0,
            stack_len: 0,
            vars: vec![],
        }
    }

    /// Generates the assembly code form the AST top node.
    pub fn codegen(&mut self) -> Result<()> {
        let tokens = Tokenizer::new(self.input).tokenize()?;
        let mut parser = Parser::new(tokens);

        let program = parser.program()?;

        // main must be public.
        println!(".global main");
        for node in program {
            self.gen(node)?;
        }

        Ok(())
    }

    /// Generates the assembly code from a top node.
    fn gen(&mut self, node: Node) -> Result<()> {
        let Node { value: node, loc } = node;
        match node {
            // When the node is a number, it is a terminal.
            NodeKind::Num(val) => {
                self.push_imm(val);
                return Ok(());
            }
            NodeKind::Var(name) => {
                let var = self.find_var(name, loc)?;
                if var.offset >= 0 {
                    println!("  lea -{}(%rbp), %rdi", var.offset);
                } else {
                    println!("  lea {}(%rbp), %rdi", -var.offset);
                }
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
            NodeKind::UnOp { op, arg } => match op.value {
                UnOpKind::Pos => self.gen(*arg)?,
                UnOpKind::Neg => {
                    self.gen(*arg)?;
                    self.pop("rax");
                    println!("  neg %rax");
                    self.push("rax");
                }
                UnOpKind::ExprStmt => {
                    self.gen(*arg)?;
                    self.stack_len -= 1;
                    println!("  add ${}, %rsp", MEMORY_SIZE);
                }
                UnOpKind::Addr => self.lval_gen(*arg)?,
                UnOpKind::Deref => {
                    self.gen(*arg)?;
                    self.pop("rdi");
                    println!("  mov (%rdi), %rax");
                    self.push("rax");
                }
            },
            NodeKind::BinOp { op, left, right } => {
                match op.value {
                    BinOpKind::Assign => {
                        self.lval_gen(*left)?;
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
            NodeKind::FnCall { name, mut args } => {
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
            NodeKind::FnDef {
                name,
                params,
                locals,
                stmts,
            } => {
                use std::cmp::min;

                println!("{}:", name);

                let deploy_param_num = min(params.len(), 6);
                self.stack_len = deploy_param_num + locals.len();

                // prologue
                self.push("rbp");
                println!("  mov %rsp, %rbp");
                if self.stack_len > 0 {
                    println!("  sub ${}, %rsp", self.stack_len * MEMORY_SIZE);
                }

                // deploys parameters on the stack.
                for i in 0..deploy_param_num {
                    let offset = (i + 1) * MEMORY_SIZE;
                    println!("  mov %{}, -{}(%rbp)", ARGREG[i], offset);
                    self.vars.push(Var::new(params[i], offset as isize))
                }

                // sets args offsets of the parameters passed with the stack.
                for i in 6..params.len() {
                    self.vars
                        .push(Var::new(params[i], -(((i - 4) * MEMORY_SIZE) as isize)));
                }

                // registers local variables
                for (i, name) in locals.into_iter().enumerate() {
                    self.vars.push(Var::new(
                        name,
                        ((i + deploy_param_num + 1) * MEMORY_SIZE) as isize,
                    ));
                }

                // process
                for stmt in stmts {
                    self.gen(stmt)?;
                }
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

    /// Checks if the lvalue then calculates the address.
    fn lval_gen(&mut self, node: Node) -> Result<()> {
        match node.value {
            NodeKind::Var(name) => {
                let var = self.find_var(name, node.loc)?;

                if var.offset >= 0 {
                    println!("  lea -{}(%rbp), %rdi", var.offset);
                } else {
                    println!("  lea {}(%rbp), %rdi", -var.offset)
                }
                println!("  push %rdi");
                Ok(())
            }
            NodeKind::UnOp { op, arg } if op.value == UnOpKind::Deref => self.gen(*arg),
            _ => Err(Error {
                value: ErrorKind::Error("not lvalue"),
                loc: node.loc,
            }),
        }
    }

    fn find_var(&self, name: &'static str, loc: Loc) -> Result<&Var> {
        self.vars.iter().find(|v| v.name == name).ok_or(Error {
            value: ErrorKind::Error("undefined variable"),
            loc,
        })
    }
}
