use crate::*;

/// Generates the assembly code form the AST top node.
pub fn codegen(input: &str) -> Result<()> {
    let tokens = Tokenizer::new(input).tokenize()?;
    let mut parser = Parser::new(tokens);

    // prepare for assembler
    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

    let node = parser.expr()?;
    gen(node);

    // return code
    println!("  pop rax");
    println!("  ret");

    Ok(())
}

/// Generates the assembly code from a top node.
fn gen(node: Node) {
    let Node { value: node, .. } = node;
    match node {
        // When the node is a number, it is a terminal.
        NodeKind::Num(val) => {
            println!("  push {}", val);
            return;
        }
        NodeKind::UnOp { op, arg } => {
            gen(*arg);

            println!("  pop rax");

            match op.value {
                UnOpKind::Pos => (),
                UnOpKind::Neg => println!("  neg rax"),
            }

            println!("  push rax");
        }
        NodeKind::BinOp { op, left, right } => {
            // When the node is a binary operator, it has left and right side.
            gen(*left);
            gen(*right);

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
            }

            println!("  push rax");
        }
    }
}
