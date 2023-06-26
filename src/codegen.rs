use crate::parse::*;

/// ノードの種類を区別するための列挙型。
#[derive(Debug)]
pub(crate) enum NodeKind {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Eq, // ==
    NE, // !=
    Lt, // <
    LE, // <=
    Assign,
    Return,
    LVar(usize),
    Num(u32),
}

/// ノードの種類と、左右を持っている構造体。
/// ノードの本体。
#[derive(Debug)]
pub(crate) struct Node {
    kind: NodeKind,
    lhs: NodeTree,
    rhs: NodeTree,
}

impl Node {
    /// 自身のノードの種類と左右のノードからノードの本体を作る。
    pub(crate) fn new(kind: NodeKind, lhs: NodeTree, rhs: NodeTree) -> Node {
        Node {
            kind,
            lhs,
            rhs,
        }
    }
}

/// ノードの本体を持つポインタ。
#[derive(Debug)]
pub(crate) enum NodeTree {
    Empty,
    NonEmpty(Box<Node>),
}

impl NodeTree {
    /// 自身のノードの種類と左右のノード本体からノードへのポインタを作る。
    pub(crate) fn new(kind: NodeKind, lhs: NodeTree, rhs: NodeTree) -> NodeTree {
        NodeTree::NonEmpty(Box::new(Node::new(kind, lhs, rhs)))
    }

    /// 自身とその下のノードを表示する。
    #[allow(dead_code)]
    fn show(&self) {
        let content;

        match self {
            NodeTree::Empty => return,
            NodeTree::NonEmpty(b) => content = b,
        }

        content.lhs.show();

        println!("{:?}", content.kind);

        content.rhs.show();
    }
}

pub(crate) fn program(tokenizer: &mut Tokenizer) -> Vec<NodeTree> {
    let mut result = Vec::new();

    while let Some(_) = tokenizer.peek() {
        result.push(stmt(tokenizer));
    }

    result
}

pub(crate) fn stmt(tokenizer: &mut Tokenizer) -> NodeTree {
    let result;

    if let Some(Token::Return) = tokenizer.peek() {
        tokenizer.next();
        result = NodeTree::new(NodeKind::Return,
                             expr(tokenizer),
                             NodeTree::Empty);
    } else {
        result = expr(tokenizer);
    }

    if let Some(Token::Reserved(";")) = tokenizer.peek() {
        tokenizer.next();
        result
    } else {
        tokenizer.error(r#"";" で文が終了していません。"#)
    }
}

/// 式をノードツリーに変換する。
pub(crate) fn expr(tokenizer: &mut Tokenizer) -> NodeTree {
    assign(tokenizer)
}

pub(crate) fn assign(tokenizer: &mut Tokenizer) -> NodeTree {
    let mut result = equality(tokenizer);

    if let Some(Token::Reserved("=")) = tokenizer.peek() {
        tokenizer.next();
        result = NodeTree::new(NodeKind::Assign, result, assign(tokenizer));
    }

    result
}

/// 等号・不等号条件をノードツリーに変換する。
fn equality(tokenizer: &mut Tokenizer) -> NodeTree {
    let mut result = relational(tokenizer);

    while let Some(token) = tokenizer.peek() {
        match token {
            Token::Reserved("==") => {
                tokenizer.next();
                result = NodeTree::new(NodeKind::Eq, result, relational(tokenizer))
            },
            Token::Reserved("!=") => {
                tokenizer.next();
                result = NodeTree::new(NodeKind::NE, result, relational(tokenizer))
            },
            _ => break,
        }
    }

    result
}

/// 不等号条件をノードツリーに変換する。
fn relational(tokenizer: &mut Tokenizer) -> NodeTree {
    let mut result = add(tokenizer);

    while let Some(token) = tokenizer.peek() {
        match token {
            Token::Reserved("<=") => {
                tokenizer.next();
                result = NodeTree::new(NodeKind::LE, result, add(tokenizer));
            },
            Token::Reserved("<") => {
                tokenizer.next();
                result = NodeTree::new(NodeKind::Lt, result, add(tokenizer));
            },
            Token::Reserved(">=") => {
                tokenizer.next();
                result = NodeTree::new(NodeKind::LE, add(tokenizer), result);
            },
            Token::Reserved(">") => {
                tokenizer.next();
                result = NodeTree::new(NodeKind::Lt, add(tokenizer), result);
            },
            _ => break,
        }
    }

    result
}

/// 加減項のノードツリーに変換する。
fn add(tokenizer: &mut Tokenizer) -> NodeTree {
    let mut result = mul(tokenizer);

    while let Some(token) = tokenizer.peek() {
        match token {
            Token::Reserved("+") => {
                tokenizer.next();
                result = NodeTree::new(NodeKind::Add, result, mul(tokenizer));
            },
            Token::Reserved("-") => {
                tokenizer.next();
                result = NodeTree::new(NodeKind::Sub, result, mul(tokenizer));
            },
            _ => break,
        }
    }

    result
}

/// 乗除項のノードツリーに変換する。
fn mul(tokenizer: &mut Tokenizer) -> NodeTree {
    let mut result = unary(tokenizer);

    while let Some(token) = tokenizer.peek() {
        match token {
            Token::Reserved("*") => {
                tokenizer.next();
                result = NodeTree::new(NodeKind::Mul, result, unary(tokenizer));
            },
            Token::Reserved("/") => {
                tokenizer.next();
                result = NodeTree::new(NodeKind::Div, result, unary(tokenizer));
            },
            _ => break,
        }
    }

    result
}

/// 単項プラス・マイナスをノードツリーに変換する。
fn unary(tokenizer: &mut Tokenizer) -> NodeTree {
    match tokenizer.peek() {
        Some(Token::Reserved("+")) => {
            tokenizer.next();
            primary(tokenizer)
        },
        Some(Token::Reserved("-")) => {
            tokenizer.next();
            let zero_node = NodeTree::new(NodeKind::Num(0), NodeTree::Empty, NodeTree::Empty);
            NodeTree::new(NodeKind::Sub, zero_node, primary(tokenizer))
        },
        _ => primary(tokenizer),
    }
}

/// 数値、もしくは括弧で囲まれた式をノードツリーに変換する。
fn primary(tokenizer: &mut Tokenizer) -> NodeTree {
    match tokenizer.next() {
        Some(Token::Number(num)) => {
            NodeTree::new(
                NodeKind::Num(num),
                NodeTree::Empty,
                NodeTree::Empty
            )
        },
        Some(Token::Identifier(offset)) => {
            NodeTree::new(
                NodeKind::LVar(offset),
                NodeTree::Empty,
                NodeTree::Empty
            )
        },
        Some(Token::Reserved("(")) => {
            let result = expr(tokenizer);
            if let Some(Token::Reserved(")")) = tokenizer.next() {
                result
            } else {
                tokenizer.error("')' で閉じられていません。");
            }
        },
        _ => tokenizer.error("数ではありません。"),
    }
}

/// 左辺値であるかを検証し、左辺値であればそのアドレスをpush。
fn gen_lvar(tree: NodeTree) {
    let content;

    match tree {
        NodeTree::Empty => {
            eprintln!("代入の左辺値が変数ではありません。");
            std::process::exit(1);
        },
        NodeTree::NonEmpty(b) => content = b,
    };

    if let NodeKind::LVar(offset) = content.kind {
        println!("\tmov rax, rbp");
        println!("\tsub rax, {}", offset);
        println!("\tpush rax");
    } else {
        eprintln!("代入の左辺値が変数ではありません。");
        std::process::exit(1)
    }
}

/// ノードツリーからアセンブラを作成。
pub(crate) fn gen(tree: NodeTree) {
    let content;

    match tree {
        NodeTree::Empty => return,
        NodeTree::NonEmpty(b) => content = b,
    };

    match content.kind {
        NodeKind::Return => {
            gen(content.lhs);
            println!("\tpop rax");
            println!("\tmov rsp, rbp");
            println!("pop rbp");
            println!("ret");
            return;
        }
        NodeKind::Num(num) => {
            println!("\tpush {}", num);
            return;
        },
        NodeKind::LVar(offset) => {
            println!("\tmov rax, rbp");
            println!("\tsub rax, {}", offset);
            println!("\tmov rax, [rax]");
            println!("\tpush rax");
            return;
        },
        NodeKind::Assign => {
            gen_lvar(content.lhs);
            gen(content.rhs);

            println!("\tpop rdi");
            println!("\tpop rax");
            println!("\tmov [rax], rdi");
            println!("\tpush rdi");
            return;
        },
        _ => (),
    }

    gen(content.lhs);
    gen(content.rhs);

    println!("\tpop rdi");
    println!("\tpop rax");

    match content.kind {
        NodeKind::Add => println!("\tadd rax, rdi"),
        NodeKind::Sub => println!("\tsub rax, rdi"),
        NodeKind::Mul => println!("\timul rax, rdi"),
        NodeKind::Div => {
            println!("\tcqo");
            println!("\tidiv rdi");
        },
        NodeKind::Eq => {
            println!("\tcmp rax, rdi");
            println!("\tsete al");
            println!("\tmovzb rax, al");
        },
        NodeKind::NE => {
            println!("\tcmp rax, rdi");
            println!("\tsetne al");
            println!("\tmovzb rax, al");
        },
        NodeKind::Lt => {
            println!("\tcmp rax, rdi");
            println!("\tsetl al");
            println!("\tmovzb rax, al");
        },
        NodeKind::LE => {
            println!("\tcmp rax, rdi");
            println!("\tsetle al");
            println!("\tmovzb rax, al");
        },
        _ => (),
    };

    println!("\tpush rax");
}