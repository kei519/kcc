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

pub(crate) fn expr(tokenizer: &mut Tokenizer) -> NodeTree {
    equality(tokenizer)
}

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

/// トークナイザから式のノードツリーを作る。
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

/// トークナイザから乗除項のノードツリーを作成。
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

/// トークナイザから和差項のノードツリーを作成。
fn primary(tokenizer: &mut Tokenizer) -> NodeTree {
    match tokenizer.next() {
        Some(Token::Number(num)) => {
            NodeTree::new(
                NodeKind::Num(num),
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

/// ノードツリーからアセンブラを作成。
pub(crate) fn gen(tree: NodeTree) {
    let content;

    match tree {
        NodeTree::Empty => return,
        NodeTree::NonEmpty(b) => content = b,
    };

    if let NodeKind::Num(num) = content.kind {
        println!("\tpush {}", num);
        return;
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