use std::env;
use std::process::exit;

/// トークン（とその値）保持用の列挙型。
#[derive(Debug)]
enum Token<'a> {
    Reserved(&'a str),
    Number(u32),
}

/// トークナイズのための構造体。
struct Tokenizer<'a, 'b> {
    input: &'a str,
    pos: usize,
    peeked: Option<Option<Token<'b>>>,
    index: usize,
}

impl<'a, 'b> Tokenizer<'a, 'b> {
    /// トークナイザの初期化。
    /// 
    /// * `input` - トークナイズする入力。
    fn new(input: &str) -> Tokenizer {
        return Tokenizer {
            /// 入力された文字列
            input,
            /// 現在のスライスの位置
            pos: 0,
            /// peek した次のトークン
            peeked: None,
            /// 現在何文字読んだか
            index: 0,
        };
    }

    /// 処理の先頭から数値を取り出す。
    /// 見つかれば`Some(_)`、なければ`None`を返す。
    fn parse_number(&mut self) -> Option<u32> {
        let mut is_first = true;
        let mut n = 0;

        loop {
            match self.input[self.pos..].chars().next() {
                Some(c) if c.is_ascii_digit() => {
                    if is_first { is_first = false; }
                    n = n * 10 + (c as u8 - b'0') as u32;
                    self.pos += c.len_utf8();
                    self.index += 1;
                }
                _ => {
                    if is_first { return None; }
                    return Some(n);
                }
            }
        }
    }

	/// 空白文字以外が来るまで読み飛ばす。
    /// 読み飛ばさなかった場合は `false`、
    /// 読み飛ばした場合は `true` を返す。
    /// （今後スペースがあるか大事な局面があるため）
    fn skip_whitespace(&mut self) -> bool {
        let mut is_first = true;

        while let Some(now) = self.input[self.pos..].chars().next() {
            if now.is_ascii_whitespace() {
                if is_first {
                    is_first = false;
                }
                self.pos += now.len_utf8();
                self.index += 1;
            } else {
                break;
            }
        }
        !is_first
    }

	/// 先頭が指定された文字列と一致しているかを返す。
    /// また、もし一致していた場合はその分だけ読み飛ばす。
    fn cmp(&mut self, s: &str) -> bool {
        let result = self.input[self.pos..].starts_with(s);

        if result {
            for c in s.chars() {
                self.pos += c.len_utf8();
            }
            self.index += s.len();
        }

        result
    }

    /// 次のトークンを消費せずに見る。
    fn peek(&mut self) -> Option<&Token> {
        let next = self.next();
        self.peeked.get_or_insert_with(|| next).as_ref()
    }

    /// エラーメッセージを出す。
    /// 
    /// * `message` - 出力するメッセージ
    fn error(&self, message: &str) -> ! {
        eprintln!("{}", self.input);

        for _ in 0..self.index {
            eprint!(" ");
        }

        eprint!("^ ");
        eprintln!("{}", message);

        exit(1)
    }
}

impl<'a, 'b> Iterator for Tokenizer<'a, 'b> {
    /// イテレータの `next()` で返す値は `Token` 型。
    type Item = Token<'b>;

    /// 次のトークンを読み返す。
    fn next(&mut self) -> Option<Self::Item> {
        match self.peeked.take() {
            Some(content) => return content,
            _ => (),
        };

        self.skip_whitespace();

		// 最後まで読んでいたら `None` を返す
        if self.index == self.input.len() {
            return None;
        }

        // 数値かどうかをチェック
        match self.parse_number() {
            Some(num) => return Some(Token::Number(num)),
            _ => (),
        };

        if self.cmp("+") {
            Some(Token::Reserved("+"))
        } else if self.cmp("-") {
            Some(Token::Reserved("-"))
        } else if self.cmp("*") {
            Some(Token::Reserved("*"))
        } else if self.cmp("/") {
            Some(Token::Reserved("/"))
        } else if self.cmp("(") {
            Some(Token::Reserved("("))
        } else if self.cmp(")") {
            Some(Token::Reserved(")"))
        } else if self.cmp("<=") {
            Some(Token::Reserved("<="))
        } else if self.cmp(">=") {
            Some(Token::Reserved(">="))
        } else if self.cmp("<") {
            Some(Token::Reserved("<"))
        } else if self.cmp(">") {
            Some(Token::Reserved(">"))
        } else if self.cmp("==") {
            Some(Token::Reserved("=="))
        } else if self.cmp("!=") {
            Some(Token::Reserved("!="))
        } else {
            self.error("トークナイズできません。")
        }
    }
}

/// ノードの種類を区別するための列挙型。
#[derive(Debug)]
enum NodeKind {
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
struct Node {
    kind: NodeKind,
    lhs: NodeTree,
    rhs: NodeTree,
}

impl Node {
    /// 自身のノードの種類と左右のノードからノードの本体を作る。
    fn new(kind: NodeKind, lhs: NodeTree, rhs: NodeTree) -> Node {
        Node {
            kind,
            lhs,
            rhs,
        }
    }
}

/// ノードの本体を持つポインタ。
#[derive(Debug)]
enum NodeTree {
    Empty,
    NonEmpty(Box<Node>),
}

impl NodeTree {
    /// 自身のノードの種類と左右のノード本体からノードへのポインタを作る。
    fn new(kind: NodeKind, lhs: NodeTree, rhs: NodeTree) -> NodeTree {
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

fn main() {
    // 引数を取得
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("引数の個数が正しくありません。");
        exit(1);
    }

    let input: &str = &args[1];

    let mut tokenizer = Tokenizer::new(input);

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

    let tree = expr(&mut tokenizer);
    gen(tree);

    println!("\tpop rax");
    println!("\tret");
}

fn expr(tokenizer: &mut Tokenizer) -> NodeTree {
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
fn gen(tree: NodeTree) {
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