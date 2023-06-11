use std::env;
use std::iter::{Enumerate, Peekable};
use std::process::exit;
use std::str::Chars;

/// トークン（とその値）保持用の列挙型。
#[derive(Debug)]
enum Token {
    Reserved(char),
    Number(u32),
}

/// トークナイズのための構造体。
struct Tokenizer<'a> {
    input: &'a str,
    pos: Peekable<Enumerate<Chars<'a>>>,
    peeked: Option<Option<Token>>,
    index: usize,
}

impl<'a> Tokenizer<'a> {
    /// トークナイザの初期化。
    /// 
    /// * `input` - トークナイズする入力。
    fn new(input: &str) -> Tokenizer {
        return Tokenizer {
            input,
            pos: input.chars().enumerate().peekable(),
            peeked: None,
            index: 0,
        };
    }

    /// 処理の先頭から数値を取り出す。
    /// 見つかれば`Some(_)`、なければ`None`を返す。
    fn parse_number(&mut self) -> Option<u32> {
        let mut is_first = true;
        let mut n = 0;

        loop {
            match self.pos.peek() {
                Some((i, c)) if c.is_ascii_digit() => {
                    if is_first { is_first = false; }
                    n = n * 10 + (*c as u8 - b'0') as u32;
                    self.index = *i;
                }
                _ => {
                    if is_first { return None; }
                    return Some(n);
                }
            }
            self.pos.next();
        }
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

impl<'a> Iterator for Tokenizer<'a> {
    /// イテレータの `next()` で返す値は `Token` 型。
    type Item = Token;

    /// 次のトークンを読み返す。
    fn next(&mut self) -> Option<Self::Item> {
        match self.peeked.take() {
            Some(content) => return content,
            _ => (),
        };

        let now;

        // 数値かどうかをチェック
        match self.parse_number() {
            Some(num) => return Some(Token::Number(num)),
            _ => (),
        };

        match self.pos.next() {
            None => return None,
            Some((i, c)) => {
                self.index = i;
                now = c;
            },
        }

        // 空白文字ならスキップ
        if now.is_whitespace() {
            return self.next();
        }

        match now {
            '+' => Some(Token::Reserved('+')),
            '-' => Some(Token::Reserved('-')),
            '*' => Some(Token::Reserved('*')),
            '/' => Some(Token::Reserved('/')),
            '(' => Some(Token::Reserved('(')),
            ')' => Some(Token::Reserved(')')),
            _ => {
                // 予約文字でも数値でもなければトークナイズ失敗
                self.error("トークナイズできません。");
            }
        }
    }
}

/// ノードの種類を区別するための列挙型。
#[derive(Debug)]
enum NodeKind {
    Add,
    Sub,
    Mul,
    Div,
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

/// トークナイザから式のノードツリーを作る。
fn expr(tokenizer: &mut Tokenizer) -> NodeTree {
    let mut result = mul(tokenizer);

    while let Some(token) = tokenizer.peek() {
        match token {
            Token::Reserved('+') => {
                tokenizer.next();
                result = NodeTree::new(NodeKind::Add, result, mul(tokenizer));
            },
            Token::Reserved('-') => {
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
    let mut result = primary(tokenizer);

    while let Some(token) = tokenizer.peek() {
        match token {
            Token::Reserved('*') => {
                tokenizer.next();
                result = NodeTree::new(NodeKind::Mul, result, primary(tokenizer));
            },
            Token::Reserved('/') => {
                tokenizer.next();
                result = NodeTree::new(NodeKind::Div, result, primary(tokenizer));
            },
            _ => break,
        }
    }

    result
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
        Some(Token::Reserved('(')) => {
            let result = expr(tokenizer);
            if let Some(Token::Reserved(')')) = tokenizer.next() {
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
        _ => (),
    };

    println!("\tpush rax");
}