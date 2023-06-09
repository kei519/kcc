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
            index: 0,
        };
    }

    /// 次のトークンが数であったらその数値を返す。
    /// そうでなければエラー出力をして終了。
    fn expect_num(&mut self) -> u32 {
        if let Some(Token::Number(num)) = self.next() {
            return num;
        }
        self.error("数ではありません");
        exit(1);
    }

    /// 先頭から数値を取り出して返す。
    /// 先頭が数値でなければ`None`。
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

    /// エラーメッセージを出す。
    /// 
    /// * `message` - 出力するメッセージ
    fn error(&self, message: &str) {
        eprintln!("{}", self.input);

        for _ in 0..self.index {
            eprint!(" ");
        }

        eprint!("^ ");
        eprintln!("{}", message);

        exit(1);
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    /// イテレータの `next()` で返す値は `Token` 型。
    type Item = Token;

    /// 次のトークンを読み返す。
    fn next(&mut self) -> Option<Self::Item> {
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

        if now == '+' {
            return Some(Token::Reserved('+'));
        }

        if now == '-' {
            return Some(Token::Reserved('-'));
        }

        // 予約文字でも数値でもなければトークナイズ失敗
        self.error("トークナイズできません。");
        exit(1);
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

    println!("\tmov rax, {}", tokenizer.expect_num());

    while let Some(token) = tokenizer.next() {
        match token {
            Token::Reserved('+') => {
                println!("\tadd rax, {}", tokenizer.expect_num())
            },
            Token::Reserved('-') => {
                println!("\tsub rax, {}", tokenizer.expect_num())
            },
            _ => {
                tokenizer.error("演算子が見つかりません");
                exit(1);
            },
        }
    }

    println!("\tret");
}
