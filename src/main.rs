use std::env;
use std::process::exit;

/// トークン（とその値）保持用の列挙型。
enum Token {
    Reserved(char),
    Number(i32),
}

/// トークナイズのための構造体。
struct Tokenizer<'a> {
    str: &'a str,
}


impl<'a> Tokenizer<'a> {
    /// トークナイザの初期化。
    /// 
    /// * `input` - トークナイズする入力。
    fn new(input: &str) -> Tokenizer {
        return Tokenizer {
            str: input,
        };
    }

    /// 次のトークンが数であったらその数値を返す。
    /// そうでなければエラー出力をして終了。
    fn expect_num(&mut self) -> i32 {
        if let Some(Token::Number(num)) = self.next() {
            return num;
        }
        eprintln!("数ではありません");
        exit(1);
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    /// イテレータの `next()` で返す値は `Token` 型。
    type Item = Token;

    /// 次のトークンを読み返す。
    fn next(&mut self) -> Option<Self::Item> {
        // 先頭の空白文字を取り除く
        self.str = self.str.trim_start();

        // 空ならトークン終了なので None を返す
        if self.str.is_empty() {
            return None;
        }

        if self.str.as_bytes()[0] == b'+' {
            // '+' を取り除く。
            self.str = self.str.split_at(1).1;
            return Some(Token::Reserved('+'));
        }

        if self.str.as_bytes()[0] == b'-' {
            // '-' を取り除く
            self.str = self.str.split_at(1).1;
            return Some(Token::Reserved('-'));
        }

        let num;
        (num, self.str) = split_digit(self.str);
        match num {
            Some(num) => return Some(Token::Number(num)),
            None => {
                // 予約文字でも数値でもなければトークナイズ失敗
                eprintln!("トークナイズできません。");
                exit(1);
            },
        }
    }
}

/// 文字列を受け取り、先頭が数値であればその数値を、
/// そしてその数値部分を切り離した文字列を返す。
fn split_digit(s: &str) -> (Option<i32>, &str) {
    // 初めに数値でない文字が現れるインデックス
    let first_non_num= s.
        find(|c| !char::is_numeric(c)).unwrap_or(s.len());
    let (num, left) = s.split_at(first_non_num);
    let num = match num.parse() {
        Ok(n) => Some(n),
        Err(_) => None,
    };
    return (num, left);
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
                eprintln!("演算子が見つかりません");
                exit(1);
            },
        }
    }

    println!("\tret");
}
