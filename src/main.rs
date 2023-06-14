mod parse;
mod codegen;

use std::env;
use std::process::exit;
use crate::parse::*;
use crate::codegen::*;

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