mod codegen;
mod parse;

use crate::codegen::*;
use crate::parse::*;
use std::env;
use std::process::exit;

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

    // ツリーを生成する。
    let trees = program(&mut tokenizer);

    // プロローグ
    println!("\tpush rbp");
    println!("\tmov rbp, rsp");
    println!("\tsub rsp, {}", tokenizer.num_lvar() * 8);

    for tree in trees {
        gen(tree);
    }

    println!("\tpop rax");

    // エピローグ
    println!("\tmov rsp, rbp");
    println!("\tpop rbp");

    println!("\tret");
}
