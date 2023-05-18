use std::{env, process::exit};

fn main() {
    // 引数を取得
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("引数の個数が正しくありません。");
        return;
    }

    let mut p: &str = &args[1];

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");
    let mut num = 0;
    (num, p) = split_digit(p);
    println!("\tmov rax, {num}");

    while !p.is_empty() {
        if p.chars().nth(0).unwrap() == '+' {
            (_, p) = p.split_at(1);
            let mut num = 0;
            (num, p) = split_digit(p);
            println!("\tadd rax, {num}");
            continue;
        }

        if p.chars().nth(0).unwrap() == '-' {
            (_, p) = p.split_at(1);
            let mut num = 0;
            (num, p) = split_digit(p);
            println!("\tsub rax, {num}");
            continue;
        }
        
        eprintln!("予期しない文字列です： {}", p.chars().nth(0).unwrap());
        exit(1);
    }
    
    println!("\tret");
}

fn split_digit(s: &str) -> (i32, &str) {
    let first_non_num = s.find(|c| !char::is_numeric(c)).unwrap_or(s.len());
    let (num, left) = s.split_at(first_non_num);
    let num: i32 = match num.parse() {
        Ok(n) => n,
        Err(_) => i32::default(),
    };
    return (num, left);
}
