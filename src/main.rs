use std::env;

fn main() {
    // 引数を取得
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("引数の個数が正しくありません。");
        return;
    }

    let num: u8 = match args[1].parse() {
        Ok(num) => num,
        Err(_) => {
            eprintln!("0-255 の範囲の整数を入力してください。");
            return;
        }
    };

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");
    println!("\tmov rax, {num}");
    println!("\tret");
}
