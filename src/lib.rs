/// Executes the main logic.
///
/// * args - command-line arguments
///
/// # Return
///
/// Exit code.
pub fn main(args: Vec<String>) -> u8 {
    if args.len() != 2 {
        eprintln!("Only ONE argument is required.");
        return 1;
    }

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");
    println!("  mov rax, {}", args[1].parse::<i32>().unwrap());
    println!("  ret");
    0
}
