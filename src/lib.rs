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

    // prepare for assembler
    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

    let input = args[1].as_str().as_bytes();
    let mut pos = 0;

    let (num, p) = str_to_num(input, pos).unwrap();
    println!("  mov rax, {}", num);
    pos = p;

    while pos < input.len() {
        if input[pos] == '+' as u8 {
            pos += 1;
            let (num, p) = str_to_num(input, pos).unwrap();
            println!("  add rax, {}", num);
            pos = p;
            continue;
        }

        if input[pos] == '-' as u8 {
            pos += 1;
            let (num, p) = str_to_num(input, pos).unwrap();
            println!("  sub rax, {}", num);
            pos = p;
            continue;
        }

        eprintln!("unexpected character: {} as {}", input[pos] as char, pos);
        return 1;
    }

    // return code
    println!("  ret");

    0
}

fn str_to_num(input: &[u8], mut pos: usize) -> Result<(u64, usize), Error> {
    use std::str::from_utf8;

    let start = pos;
    while pos < input.len() && input[pos].is_ascii_digit() {
        pos += 1;
    }

    if start == pos {
        Err(Error::ParseError)
    } else {
        let n = from_utf8(&input[start..pos])
            // This always succeeds becase of construction
            .unwrap()
            .parse()
            // So does this!
            .unwrap();
        Ok((n, pos))
    }
}

#[derive(Debug)]
/// Represents all errros.
enum Error {
    ParseError,
    Eof,
}
