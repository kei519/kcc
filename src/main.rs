fn main() -> std::process::ExitCode {
    let args = std::env::args().skip(1);
    match kcc::main(args) {
        Ok(_) => 0,
        Err(e) => {
            eprintln!("{}", e);
            1
        }
    }
    .into()
}
