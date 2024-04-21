fn main() -> std::process::ExitCode {
    let args = std::env::args().collect();
    kcc::main(args).into()
}
