fn main() -> std::process::ExitCode {
    let args = std::env::args().skip(1);
    kcc::main(args).into()
}
