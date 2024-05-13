fn main() -> std::process::ExitCode {
    let args = std::env::args().skip(1).collect();
    kcc::main(args).into()
}
