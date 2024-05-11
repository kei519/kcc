fn main() -> std::process::ExitCode {
    kcc::main(std::env::args().skip(1)).into()
}
