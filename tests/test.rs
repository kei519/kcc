use std::{
    hash::{DefaultHasher, Hash, Hasher},
    path::Path,
    process::{self, Command},
    time::SystemTime,
};

const TEST_DIR: &str = "tmp";

#[test]
fn imed_value_test() {
    let exit_code = rand() as u8;

    // Checks if compiling successes.
    assert_eq!(kcc::main(vec![format!("{}", exit_code)]), 0);

    let mut process = Command::new(kcc::DEFAULT_OUTPUT).spawn().unwrap();
    let exit_status = process.wait().unwrap();

    assert_eq!(exit_status.code().unwrap(), exit_code as i32);
}

#[test]
fn test_specified_output() {
    let test_dir = Path::new(TEST_DIR);
    let output_path = test_dir.join(format!("kcc-{}", rand() as u16));

    let exit_code = rand() as u8;

    let args = ["-o", output_path.to_str().unwrap(), &exit_code.to_string()].map(|arg| arg.into());
    assert_eq!(kcc::main(args), 0);

    let mut process = Command::new(output_path).spawn().unwrap();
    let exit_status = process.wait().unwrap();

    assert_eq!(exit_status.code().unwrap(), exit_code as i32);
}

fn rand() -> u64 {
    let mut hasher = DefaultHasher::new();
    SystemTime::now().hash(&mut hasher);
    process::id().hash(&mut hasher);
    hasher.finish()
}
