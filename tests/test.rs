use std::process::Command;

/// Run a test with the given input and asserts the exit code and the expected.
macro_rules! test {
    ($input:expr, $expected:expr) => {
        assert_eq!(run_test($input), $expected as i32);
    };
}

#[test]
fn test_num1() {
    test!("0", 0);
}

#[test]
fn test_num2() {
    test!("47", 47);
}

#[test]
fn test_rand_num() {
    let rand = kcc::rand() as u8;
    test!(rand.to_string(), rand);
}

/// Run a test with the given input and returns the exit code.
fn run_test(input: impl Into<String>) -> i32 {
    let test_file = kcc::mktemp().unwrap();
    kcc::main(["-o", test_file.to_str().unwrap(), input.into().as_str()]).unwrap();
    let result = Command::new(test_file).output().unwrap();
    result.status.code().unwrap()
}
