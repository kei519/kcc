use std::{fs, process::Command, str};

const TEST_DIR: &str = "tests";

/// Compile C files in [TEST_DIR] and run them as tests.
#[test]
fn run_test() {
    for test_file in fs::read_dir(TEST_DIR)
        .unwrap()
        .filter_map(|entry| entry.ok())
        .filter(|entry| {
            entry.file_type().unwrap().is_file()
                && entry.file_name().to_str().unwrap().ends_with(".c")
        })
    {
        let test_file = String::from(test_file.path().to_str().unwrap());
        let output_file = kcc::mktemp().unwrap();
        match kcc::main(["-o", output_file.to_str().unwrap(), &test_file]) {
            Ok(_) => {}
            Err(e) => {
                panic!("{}", e);
            }
        }

        let result = Command::new(&output_file).output().unwrap();
        println!("======= {} =======", &test_file);
        println!("{}", str::from_utf8(&result.stdout).unwrap());
        if let Some(0) = result.status.code() {
        } else if let Some(_) = result.status.code() {
            panic!("in {}, {}", &test_file, &output_file.to_str().unwrap());
        } else {
            panic!(
                "in {}, {}: {}",
                &test_file,
                &output_file.to_str().unwrap(),
                result.status
            );
        }
    }
}
