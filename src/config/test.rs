use crate::config::Config;

#[test]
fn test_no_arg() {
    assert_eq!(Config::new(Vec::<String>::new()), Err(true));
}

#[test]
fn test_one_input() {
    let config = Config::new(["hoge"]).unwrap();
    assert_eq!(config.input, "hoge");
    assert!(config.out_path.is_none());
}

#[test]
fn test_two_input() {
    assert_eq!(Config::new(["hoge", "fuga"]), Err(true));
}

#[test]
fn test_no_output() {
    assert_eq!(Config::new(["", "-o"]), Err(true));
}

#[test]
fn test_no_input_with_output() {
    assert_eq!(Config::new(["-o", "a.out"]), Err(true));
}

#[test]
fn test_one_input_with_output() {
    let config = Config::new(["hoge", "-o", "a.exe"]).unwrap();
    assert_eq!(config.input, "hoge");
    assert_eq!(config.out_path.unwrap(), "a.exe");
}

#[test]
fn test_concat_output() {
    let config = Config::new(["hoge", "-oa.out"]).unwrap();
    assert_eq!(config.input, "hoge");
    assert_eq!(config.out_path.unwrap(), "a.out");
}

#[test]
fn test_two_output() {
    assert_eq!(Config::new(["-oa.exe", "-o", "hoge"]), Err(true));
}

#[test]
fn test_help() {
    assert_eq!(Config::new(["-oa.out", "hoge", "-h"]), Err(false));
    assert_eq!(Config::new(["--help", "-o", "a.exe"]), Err(false));
}
