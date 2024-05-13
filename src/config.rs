use std::collections::VecDeque;

/// Stores configuration data in the program.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Config {
    /// Input string.
    pub input: String,
    /// Path where the compiled file is output.
    pub out_path: Option<String>,
}

impl Config {
    /// Constructs new [Config] with command-line arguments.
    ///
    /// If unrecognized args are passed, show the proper error message and usage.
    /// After that, exits the program.
    ///
    /// # Remarks
    /// This function cae exit process so call it before constructing others.
    pub fn new<Input>(args: impl IntoIterator<Item = Input>) -> Self
    where
        Input: Into<String>,
    {
        let mut input = None;
        let mut out_path = None;

        let mut args: VecDeque<String> = args.into_iter().map(|arg| arg.into()).collect();

        while !args.is_empty() {
            // This unwraping always succeeds because of condition.
            let arg = args.pop_front().unwrap();

            // output
            if arg.starts_with("-o") {
                // Checks the out_path is not still specified
                if out_path.is_some() {
                    eprintln!("output path is already specified.");
                    usage(1);
                }
                // -o<output> can be recognized.
                if arg.len() > 2 {
                    out_path = Some(arg[2..].into());
                } else {
                    // Checks output path follows
                    if args.is_empty() {
                        eprintln!("output path is required");
                        usage(1);
                    }
                    out_path = args.pop_front();
                }

                continue;
            }

            // help
            if arg.starts_with("-h") || arg.starts_with("--help") {
                usage(0);
            }

            // Non option arg is the input.
            if input.is_some() {
                eprintln!("only ONE input is accepted");
                usage(1);
            }
            input = Some(arg);
        }

        // Input must be set.
        let Some(input) = input else {
            eprintln!("input is required");
            usage(1);
        };

        Self { input, out_path }
    }
}

/// Shows usage and exits the process with exit code `code`.
fn usage(code: i32) -> ! {
    let message = "usage: kcc [-o <output>] <input>";
    // If error occurs, outputs message int stderr.
    if code != 0 {
        eprintln!("{}", message);
    } else {
        println!("{}", message);
    }
    if !cfg!(test) {
        std::process::exit(code);
    } else {
        panic!("{}", code);
    }
}

#[cfg(test)]
mod tests {
    use super::Config;

    #[test]
    #[should_panic(expected = "1")]
    fn test_no_input() {
        let _ = Config::new(Vec::<String>::new());
    }

    #[test]
    fn test_only_input() {
        let config = Config::new(["49"]);
        assert_eq!(config.input, "49");
        assert_eq!(config.out_path, None);
    }

    #[test]
    #[should_panic(expected = "1")]
    fn test_two_input() {
        let _ = Config::new(["1", "85"]);
    }

    #[test]
    #[should_panic(expected = "1")]
    fn test_no_input_with_output() {
        let _ = Config::new(["-o", "a.out"]);
    }

    #[test]
    #[should_panic(expected = "1")]
    fn test_no_output_arg() {
        let _ = Config::new(["hoge", "-o"]);
    }

    #[test]
    fn test_with_output_arg() {
        let config = Config::new(["hoge", "-o", "a.exe"]);
        assert_eq!(config.input, "hoge");
        assert_eq!(config.out_path.unwrap(), "a.exe");
    }

    #[test]
    #[should_panic(expected = "0")]
    fn test_with_help() {
        let _ = Config::new(["hoge", "-o", "a.exe", "--help"]);
    }
}
