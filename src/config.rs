use std::collections::VecDeque;

/// Represents the configuration.
pub struct Config {
    /// Input that will be compiled.
    input: String,
    /// Path where compiled file is output.
    output_path: Option<String>,
}

impl Config {
    /// Construct new configuration from command-line arguments.
    /// If received invalid arguments, shows the error message and usage,
    /// and returns an error.
    ///
    /// * `args` - Command-line arguments.
    pub fn new<T>(args: impl IntoIterator<Item = T>) -> Result<Self, ()>
    where
        T: Into<String>,
    {
        let mut args: VecDeque<_> = args.into_iter().map(|arg| arg.into()).collect();

        let mut input = None;
        let mut output_path = None;

        // Checks all arguments.
        while !args.is_empty() {
            // This unwraping always succeeds because of the condition.
            let opt = args.pop_front().unwrap();

            // Checks whether the output file path is specified.
            if opt.starts_with("-o") {
                // When already specified, it is an error.
                if output_path.is_some() {
                    eprintln!("output file is already specified");
                    usage(true);
                    return Err(());
                }

                // -o<output> is also correct style.
                if opt.len() > 2 {
                    output_path = Some(String::from(&opt[2..]));
                } else {
                    if args.is_empty() {
                        eprintln!("output file is not specified");
                        usage(true);
                        return Err(());
                    }
                    // This unwrapping always succeeds.
                    output_path = Some(args.pop_front().unwrap());
                }

                continue;
            }

            // Not option means that is the input.
            // If already provided, it is an error.
            if input.is_none() {
                input = Some(opt);
            } else {
                eprintln!("too many inputs");
                usage(true);
                return Err(());
            }
        }

        // Input must be provided after reading all arguments.
        let input = match input {
            Some(input) => input,
            None => {
                eprintln!("input is requreid");
                usage(true);
                return Err(());
            }
        };

        Ok(Self { input, output_path })
    }

    /// Returns the refference of the input.
    pub fn input(&self) -> &String {
        &self.input
    }

    /// Returns the output file path if specified.
    pub fn output_path(&self) -> Option<&String> {
        self.output_path.as_ref()
    }
}

/// Shows usage.
///
/// * `in_stderr` - If true, shows the message in stderr, otherwise in stdout.
fn usage(in_stderr: bool) {
    let message = format!(r#"Usage: [-o <output>] <input>"#);
    if in_stderr {
        eprintln!("{}", message);
    } else {
        println!("{}", message);
    }
}

#[cfg(test)]
mod test {
    use crate::config::Config;

    #[test]
    fn test_no_input() {
        assert!(Config::new(Vec::<String>::new()).is_err());
    }

    #[test]
    fn test_input() {
        let input = "input something";
        let config = Config::new([String::from(input)]).unwrap();
        assert_eq!(config.input, input);
        assert_eq!(config.output_path, None);
    }

    #[test]
    fn test_two_input() {
        let inputs = ["hoge", "fuga"];
        assert!(Config::new(inputs).is_err());
    }

    #[test]
    fn test_not_specified_output() {
        let inputs = ["hoge", "-o"];
        assert!(Config::new(inputs).is_err());
    }

    #[test]
    fn test_concatenated_output() {
        let inputs = ["-oa.exe", "12"];
        let config = Config::new(inputs).unwrap();

        assert_eq!(config.input, "12");
        assert_eq!(config.output_path, Some("a.exe".into()));
    }

    #[test]
    fn test_output() {
        let inputs = ["-o", "hoge", "58"];
        let config = Config::new(inputs).unwrap();

        assert_eq!(config.input, "58");
        assert_eq!(config.output_path, Some("hoge".into()));
    }

    #[test]
    fn test_two_outputs() {
        let inputs = ["-ohoge", "-o", "-fuga", "111"];
        assert!(Config::new(inputs).is_err());
    }
}
