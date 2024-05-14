use std::collections::VecDeque;

#[cfg(test)]
mod test;

/// Represents the configuration of this program.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Config {
    pub input: String,
    pub out_path: Option<String>,
}

impl Config {
    /// Constructs a new [Config] with commadn-line args.
    /// If the program must exit, returns `Err(bool)`
    /// where the bool value indicates whether an error occured.
    pub fn new<T>(args: impl IntoIterator<Item = T>) -> Result<Self, bool>
    where
        T: Into<String>,
    {
        let mut args: VecDeque<String> = args.into_iter().map(|arg| arg.into()).collect();

        let mut input = None;
        let mut out_path = None;

        while !args.is_empty() {
            // This unwrapping always succeed due to the condition above.
            let arg = args.pop_front().unwrap();

            // Check "-o" option.
            if arg.starts_with("-o") {
                // Error if the output path is already specified.
                if out_path.is_some() {
                    eprintln!("specify output twice");
                    usage(true)?;
                }

                // "-o<output>" is also recognized.
                if !arg[2..].is_empty() {
                    out_path = Some(arg[2..].into());
                } else {
                    let path = match args.pop_front() {
                        Some(arg) => arg,
                        None => {
                            eprintln!(r#"output path is required after "-o" option"#);
                            usage(true)?;
                            unreachable!();
                        }
                    };
                    out_path = Some(path);
                }

                continue;
            }

            // Non-option arg is the input.
            // Error if input is already specified.
            if input.is_some() {
                eprintln!("only ONE input is recognized");
                usage(true)?;
            }
            input = Some(arg);
        }

        // Check if the input is specified.
        let Some(input) = input else {
            eprintln!("input is required");
            usage(true)?;
            unreachable!();
        };

        Ok(Self { input, out_path })
    }
}

/// Displays usage and returns `Err(bool)` indicating wheter an error occured.
/// For example, when "--help" option is passed, this is called even thougn no error occured.
///
/// * is_err - Indicates whether an error occured.
fn usage(is_err: bool) -> Result<(), bool> {
    let message = "usage: kcc [-o <output>] <input>";
    if is_err {
        eprintln!("{}", message);
    } else {
        println!("{}", message);
    }
    Err(is_err)
}
