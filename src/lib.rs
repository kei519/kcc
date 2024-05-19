mod codegen;
mod config;
mod parse;
mod tokenize;
mod typing;
mod util;

use codegen::Generator;
use config::Config;
use tokenize::Tokenizer;
use util::{assemble, Error, Result};

pub use util::{mktemp, rand};

/// The name of this program.
const PROG_NAME: &str = "kcc";

/// The default output file name.
const DEFAULT_OUTPUT: &str = "a.out";

/// Executes the main logic.
///
/// * args - command-line arguments
pub fn main<T>(args: impl IntoIterator<Item = T>) -> Result<()>
where
    T: Into<String>,
{
    let config = match Config::new(args) {
        Ok(config) => config,
        Err(is_err) => {
            return if is_err {
                Err(Error::ConfigError)
            } else {
                Ok(())
            }
        }
    };

    let input = config.input.leak();
    let tokenizer = Tokenizer::new(input);
    let tokens = tokenizer.tokenize()?;

    let parser = parse::Parser::new(input, tokens);
    let top_node = parser.parse()?;

    // Generate assembly.
    let asm_path = mktemp()?;
    let mut gen = Generator::from_path(input, &asm_path)?;
    gen.codegen(top_node)?;

    // Assemble the assembly.
    assemble(
        asm_path,
        config.obj_paths,
        config.out_path.unwrap_or(DEFAULT_OUTPUT.into()),
    )?;

    Ok(())
}
