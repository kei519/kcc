mod config;
mod tokenize;
mod util;

use config::Config;
use tokenize::Tokenizer;
use util::{Error, Result};

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
    tokenizer.tokenize()?;

    Ok(())
}
