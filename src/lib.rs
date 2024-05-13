pub mod config;
pub mod tokenize;
pub mod util;

use config::Config;
use tokenize::Tokenizer;
use util::Result;

/// Executes the main logic.
/// Returns an [Error][util::Error] when occurs.
///
/// * args - command-line arguments
pub fn main<T>(args: impl IntoIterator<Item = T>) -> Result<()>
where
    T: Into<String>,
{
    let config = Config::new(args);
    let tokenizer = Tokenizer::new(config);
    tokenizer.tokenize()?;
    Ok(())
}
