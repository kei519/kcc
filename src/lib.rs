pub mod config;
pub mod parse;
pub mod tokenize;
pub mod util;

use config::Config;
use parse::Parser;
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
    let (tokens, config) = tokenizer.tokenize()?;
    let parser = Parser::new(tokens, config);
    parser.parse()?;
    Ok(())
}
