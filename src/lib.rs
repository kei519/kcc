pub mod config;
pub mod tokenize;
pub mod util;

use config::Config;
use tokenize::Tokenizer;

/// Executes the main logic.
///
/// * args - command-line arguments
///
/// # Return
///
/// Exit code.
pub fn main(args: Vec<String>) -> u8 {
    let config = Config::new(args);
    let tokenizer = Tokenizer::new(config);
    match tokenizer.tokenize() {
        Ok(_) => {}
        Err(e) => {
            e.show();
            return 1;
        }
    }
    0
}
