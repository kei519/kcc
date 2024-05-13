pub mod config;
pub mod util;

/// Executes the main logic.
///
/// * args - command-line arguments
///
/// # Return
///
/// Exit code.
pub fn main(_args: Vec<String>) -> u8 {
    println!("Hello Wordl!");
    0
}
