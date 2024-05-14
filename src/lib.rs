mod config;

use config::Config;

/// Executes the main logic.
///
/// * args - command-line arguments
pub fn main<T>(args: impl IntoIterator<Item = T>) -> Result<(), ()>
where
    T: Into<String>,
{
    let _config = match Config::new(args) {
        Ok(config) => config,
        Err(is_err) => return if is_err { Err(()) } else { Ok(()) },
    };

    Ok(())
}
