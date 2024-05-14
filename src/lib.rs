/// Executes the main logic.
///
/// * args - command-line arguments
///
/// # Return
///
/// Exit code.
pub fn main<T>(_args: impl IntoIterator<Item = T>) -> u8
where
    T: Into<String>,
{
    println!("Hello World!");
    0
}
