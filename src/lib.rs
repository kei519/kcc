/// Executes the main logic.
///
/// * args - command-line arguments
pub fn main<T>(_args: impl IntoIterator<Item = T>) -> Result<(), ()>
where
    T: Into<String>,
{
    println!("Hello World!");
    Ok(())
}
