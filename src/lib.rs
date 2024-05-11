use std::{
    fs::File,
    hash::{Hash, Hasher},
    io::Result,
    path::PathBuf,
    str,
    time::SystemTime,
};

/// Executes the main logic.
///
/// * `_args` - Command-line arguments.
///
/// # Returns
/// Exit code.
pub fn main(_args: Vec<String>) -> u8 {
    0
}

/// Creates a temp file in the directory determined by [temp_dir()][std::env::temp_dir()].
/// The file follows the pattern `kcc-XXXXXX`.
/// Returns an error if any errors occurs.
///
/// # Returns
/// The path to the file as [PathBuf].
fn mktemp() -> Result<PathBuf> {
    const PREFIX: &str = "kcc-";
    const TABLE: &[u8; 62] = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

    let mut hasher = std::hash::DefaultHasher::new();
    SystemTime::now().hash(&mut hasher);
    let hash = hasher.finish();

    let mut rand: Vec<_> = b"XXXXXX".into();
    for i in 0..rand.len() {
        let index = ((hash >> i) as u8) % TABLE.len() as u8;
        rand[i] = TABLE[index as usize];
    }

    let file_name = format!("{}{}", PREFIX, str::from_utf8(&rand).unwrap());
    let path = std::env::temp_dir().join(file_name);
    File::create_new(&path)?;

    return Ok(path);
}

#[cfg(test)]
mod test {
    use std::fs::File;

    #[test]
    fn mktemp_test() {
        let path = crate::mktemp().unwrap();
        assert!(File::open(path).is_ok());
    }
}
