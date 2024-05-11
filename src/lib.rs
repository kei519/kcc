use std::{
    collections::VecDeque,
    ffi::OsStr,
    fs::File,
    hash::{Hash, Hasher},
    io::{Result, Write},
    path::{Path, PathBuf},
    process::Command,
    result::Result as StdResult,
    str,
    time::SystemTime,
};

/// Default output path of the program.
const OUTPUT_PATH: &str = "./a.out";

/// Executes the main logic.
///
/// * `args` - Command-line arguments.
///
/// # Returns
/// Exit code.
pub fn main(args: impl IntoIterator<Item = String>) -> u8 {
    let mut args: VecDeque<_> = args.into_iter().collect();
    if args.len() != 1 {
        eprintln!("one input is required");
        return 1;
    }
    // This unwrapping always success because of checking before.
    let input = args.pop_back().unwrap();

    let (n, _input) = match consume_digit(input.as_bytes()) {
        (Err(_), _) => {
            eprintln!("input must be a number.");
            return 1;
        }
        (Ok(n), input) => {
            if input.len() != 0 {
                eprintln!("input must be a number.");
                return 1;
            }
            (n, input)
        }
    };

    let asm_path = match mktemp() {
        Ok(path) => path,
        Err(e) => {
            eprintln!("fails creating a temp file: {}", e);
            return 1;
        }
    };

    let mut asm_file = File::create(&asm_path).unwrap();
    codegen(n, &mut asm_file).unwrap();

    assemble(&asm_path, OUTPUT_PATH).unwrap();

    0
}

fn codegen<W>(n: usize, writer: &mut W) -> Result<()>
where
    W: Write,
{
    writeln!(writer, ".global main")?;
    writeln!(writer, "main:")?;
    writeln!(writer, "  mov ${}, %rax", n)?;
    writeln!(writer, "  ret")?;
    Ok(())
}

/// Consumes input as digit from head and returns read digit if the head is digit
/// and remains of input.
/// Otherwise, returns error and the input.
///
/// * input - Decoded input string.
fn consume_digit(mut input: &[u8]) -> (StdResult<usize, ()>, &[u8]) {
    if input.len() == 0 || !input[0].is_ascii_digit() {
        return (Err(()), input);
    }

    let mut ret = 0;
    while input.len() != 0 && input[0].is_ascii_digit() {
        ret *= 10;
        ret += (input[0] - b'0') as usize;
        input = &input[1..];
    }
    (Ok(ret), input)
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

/// Assembles `asm_path` into `output_path`.
/// Returns an error if any errors occurs.
///
/// * `asm_path` - Path to the file to assemble.
/// * `output_path` - Path to the assembled file.
fn assemble(asm_path: impl AsRef<Path>, output_path: impl AsRef<Path>) -> Result<()> {
    let mut process = Command::new("gcc")
        .args([
            OsStr::new("-xassembler"),
            OsStr::new("-o"),
            output_path.as_ref().as_ref(),
            asm_path.as_ref().as_ref(),
        ])
        .spawn()?;

    let exit_status = process.wait()?;
    if !exit_status.success() {
        // We should not reache here
        // because assembly that this program outputs
        // and the way of calling assembler should be correct.
        unreachable!();
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use std::{
        fs::{self, File},
        hash::{DefaultHasher, Hash, Hasher},
        io::Write,
        path::Path,
        process::Command,
        time::SystemTime,
    };

    use crate::OUTPUT_PATH;

    /// Directory for files used during testing.
    const TEST_DIR: &str = "tmp";

    #[test]
    fn mktemp_test() {
        let path = crate::mktemp().unwrap();
        assert!(File::open(path).is_ok());
    }

    #[test]
    fn asm_test() {
        let asm_path = crate::mktemp().unwrap();
        check_test_dir();
        let output_path = Path::new(TEST_DIR).join("asm_test");

        // Determins a random exit code.
        let mut hasher = DefaultHasher::new();
        SystemTime::now().hash(&mut hasher);
        let exit_code = hasher.finish() as u8;

        let mut asm_file = File::create(&asm_path).unwrap();
        writeln!(
            asm_file,
            r".global main
main:
    mov ${}, %rax
    ret",
            exit_code
        )
        .unwrap();

        crate::assemble(asm_path, &output_path).unwrap();

        let mut process = Command::new(output_path).spawn().unwrap();
        let exit_status = process.wait().unwrap();

        assert_eq!(exit_status.code().unwrap(), exit_code as i32);
    }

    /// Checks if [TEST_DIR] is a valid directory.
    fn check_test_dir() {
        let test_dir = Path::new(TEST_DIR);
        if test_dir.is_dir() {
            return;
        } else if test_dir.exists() {
            fs::remove_file(test_dir).unwrap();
        }
        fs::create_dir_all(test_dir).unwrap();
    }

    #[test]
    fn imed_value_test() {
        let mut haser = DefaultHasher::new();
        SystemTime::now().hash(&mut haser);
        let exit_code = haser.finish() as u8;

        // Checks if compiling successes.
        assert_eq!(crate::main(vec![format!("{}", exit_code)]), 0);

        let mut process = Command::new(OUTPUT_PATH).spawn().unwrap();
        let exit_status = process.wait().unwrap();

        assert_eq!(exit_status.code().unwrap(), exit_code as i32);
    }
}
