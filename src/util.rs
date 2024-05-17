use std::{
    cmp, env,
    fmt::Display,
    fs::File,
    hash::{DefaultHasher, Hash as _, Hasher as _},
    io,
    ops::{Add, AddAssign},
    path::{Path, PathBuf},
    process,
    time::SystemTime,
};

use crate::PROG_NAME;

/// Represents a location [`start`, `end`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Loc {
    start: usize,
    end: usize,
}

impl Loc {
    pub fn at(pos: usize) -> Self {
        Self {
            start: pos,
            end: pos + 1,
        }
    }

    pub fn range(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

impl Add for Loc {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            start: cmp::min(self.start, rhs.start),
            end: cmp::max(self.end, rhs.end),
        }
    }
}

impl AddAssign for Loc {
    fn add_assign(&mut self, rhs: Self) {
        self.start = cmp::min(self.start, rhs.start);
        self.end = cmp::max(self.end, rhs.end);
    }
}

/// Represents data annotated with a location.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Annot<T> {
    pub data: T,
    pub loc: Loc,
}

pub type Result<T> = std::result::Result<T, Error>;

/// Represents a error occured in this program.
#[derive(Debug)]
pub enum Error {
    /// Represents a error with the configuration.
    ConfigError,
    /// Represents a error occured during compilation.
    CompileError {
        message: String,
        input: &'static str,
        loc: Loc,
    },
    /// Represents a error occured during I/O.
    IoError(io::Error),
    /// Represents a error other than the above with a message.
    Any(String),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ConfigError => Ok(()),
            Self::CompileError {
                message,
                input,
                loc,
            } => {
                f.write_str(*input)?;
                f.write_str("\n")?;
                write!(f, "{:num$}^ {}", "", message, num = loc.start)
            }
            Self::IoError(e) => e.fmt(f),
            Self::Any(s) => f.write_str(s),
        }
    }
}

impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        Self::IoError(value)
    }
}

/// Assemble the given assemly code located at `asm_path` into a executable located at `out_path`.
pub fn assemble(asm_path: impl AsRef<Path>, out_path: impl AsRef<Path>) -> Result<()> {
    let output = process::Command::new("gcc")
        .arg("-o")
        .arg(out_path.as_ref())
        .arg("-xassembler")
        .arg(asm_path.as_ref())
        .output()?;

    if !output.status.success() {
        return Err(Error::Any(format!(
            "gcc failed: {}",
            String::from_utf8_lossy(&output.stderr)
        )));
    }

    Ok(())
}

/// Generates a temporary file and returns its path.
pub fn mktemp() -> io::Result<PathBuf> {
    /// The characters used in the file name.
    const CHARS: &[u8; 62] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
    /// The length of the random part of the file name.
    const RAND_LEN: usize = 6;

    // Generate a file name.
    let rand = rand();
    let suffix: String = (0..RAND_LEN)
        .map(|i| CHARS[(rand >> (i * 8)) as usize % CHARS.len()] as char)
        .collect();
    let file_name = format!("{}-{}", PROG_NAME, suffix);

    // Create a temporary file.
    let path = env::temp_dir().join(file_name);
    File::create_new(&path)?;
    Ok(path)
}

/// Generates a [u64] random number using the current time and process ID.
pub fn rand() -> u64 {
    let mut hasher = DefaultHasher::new();
    SystemTime::now().hash(&mut hasher);
    process::id().hash(&mut hasher);

    let maybe_exe_path = env::args().find(|_| true);
    maybe_exe_path.hash(&mut hasher);

    hasher.finish()
}
