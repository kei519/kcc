use crate::{
    parse::{Node, NodeKind},
    util::{Error, Result},
};

use std::{
    fs::File,
    io::{self, Write as _},
    path::Path,
};

/// Generates assembly code for the given `nodes` and writes it to the file at `out_path`.
pub fn codegen(nodes: Vec<Node>, out_path: impl AsRef<Path>) -> Result<()> {
    let mut file = match File::create(&out_path) {
        Ok(file) => file,
        Err(e) => {
            return Err(Error::Any(format!(
                "{}: {}",
                e,
                out_path.as_ref().display()
            )))
        }
    };

    // Write the prolouge.
    writeln!(file, ".global main").map_err(into_err)?;
    writeln!(file, "main:").map_err(into_err)?;

    // Write the body.
    for node in nodes {
        gen(node, &mut file)?;
    }

    // Write the epilouge.
    writeln!(file, "  pop %rax").map_err(into_err)?;
    writeln!(file, "  ret").map_err(into_err)?;

    Ok(())
}

/// Generates code for the given `node` and writes it to `file`.
fn gen(node: Node, file: &mut File) -> Result<()> {
    match node.data {
        NodeKind::Num(num) => {
            writeln!(file, "  mov ${}, %rax", num).map_err(into_err)?;
            writeln!(file, "  push %rax").map_err(into_err)?;
        }
    }
    Ok(())
}

/// Converts [io::Error] into [Error].
fn into_err(e: io::Error) -> Error {
    Error::IoError(e)
}
