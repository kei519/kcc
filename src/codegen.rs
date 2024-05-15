use crate::{
    parse::{Node, NodeKind},
    util::{into_err, Error, Result},
};

use std::{fs::File, io::Write as _, path::Path};

/// Generates assembly code for the given `nodes` and writes it to the file at `asm_path`.
pub fn codegen(nodes: Vec<Node>, asm_path: impl AsRef<Path>) -> Result<()> {
    let mut asm_file = match File::create(&asm_path) {
        Ok(file) => file,
        Err(e) => {
            return Err(Error::Any(format!(
                "{}: {}",
                e,
                asm_path.as_ref().display(),
            )))
        }
    };

    // Write the prolouge.
    writeln!(asm_file, ".global main").map_err(into_err)?;
    writeln!(asm_file, "main:").map_err(into_err)?;

    // Write the body.
    for node in nodes {
        gen(node, &mut asm_file)?;
    }

    // Write the epilouge.
    writeln!(asm_file, "  pop %rax").map_err(into_err)?;
    writeln!(asm_file, "  ret").map_err(into_err)?;

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
