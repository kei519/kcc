use crate::{
    config::Config,
    tokenize::Token,
    util::{Annot, Error, Loc, Result},
};

pub struct Parser {
    tokens: Vec<Token>,
    /// Token position where the parser is reading.
    pos: usize,
    config: Config,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, config: Config) -> Self {
        Self {
            tokens,
            pos: 0,
            config,
        }
    }

    pub fn parse(self) -> Result<Vec<Node>> {
        Err(Error::from_compilation(
            "cannot compile anything.".into(),
            self.config.input,
            Loc::new(0, 1),
        ))
    }
}

pub enum NodeKind {}

pub type Node = Annot<NodeKind>;
