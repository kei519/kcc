/// Represents only non pointer types.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    /// int
    Int,
    /// Pointer.
    Ptr { base: Box<Type> },
}

/// Represents types.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    kind: TypeKind,
}

impl Type {
    pub fn new(kind: TypeKind) -> Self {
        Self { kind }
    }

    pub fn with_ptr(base: Type) -> Self {
        Self {
            kind: TypeKind::Ptr {
                base: Box::new(base),
            },
        }
    }
}
