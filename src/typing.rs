/// Represents only non pointer types.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    /// int
    Int,
    /// Pointer.
    Ptr { base: Box<Type> },
    /// Array.
    Array { base: Box<Type>, len: usize },
}

/// Represents types.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    kind: TypeKind,
    /// sizeof([TypeKind])
    size: usize,
}

impl Type {
    pub fn new(kind: TypeKind) -> Self {
        let size = match kind {
            TypeKind::Int => 8,
            TypeKind::Ptr { .. } => 8,
            TypeKind::Array { .. } => 8,
        };

        Self { kind, size }
    }

    pub fn with_ptr(base: Type) -> Self {
        Self {
            kind: TypeKind::Ptr {
                base: Box::new(base),
            },
            size: 8,
        }
    }

    pub fn with_array(base: Type, len: usize) -> Self {
        let size = base.size * len;
        Self {
            kind: TypeKind::Array {
                base: Box::new(base),
                len,
            },
            size,
        }
    }
}
