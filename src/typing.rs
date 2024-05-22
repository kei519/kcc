use crate::util::WORD_SIZE;

/// Represents a type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    /// int
    Int,
    /// Pointer.
    Ptr { base: Box<Type> },
    /// Void
    Void,
    /// Array.
    Array { base: Box<Type>, len: usize },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub kind: TypeKind,
    /// sizeof([TypeKind])
    pub size: usize,
}

impl Type {
    pub fn new(kind: TypeKind) -> Self {
        let size = match kind {
            TypeKind::Int => WORD_SIZE,
            TypeKind::Ptr { .. } => WORD_SIZE,
            TypeKind::Void => 0,
            TypeKind::Array { .. } => WORD_SIZE,
        };

        Self { kind, size }
    }

    pub fn with_ptr(base: Type) -> Self {
        Self {
            kind: TypeKind::Ptr {
                base: Box::new(base),
            },
            size: WORD_SIZE,
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

    pub fn void() -> Self {
        Self::new(TypeKind::Void)
    }

    pub fn is_integer(&self) -> bool {
        matches!(self.kind, TypeKind::Int)
    }

    pub fn base(&self) -> Option<Self> {
        match &self.kind {
            TypeKind::Ptr { base } => Some(*base.clone()),
            TypeKind::Array { base, .. } => Some(*base.clone()),
            _ => None,
        }
    }
}
