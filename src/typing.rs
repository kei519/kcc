use crate::util::WORD_SIZE;

/// Represents a type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    /// char
    Char,
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
    pub fn char_type() -> Self {
        Self {
            kind: TypeKind::Char,
            size: 1,
        }
    }

    pub fn int_type() -> Self {
        Self {
            kind: TypeKind::Int,
            size: 8,
        }
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

    pub fn with_str(len: usize) -> Self {
        Self {
            kind: TypeKind::Array {
                base: Box::new(Type::char_type()),
                len: len,
            },
            size: len,
        }
    }

    pub fn void() -> Self {
        Self {
            kind: TypeKind::Void,
            size: 0,
        }
    }

    pub fn is_integer(&self) -> bool {
        matches!(self.kind, TypeKind::Char | TypeKind::Int)
    }

    pub fn base(&self) -> Option<Self> {
        match &self.kind {
            TypeKind::Ptr { base } => Some(*base.clone()),
            TypeKind::Array { base, .. } => Some(*base.clone()),
            _ => None,
        }
    }

    pub fn is_array(&self) -> bool {
        matches!(self.kind, TypeKind::Array { .. })
    }
}
