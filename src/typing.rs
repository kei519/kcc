use std::rc::Rc;

use crate::util::WORD_SIZE;

/// Represents a type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    /// char
    Char,
    /// int
    Int,
    /// Pointer.
    Ptr { base: Rc<Type> },
    /// Void
    Void,
    /// Array.
    Array { base: Rc<Type>, len: usize },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub kind: TypeKind,
    /// sizeof([TypeKind])
    pub size: usize,
}

impl Type {
    pub fn char_type() -> Rc<Self> {
        Rc::new(Self {
            kind: TypeKind::Char,
            size: 1,
        })
    }

    pub fn int_type() -> Rc<Self> {
        Rc::new(Self {
            kind: TypeKind::Int,
            size: 8,
        })
    }

    pub fn with_ptr(base: Rc<Type>) -> Rc<Self> {
        Rc::new(Self {
            kind: TypeKind::Ptr { base },
            size: WORD_SIZE,
        })
    }

    pub fn with_array(base: Rc<Type>, len: usize) -> Rc<Self> {
        let size = base.size * len;

        Rc::new(Self {
            kind: TypeKind::Array { base, len },
            size,
        })
    }

    pub fn with_str(len: usize) -> Rc<Self> {
        Rc::new(Self {
            kind: TypeKind::Array {
                base: Type::char_type(),
                len: len,
            },
            size: len,
        })
    }

    pub fn void() -> Rc<Self> {
        Rc::new(Self {
            kind: TypeKind::Void,
            size: 0,
        })
    }

    pub fn is_integer(&self) -> bool {
        matches!(self.kind, TypeKind::Char | TypeKind::Int)
    }

    pub fn base(&self) -> Option<Rc<Self>> {
        match &self.kind {
            TypeKind::Ptr { base } => Some(base.clone()),
            TypeKind::Array { base, .. } => Some(base.clone()),
            _ => None,
        }
    }

    pub fn is_array(&self) -> bool {
        matches!(self.kind, TypeKind::Array { .. })
    }
}
