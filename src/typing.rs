use std::rc::Rc;

use crate::util::{align_to, WORD_SIZE};

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
    /// struct.
    Struct { members: Vec<Rc<Member>> },
}

/// Struct member.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Member {
    pub name: &'static str,
    pub ty: Rc<Type>,
    pub offset: usize,
}

impl Member {
    pub fn new(name: &'static str, ty: Rc<Type>) -> Self {
        Self {
            name,
            ty,
            offset: 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub kind: TypeKind,
    /// sizeof([TypeKind])
    pub size: usize,
    /// alignment
    pub align: usize,
}

impl Type {
    pub fn char_type() -> Rc<Self> {
        Rc::new(Self {
            kind: TypeKind::Char,
            size: 1,
            align: 1,
        })
    }

    pub fn int_type() -> Rc<Self> {
        Rc::new(Self {
            kind: TypeKind::Int,
            size: 4,
            align: 4,
        })
    }

    pub fn with_ptr(base: Rc<Type>) -> Rc<Self> {
        Rc::new(Self {
            kind: TypeKind::Ptr { base },
            size: WORD_SIZE,
            align: WORD_SIZE,
        })
    }

    pub fn with_array(base: Rc<Type>, len: usize) -> Rc<Self> {
        let size = base.size * len;
        let align = base.align;

        Rc::new(Self {
            kind: TypeKind::Array { base, len },
            size,
            align,
        })
    }

    pub fn with_str(len: usize) -> Rc<Self> {
        Rc::new(Self {
            kind: TypeKind::Array {
                base: Type::char_type(),
                len: len,
            },
            size: len,
            align: 1,
        })
    }

    pub fn void() -> Rc<Self> {
        Rc::new(Self {
            kind: TypeKind::Void,
            size: 0,
            align: 0,
        })
    }

    pub fn with_struct(members: Vec<Rc<Member>>) -> Rc<Self> {
        let align = members.iter().map(|mem| mem.ty.align).max().unwrap_or(0);
        let size = members.iter().fold(0, |acc, mem| acc + mem.ty.size);
        Rc::new(Self {
            kind: TypeKind::Struct { members },
            size: align_to(size, align),
            align,
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

    pub fn is_struct(&self) -> bool {
        matches!(self.kind, TypeKind::Struct { .. })
    }
}
