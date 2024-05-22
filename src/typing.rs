/// Represents a type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    /// int
    Int,
    /// Pointer.
    Ptr { base: Box<Type> },
    /// Void
    Void,
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
            TypeKind::Int => 8,
            TypeKind::Ptr { .. } => 8,
            TypeKind::Void => 0,
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

    pub fn void() -> Self {
        Self::new(TypeKind::Void)
    }

    pub fn is_integer(&self) -> bool {
        matches!(self.kind, TypeKind::Int)
    }

    pub fn base(&self) -> Option<Self> {
        match &self.kind {
            TypeKind::Ptr { base } => Some(*base.clone()),
            _ => None,
        }
    }
}
