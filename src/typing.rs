/// Represents a type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// int
    Int,
    /// Pointer.
    Ptr(Box<Type>),
}
