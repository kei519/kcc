use std::{
    cmp,
    ops::{Add, AddAssign},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// Represents a kind [T] and the location of it.
pub struct Annot<T> {
    /// Kind.
    pub kind: T,
    /// Location.
    pub loc: Loc,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// Represents a location [start, end).
pub struct Loc {
    pub start: usize,
    pub end: usize,
}

impl Loc {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

impl Add for Loc {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            start: cmp::min(self.start, rhs.start),
            end: cmp::max(self.end, rhs.end),
        }
    }
}

impl AddAssign for Loc {
    fn add_assign(&mut self, rhs: Self) {
        self.start = cmp::min(self.start, rhs.start);
        self.end = cmp::max(self.end, rhs.end);
    }
}

#[cfg(test)]
mod tests {
    use crate::util::Loc;

    #[test]
    fn test_loc_add() {
        let l1 = Loc { start: 4, end: 18 };
        let l2 = Loc { start: 2, end: 7 };
        let loc = l1 + l2;
        assert_eq!(loc, Loc { start: 2, end: 18 });
    }

    #[test]
    fn test_loc_add_assign() {
        let mut loc = Loc {
            start: 83,
            end: 100,
        };
        loc += Loc { start: 12, end: 44 };
        assert_eq!(
            loc,
            Loc {
                start: 12,
                end: 100
            }
        );
    }
}
