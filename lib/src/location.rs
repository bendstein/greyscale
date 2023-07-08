#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Location {
    pub column: usize,
    pub line: usize
}