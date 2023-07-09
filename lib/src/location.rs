#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Location {
    pub column: usize,
    pub line: usize
}

impl Location {
    pub fn new(column: usize, line: usize) -> Self {
        Self {
            column,
            line
        }
    }

    pub fn from_line(line: usize) -> Self {
        Self::new(0, line)
    }

    pub fn from_column(column: usize) -> Self {
        Self::new(column, 0)
    }
}