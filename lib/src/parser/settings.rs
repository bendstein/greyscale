#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct ParserSettings {
    pub allow_implicit_final_semicolon: bool
}

impl ParserSettings {
    pub fn allow_implicit_final_semicolon(mut self, value: bool) -> Self {
        self.allow_implicit_final_semicolon = value;
        self
    }
}