#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct VMSettings {
    pub ignore_final_pop: bool
}

impl VMSettings {
    pub fn ignore_final_pop(mut self, value: bool) -> Self {
        self.ignore_final_pop = value;
        self
    }
}