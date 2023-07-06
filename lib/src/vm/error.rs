#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum GreyscaleError {
    CompileErr(String),
    RuntimeErr(String)
}