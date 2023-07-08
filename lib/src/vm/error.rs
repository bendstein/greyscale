use crate::location::Location;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum GreyscaleError {
    CompileErr(String, Location),
    RuntimeErr(String, Location),
    AggregateErr(Vec<GreyscaleError>)
}