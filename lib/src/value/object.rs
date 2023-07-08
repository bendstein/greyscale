use std::fmt::Display;

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Object {
    String(String)
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::String(s) => f.write_fmt(format_args!("{s}")),
        }
    }
}

impl Object {
    pub fn string(&self) -> String {
        match self {
            Object::String(s) => s.clone()
        }
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Self::String(_))
    }

    pub fn unwrap_string(&self) -> String {
        if let Self::String(s) = self {
            s.clone()
        }
        else {
            panic!("Not a string!")
        }
    }
}