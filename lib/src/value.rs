use std::{slice::SliceIndex, ops::{Index, IndexMut}, fmt::Display};

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Value {
    Double(f64)
}

impl Default for Value {
   fn default() -> Self {
       Value::Double(0_f64)
   } 
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Double(n) => f.write_fmt(format_args!("{n}"))
        }
    }
}

#[derive(Default, Debug, PartialEq, PartialOrd)]
pub struct Values {
    values: Vec<Value>,
}

impl Values {
    pub fn write(&mut self, value: Value) {
        self.values.push(value);
    }

    pub fn count(&self) -> usize {
        self.values.len()
    }

    pub fn at(&self, index: usize) -> &Value {
        &self.values[index]
    }

    pub fn try_at(&self, index: usize) -> Option<&Value> {
        if index >= self.count() {
            None
        }
        else {
            Some(self.at(index))
        }
    }
}

impl<Idx> Index<Idx> for Values
    where Idx : SliceIndex<[Value], Output = Value>
{
    type Output = Value;

    fn index(&self, index: Idx) -> &Self::Output {
        &self.values[index]
    }
}

impl<Idx> IndexMut<Idx> for Values
    where Idx : SliceIndex<[Value], Output = Value>
{
    fn index_mut(&mut self, index: Idx) -> &mut Self::Output {
        &mut self.values[index]
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct ValuesIter<'a> {
    values: &'a Values,
    n: usize
}

impl<'a> From<&'a Values> for ValuesIter<'a> {
    fn from(value: &'a Values) -> Self {
        Self {
            values: value,
            n: 0
        }
    }
}

impl<'a> Iterator for ValuesIter<'a> {
    type Item = &'a Value;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.values.try_at(self.n)?;
        self.n += 1_usize;
        Some(result)
    }
}