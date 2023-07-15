use std::{slice::SliceIndex, ops::{Index, IndexMut}, fmt::Display, rc::Rc, borrow::Borrow};

use self::object::Object;

pub mod object;

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    #[default]
    Null,
    Void,
    Bool(bool),
    Int(i64),
    Double(f64),
    Object(Rc<Object>)
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => f.write_fmt(format_args!("null")),
            Self::Void => f.write_fmt(format_args!("void")),
            Self::Bool(b) => f.write_fmt(format_args!("{b}")),
            Self::Int(n) => f.write_fmt(format_args!("{n}")),
            Self::Double(n) => f.write_fmt(format_args!("{n}")),
            Self::Object(o) => f.write_fmt(format_args!("{}", *o))
        }
    }
}

impl Value {
    pub fn string(&self) -> String {
        match self {
            Self::Null => String::from(""),
            Self::Void => String::from(""),
            Self::Bool(b) => format!("{b}"),
            Self::Int(n) => format!("{n}"),
            Self::Double(n) => format!("{n}"),
            Self::Object(o) => o.string()
        }
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Self::Null)
    }

    pub fn is_void(&self) -> bool {
        matches!(self, Self::Void)
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool(_))
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Self::Int(_))
    }

    pub fn is_double(&self) -> bool {
        matches!(self, Self::Double(_))
    }

    pub fn is_object(&self) -> bool {
        matches!(self, Self::Object(_))
    }

    pub fn is_object_string(&self) -> bool {
        if let Self::Object(obj) = self {
            return obj.is_string();
        }

        false
    }

    pub fn unwrap_bool(&self) -> bool {
        if let Self::Bool(b) = self {
            *b
        }
        else {
            panic!("Not a bool!")
        }
    }

    pub fn unwrap_int(&self) -> i64 {
        if let Self::Int(i) = self {
            *i
        }
        else {
            panic!("Not an int!")
        }
    }

    pub fn unwrap_double(&self) -> f64 {
        if let Self::Double(d) = self {
            *d
        }
        else {
            panic!("Not a double!")
        }
    }

    pub fn unwrap_object(&self) -> Rc<Object> {
        if let Self::Object(o) = self {
            o.clone()
        }
        else {
            panic!("Not an object!")
        }
    }

    pub fn unwrap_object_string(&self) -> String {
        if let Self::Object(obj) = self {
            let ptr: &Object = Rc::borrow(obj);

            #[allow(irrefutable_let_patterns)]
            if let Object::String(s) = ptr {
                return s.clone();
            }
        }

        panic!("Not a string object!")
    }

    pub fn encode_as_bytes(&self) -> Vec<u8> {
        let mut bytes: Vec<u8> = Vec::new();

        match self {
            Value::Null => {
                //Push discriminant
                bytes.push(0_u8);
            },
            Value::Void => {
                //Push discriminant
                bytes.push(1_u8);
            },
            Value::Bool(b) => {
                //Push discriminant
                bytes.push(2_u8);

                //Push value
                if *b {
                    bytes.push(1_u8);
                }
                else {
                    bytes.push(0_u8);
                }
            },
            Value::Int(i) => {
                //Push discriminant
                bytes.push(3_u8);  

                //Push value
                bytes.extend(i.to_be_bytes());
            },
            Value::Double(d) => {
                //Push discriminant
                bytes.push(4_u8);    

                //Push value
                bytes.extend(d.to_be_bytes());
            },
            Value::Object(o) => {
                //Push discriminant
                bytes.push(5_u8);

                //Push Value
                bytes.extend(o.encode_as_bytes());
            },
        }

        bytes
    }

    pub fn decode_from_bytes(bytes: &[u8]) -> (Self, usize) {
        //Use first byte as discriminator
        let discriminator = bytes[0];

        match discriminator {
            //Null
            0 => {
                (Self::Null, 1)
            },
            //Void
            1 => {
                (Self::Void, 1)
            },
            //Bool
            2 => {
                //Read next byte to get boolean value
                let value = bytes[1];

                if value == 0 {
                    (Self::Bool(false), 2)
                }
                else {
                    (Self::Bool(true), 2)
                }
            },
            //Int
            3 => {
                //Read next 8 bytes to get i64 value
                let value = i64::from_be_bytes(bytes[1..9].try_into().unwrap_or_default());
                (Self::Int(value), 9)
            },
            //Double
            4 => {
                //Read next 8 bytes to get f64 value
                let value = f64::from_be_bytes(bytes[1..9].try_into().unwrap_or_default());
                (Self::Double(value), 9)
            },
            //Object
            5 => {
                //Read object from bytes
                let (obj, len) = Object::decode_from_bytes(&bytes[1..]);
                (Self::Object(Rc::new(obj)), len + 1)
            },
            _ => {
                panic!("Invalid value discriminator {discriminator}.")
            }
        }
    }
}

#[derive(Default, Debug, PartialEq, PartialOrd, Clone)]
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

    pub fn index_of(&self, value: &Value) -> Option<usize> {
        self.values.iter()
            .enumerate()
            .find(|(_, el)| &value == el)
            .map(|(ndx, _)| ndx)
    }

    pub fn encode_as_bytes(&self) -> Vec<u8> {
        self.values.iter()
            .flat_map(|v| v.encode_as_bytes())
            .collect()
    }

    pub fn decode_from_bytes(bytes: &[u8]) -> Self {
        let mut values: Vec<Value> = Vec::new();
        let mut offset: usize = 0;

        while offset < bytes.len() {
            let (val, len) = Value::decode_from_bytes(&bytes[offset..]);
            offset += len;
            values.push(val);
        }

        Self {
            values
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