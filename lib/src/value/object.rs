use std::fmt::Display;

use crate::chunk::Chunk;

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Object {
    String(String),
    Function(Function)
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.string()))
    }
}

impl Object {
    pub fn string(&self) -> String {
        match self {
            Object::String(s) => s.clone(),
            Object::Function(f) => match f.func_type {
                FunctionType::TopLevel => "<script>".to_string(),
                _ => format!("<fn{} {}>", if f.arity == 0 {
                    "".to_string()
                } else {
                    format!("_{}", f.arity)
                }, f.name())
            }
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

    pub fn is_function(&self) -> bool {
        matches!(self, Self::Function(_))
    }

    pub fn unwrap_function(&self) -> Function {
        if let Self::Function(f) = self {
            f.clone()
        }
        else {
            panic!("Not a function!")
        }
    }

    pub fn encode_as_bytes(&self) -> Vec<u8> {
        let mut bytes: Vec<u8> = Vec::new();

        match self {
            Object::String(s) => {
                //Push discriminant
                bytes.push(0_u8);

                //Push string length
                let sbytes = s.clone().into_bytes();
                bytes.extend((sbytes.len() as u64).to_be_bytes());

                //Push value
                bytes.extend(sbytes);
            },
            Object::Function(f) => {
                //Don't write top-level function
                if !f.func_type.is_top_level() {
                    //Push discriminant
                    bytes.push(1_u8);

                    //Push length of name
                    let nbytes = f.name().into_bytes();
                    bytes.extend((nbytes.len() as u64).to_be_bytes());

                    //Push name
                    bytes.extend(nbytes);

                    //Push number of arguments
                    bytes.push(f.arity);

                    //Encode chunk
                    let chunk_bytes = f.chunk.encode_as_bytes();

                    //Push chunk length
                    bytes.extend((chunk_bytes.len() as u64).to_be_bytes());

                    //Push chunk
                    bytes.extend(chunk_bytes);
                }
            }
        }

        bytes
    }

    pub fn decode_from_bytes(bytes: &[u8]) -> (Self, usize) {
        //Use first byte as discriminator
        let discriminator = bytes[0];

        let mut offset = 1;

        match discriminator {
            //String
            0 => {
                //Read next 8 bytes as string length
                let slice = &bytes[offset..offset + 8];
                let slength = u64::from_be_bytes(slice.try_into().unwrap_or_default()) as usize;     

                offset += 8;

                (Self::String(String::from_utf8(Vec::from(&bytes[offset..offset + slength])).unwrap()), offset + slength)
            },
            //Function
            1 => {
                //Read next 8 bytes as name length
                let slice = &bytes[offset..offset + 8];
                let nlength = u64::from_be_bytes(slice.try_into().unwrap_or_default()) as usize;  
                offset += 8;

                //Read name
                let name = String::from_utf8(Vec::from(&bytes[offset..offset + nlength])).unwrap();

                offset += nlength;

                //Read next byte as function arity
                let arity = &bytes[offset];
                offset += 1;

                //Read next 8 bytes as chunk length
                let slice = &bytes[offset..offset + 8];
                let clength = u64::from_be_bytes(slice.try_into().unwrap_or_default()) as usize;  
                offset += 8;

                //Read chunk
                let chunk = Chunk::decode_from_bytes(&bytes[offset..offset + clength]);

                offset += clength;

                (Self::Function(Function {
                    arity: *arity,
                    chunk,
                    func_type: FunctionType::Function(name)
                }), offset)
            }
            _ => {
                panic!("Invalid object discriminator {discriminator}.")
            }
        }
    }
}

#[derive(Debug, Default, PartialEq, PartialOrd, Clone)]
pub enum FunctionType {
    #[default]
    TopLevel,
    Function(String)
}

impl FunctionType {
    pub fn is_top_level(&self) -> bool {
        matches!(self, Self::TopLevel)
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Self::Function(_))
    }
}

#[derive(Debug, Default, PartialEq, PartialOrd, Clone)]
pub struct Function {
    pub arity: u8,
    pub chunk: Chunk,
    pub func_type: FunctionType
}

impl Function {
    pub fn name(&self) -> String {
        match &self.func_type {
            FunctionType::TopLevel => "<script>".to_string(),
            FunctionType::Function(name) => name.clone(),
        }
    }
}