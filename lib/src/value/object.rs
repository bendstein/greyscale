use std::fmt::Display;

#[derive(Debug, PartialEq, PartialOrd, Clone, Hash)]
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
        #[allow(irrefutable_let_patterns)]
        if let Self::String(s) = self {
            s.clone()
        }
        else {
            panic!("Not a string!")
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

                (Self::String(String::from_utf8(Vec::from(&bytes[offset..offset + slength])).unwrap()), 9 + slength)
            },
            _ => {
                panic!("Invalid object discriminator {discriminator}.")
            }
        }
    }
}