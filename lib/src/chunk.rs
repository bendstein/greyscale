pub mod disassemble;

use std::{slice::SliceIndex, ops::{Index, IndexMut}, fmt::Display};
use crate::{value::{Values, Value, ValuesIter}, metadata::Metadata};

#[derive(Default, Debug, PartialEq, PartialOrd, Clone)]
pub struct Chunk {
    pub name: Option<String>,
    code: Vec<u8>,
    constants: Values,
    pub metadata: Metadata
}

impl Chunk {
    pub fn append(&mut self, chunk: &Chunk) {
        for c in &chunk.code {
            self.code.push(*c);
        }
        for c in ValuesIter::from(&chunk.constants) {
            self.constants.write(c.clone());
        }
        for m in chunk.metadata.newlines() {
            self.metadata.new_line(m.0, m.1);
        }
    }

    pub fn write(&mut self, code: u8, line: usize) {
        self.metadata.new_line(self.count(), line);
        self.code.push(code);
    }

    pub fn write_u16(&mut self, code: u16, line: usize) {
        let a1 = ((code >> 8) & 0xFF) as u8;
        let a2 = (code & 0xFF) as u8;
        self.write(a1, line);
        self.write(a2, line);
    }

    pub fn patch(&mut self, offset: usize, code: u8) {
        self.code[offset] = code;
    }

    pub fn patch_u16(&mut self, offset: usize, code: u16) {
        let a1 = ((code >> 8) & 0xFF) as u8;
        let a2 = (code & 0xFF) as u8;
        self.patch(offset, a1);
        self.patch(offset + 1, a2);
    }

    pub fn add_const(&mut self, constant: Value) -> usize {
        self.constants.write(constant);
        self.constants.count() - 1
    }

    pub fn get_const(&self, n: usize) -> &Value {
        &self.constants[n]
    }

    pub fn try_get_const(&self, n: usize) -> Option<&Value> {
        if n >= self.constants.count() {
            None
        }
        else {
            Some(self.get_const(n))
        }
    }

    pub fn count_consts(&self) -> usize {
        self.constants.count()
    }

    pub fn count(&self) -> usize {
        self.code.len()
    }

    pub fn at(&self, index: usize) -> u8 {
        self.code[index]
    }

    pub fn try_at(&self, index: usize) -> Option<u8> {
        if index >= self.count() {
            None
        }
        else {
            Some(self.at(index))
        }
    }

    pub fn iter(&self) -> ChunkIter {
        ChunkIter::from(self)
    }

    pub fn with_name(mut self, name: Option<String>) -> Self {
        self.name = name;
        self
    }

    pub fn encode_as_bytes(&self) -> Vec<u8> {
        let mut bytes: Vec<u8> = Vec::new();

        //Write signature
        let signature = Vec::from(crate::constants::BIN_SIGNATURE.as_bytes());
        bytes.extend(signature);

        //Encode metadata
        let metadata_bytes = self.metadata.encode_as_bytes();

        //Encode constants
        let constants_bytes = self.constants.encode_as_bytes();

        //Write metadata length, followed by metadata
        bytes.extend((metadata_bytes.len() as u64).to_be_bytes());
        bytes.extend(metadata_bytes);

        //Write constants length, followed by constants
        bytes.extend((constants_bytes.len() as u64).to_be_bytes());
        bytes.extend(constants_bytes);

        //Write code length, followed by code
        bytes.extend((self.code.len() as u64).to_be_bytes());
        bytes.extend(self.code.clone());

        bytes
    }

    pub fn decode_from_bytes(bytes: &[u8]) -> Result<Self, String> {
        let mut offset = 0;

        let expected_signature = *crate::constants::BIN_SIGNATURE;

        if bytes.is_empty() || bytes.len() < offset + expected_signature.len() {
            return Err(String::from("Unexpected EOF"));
        }

        if !Self::verify_signature(bytes) {
            return Err(String::from("Invalid signature"));
        }

        offset += expected_signature.len();

        if bytes.len() < offset + 8 {
            return Err(String::from("Unexpected EOF"));
        }

        //Get metadata length
        let metadata_len = u64::from_be_bytes(bytes[offset..offset + 8].try_into().unwrap_or_default()) as usize;

        offset += 8;

        if bytes.len() < offset + metadata_len {
            return Err(String::from("Unexpected EOF"));
        }

        //Get metadata
        let metadata = Metadata::decode_from_bytes(&bytes[offset..metadata_len + offset])?;

        offset += metadata_len;

        if bytes.len() < offset + 8 {
            return Err(String::from("Unexpected EOF"));
        }

        //Get constants length
        let constants_len = u64::from_be_bytes(bytes[offset..offset + 8].try_into().unwrap_or_default()) as usize;

        offset += 8;

        if bytes.len() < offset + constants_len {
            return Err(String::from("Unexpected EOF"));
        }

        //Get constants
        let constants = Values::decode_from_bytes(&bytes[offset..constants_len + offset])?;
    
        offset += constants_len;

        if bytes.len() < offset + 8 {
            return Err(String::from("Unexpected EOF"));
        }

        //Get code length
        let code_len = u64::from_be_bytes(bytes[offset..offset + 8].try_into().unwrap_or_default()) as usize;

        offset += 8;

        if bytes.len() < offset + code_len {
            return Err(String::from("Unexpected EOF"));
        }

        //Get code
        let code = Vec::from(&bytes[offset..code_len + offset]);

        Ok(Self {
            code,
            name: None,
            constants,
            metadata
        })
    }

    pub fn verify_signature(bytes: &[u8]) -> bool {
        let expected_signature = *crate::constants::BIN_SIGNATURE;

        if bytes.is_empty() || bytes.len() < expected_signature.len() {
            return false;
        }

        let signature = &bytes[0..expected_signature.len()];

        if !signature.is_ascii() {
            return false;
        }

        let signature_str = std::str::from_utf8(signature).unwrap();

        signature_str == expected_signature
    }

}

impl<Idx> Index<Idx> for Chunk
    where Idx : SliceIndex<[u8], Output = u8>
{
    type Output = u8;

    fn index(&self, index: Idx) -> &Self::Output {
        &self.code[index]
    }
}

impl<Idx> IndexMut<Idx> for Chunk
    where Idx : SliceIndex<[u8], Output = u8>
{
    fn index_mut(&mut self, index: Idx) -> &mut Self::Output {
        &mut self.code[index]
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.disassemble()))
    }
}

#[derive(Debug)]
pub struct ChunkIter<'a> {
    chunk: &'a Chunk,
    n: usize
}

impl<'a> From<&'a Chunk> for ChunkIter<'a> {
    fn from(value: &'a Chunk) -> Self {
        Self {
            chunk: value,
            n: 0
        }
    }
}

impl<'a> Iterator for ChunkIter<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.chunk.try_at(self.n)?;
        self.n += 1_usize;
        Some(result)
    }
}
