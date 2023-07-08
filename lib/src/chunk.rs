pub mod disassemble;

use std::{slice::SliceIndex, ops::{Index, IndexMut}, fmt::Display};
use crate::{value::{Values, Value}, metadata::Metadata};

#[derive(Default, Debug)]
pub struct Chunk {
    pub name: Option<String>,
    code: Vec<u8>,
    constants: Values,
    pub metadata: Metadata
}

impl Chunk {
    pub fn write(&mut self, code: u8) {
        self.code.push(code);
    }

    pub fn write_u16(&mut self, code: u16) {
        let a1 = ((code >> 8) & 0xFF) as u8;
        let a2 = (code & 0xFF) as u8;
        self.write(a1);
        self.write(a2);
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
        self.disassemble(f)
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