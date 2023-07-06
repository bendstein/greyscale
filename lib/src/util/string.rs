use std::ops::Range;

use unicode_segmentation::UnicodeSegmentation;

#[derive(Debug, Default, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub struct GraphemeString<'a> {
    graphemes: Vec<&'a str>
}

impl<'a, 'b: 'a> GraphemeString<'a> {
    pub fn new(string: &'b str) -> Self {
        Self {
            graphemes: UnicodeSegmentation::graphemes(string, true).collect()
        }
    }

    pub fn iter(&self) -> core::slice::Iter<&'a str> {
        self.graphemes.iter()
    }

    pub fn char_at(&self, n: usize) -> &'a str {
        self.graphemes[n]
    }

    pub fn chars_in_range(&self, range: &Range<usize>) -> &[&'a str] {
        &self.graphemes[range.clone()]
    }

    pub fn substring(&self, range: &Range<usize>) -> String {
        let chars = self.chars_in_range(range);
        chars.join("")
    }

    pub fn len(&self) -> usize {
        self.graphemes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.graphemes.is_empty()
    }

}