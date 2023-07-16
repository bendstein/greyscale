#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Metadata {
    newlines: Vec<(usize, usize)>
}

impl Metadata {
    pub fn new_line(&mut self, index: usize, line: usize) {
        self.newlines.push((index, line));
    }

    pub fn get_line(&self, index: usize) -> usize {
        if self.newlines.is_empty() {
            0
        }
        else {
            let mut lower_bound = 0;
            let mut start = 0;
            let mut end = self.newlines.len() - 1;
            let (mut at_start, _) = self.newlines[start];
            let (mut at_end, _) = self.newlines[end];

            let found_index: Option<usize>;

            loop {
                if index == at_start {
                    found_index = Some(start);
                    break;
                }
                else if index >= at_end {
                    found_index = Some(end);
                    break;
                }
                else if index < at_start || end - start <= 1 {
                    found_index = Some(lower_bound);
                    break;
                }
                else {
                    let midpoint = start + ((end - start) / 2);
                    let (at_midpoint, _) = self.newlines[midpoint];
                    
                    if at_midpoint > index {
                        end = midpoint - 1;
                        (at_end, _) = self.newlines[end];
                    }
                    else {
                        start = midpoint;
                        at_start = at_midpoint;
                        lower_bound = midpoint;
                    }
                }
            }

            if let Some(findx) = found_index {
                if findx < self.newlines.len() {
                    return self.newlines[findx].1;
                }
            }

            0_usize
        }
    }

    pub fn current_line(&self) -> usize {
        if let Some((_, line)) = self.newlines.last() {
            *line
        }
        else {
            0_usize
        }
    }

    pub fn line_count(&self) -> usize {
        *self.newlines.iter()
            .map(|(_, n)| n)
            .max()
            .unwrap_or(&0)
    }

    pub fn encode_as_bytes(&self) -> Vec<u8> {
        let mut bytes: Vec<u8> = Vec::new();

        //Push new line data
        for line in &self.newlines {
            bytes.extend((line.0 as u64).to_be_bytes());
            bytes.extend((line.1 as u64).to_be_bytes());
        }

        //Prepend with newline bytes length
        let bytes_temp = bytes;
        let mut bytes = Vec::from((self.newlines.len() as u64 * 16_u64).to_be_bytes());
        bytes.extend(bytes_temp);

        bytes
    }

    pub fn decode_from_bytes(bytes: &[u8]) -> Self {
        let mut newlines: Vec<(usize, usize)> = Vec::new();

        //Get newline data length
        let lines_count = u64::from_be_bytes(bytes[0..8].try_into().unwrap_or_default()) as usize;

        let mut offset = 8;

        while offset + 8 <= lines_count {
            let a = u64::from_be_bytes(bytes[offset..offset + 8].try_into().unwrap_or_default()) as usize;
            offset += 8;

            let b = u64::from_be_bytes(bytes[offset..offset + 8].try_into().unwrap_or_default()) as usize;
            offset += 8;

            newlines.push((a, b));
        }

        Self {
            newlines
        }
    }
}