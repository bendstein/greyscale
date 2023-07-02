#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct Metadata {
    newlines: Vec<usize>
}

impl Metadata {
    pub fn new_line(&mut self, index: usize) {
        self.newlines.push(index);
    }

    pub fn get_line(&self, index: usize) -> usize {
        if self.newlines.is_empty() {
            0
        }
        else {
            let mut lower_bound = 0;
            let mut start = 0;
            let mut end = self.newlines.len() - 1;
            let mut at_start = self.newlines[start];
            let mut at_end = self.newlines[end];

            loop {
                if index == at_start {
                    return start + 1;
                }
                else if index >= at_end {
                    return end + 1;
                }
                else if index < at_start || end - start <= 1 {
                    return lower_bound;
                }
                else {
                    let midpoint = start + ((end - start) / 2);
                    let at_midpoint = self.newlines[midpoint];
                    
                    if at_midpoint > index {
                        end = midpoint - 1;
                        at_end = self.newlines[end];
                    }
                    else {
                        start = midpoint;
                        at_start = at_midpoint;
                        lower_bound = midpoint;
                    }
                }
            }
        }
    }

    pub fn line_count(&self) -> usize {
        self.newlines.len()
    }
}