#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
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
        self.newlines.len()
    }
}