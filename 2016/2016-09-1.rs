const MARKER_START_CHAR: char = '(';
const MULTIPLY_CHAR: char = 'x';
const MARKER_END_CHAR: char = ')';

struct MarkerSpecs { new_pos: usize, decompressed_size: usize }

struct CompressedFileParser { content: Vec<char> }

impl CompressedFileParser {

    fn calc_decompressed_length(&self) -> Result<usize, String> {
        let mut pos = 0;
        let mut decompressed_length = 0;
        while pos < self.content.len() {
            if self.content[pos] == MARKER_START_CHAR {
                match self.parse_marker(pos) {
                    Some(marker_size) => {
                        pos = marker_size.new_pos;
                        decompressed_length += marker_size.decompressed_size;
                    },
                    None => return Err(format!("could not parse marker starting at pos={}", pos)),
                };
            } else {
                pos += 1;
                decompressed_length += 1;
            }
        }
        return Ok(decompressed_length);
    }

    fn parse_marker(&self, pos: usize) -> Option<MarkerSpecs> {
        let multiply_pos = Self::find_next(&self.content, MULTIPLY_CHAR, pos)?;
        let marker_end_pos = Self::find_next(&self.content, MARKER_END_CHAR, multiply_pos)?;
        let characters = self.parse_number(pos + 1, multiply_pos)?;
        let repetitions = self.parse_number(multiply_pos + 1, marker_end_pos)?;
        Some(MarkerSpecs {
            new_pos: marker_end_pos + characters + 1,
            decompressed_size: (characters * repetitions),
        })
    }

    fn parse_number(&self, start_index: usize, end_index: usize) -> Option<usize> {
        if start_index >= end_index || end_index > self.content.len() {
            return None
        }
        self.content[start_index..end_index].iter()
            .map(|c| c.to_digit(10))
            .collect::<Option<Vec<u32>>>()
            .map(|digits| {
                digits.iter().fold(0, |acc, digit| acc * 10 + digit)
            })
            .map(|u| u as usize)
    }

    fn find_next(chars: &Vec<char>, c: char, start_index: usize) -> Option<usize> {
        let mut pos = start_index;
        while pos < chars.len() {
            if chars[pos] == c {
                return Some(pos)
            }
            pos += 1;
        }
        None
    }
}

pub fn solve() {
    let input = include_str!("2016-09.txt");
    let result = CompressedFileParser { content: input.chars().collect() }
        .calc_decompressed_length();
    match result {
        Ok(length) => println!("{}", length),
        Err(e) => println!("{}", e),
    }
}
