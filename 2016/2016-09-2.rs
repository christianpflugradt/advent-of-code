const MARKER_START_CHAR: char = '(';
const MULTIPLY_CHAR: char = 'x';
const MARKER_END_CHAR: char = ')';
const GENERIC_ERROR: &str = "oops, good luck trying to find out what went wrong";

fn calc_decompressed_length(content: &[char]) -> Result<usize, String> {
    let mut pos = 0;
    let mut decompressed_length = 0;
    while pos < content.len() {
        if content[pos] == MARKER_START_CHAR {
            let multiply_pos = find_next(&content, MULTIPLY_CHAR, pos).ok_or(GENERIC_ERROR)?;
            let marker_end_pos = find_next(&content, MARKER_END_CHAR, multiply_pos).ok_or(GENERIC_ERROR)?;
            let characters = parse_number(&content, pos + 1, multiply_pos).ok_or(GENERIC_ERROR)?;
            let repetitions = parse_number(&content, multiply_pos + 1, marker_end_pos).ok_or(GENERIC_ERROR)?;
            let sub_start = marker_end_pos + 1;
            let sub_end = sub_start + characters;
            pos = marker_end_pos + characters + 1;
            decompressed_length += repetitions * calc_decompressed_length(&content[sub_start..sub_end])?;
        } else {
            pos += 1;
            decompressed_length += 1;
        }
    }
    return Ok(decompressed_length);
}

fn parse_number(content: &[char], start_index: usize, end_index: usize) -> Option<usize> {
    if start_index >= end_index || end_index > content.len() {
        return None
    }
    content[start_index..end_index].iter()
        .map(|c| c.to_digit(10))
        .collect::<Option<Vec<u32>>>()
        .map(|digits| {
            digits.iter().fold(0, |acc, digit| acc * 10 + digit)
        })
        .map(|u| u as usize)
}

fn find_next(content: &[char], c: char, start_index: usize) -> Option<usize> {
    let mut pos = start_index;
    while pos < content.len() {
        if content[pos] == c {
            return Some(pos)
        }
        pos += 1;
    }
    None
}

pub fn solve() {
    let input: Vec<char> = include_str!("2016-09.txt").chars().collect();
    let result = calc_decompressed_length(&input);
    match result {
        Ok(length) => println!("{}", length),
        Err(e) => println!("{}", e),
    }
}
