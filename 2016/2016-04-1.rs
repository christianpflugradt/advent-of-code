use std::collections::HashMap;
use regex::Regex;

struct Room {
    name: String,
    sector_id: i32,
    checksum: String,
}

impl Room {
    fn is_real(&self) -> bool {
        let chars = Self::get_most_frequent_chars(self.create_common_char_map());
        chars.iter()
            .fold(1, |acc, &c| acc * occurrences_of_char_in_string(c, &self.checksum))
            .eq(&1)
    }

    fn create_common_char_map(&self) -> HashMap<i32, String> {
        let mut map: HashMap<i32, String> = HashMap::new();
        for ascii_val in 97..=122 {
            let chr = ascii_val as u8 as char;
            let count = occurrences_of_char_in_string(chr, &self.name) as i32;
            match map.get(&count) {
                None => {
                    map.insert(count, chr.to_string());
                }
                Some(_) => {
                    map.insert(count, map.get(&count).unwrap().as_str().to_string() + &chr.to_string());
                },
            }
        }
        map
    }

    fn get_most_frequent_chars(map: HashMap<i32, String>) -> Vec<char> {
        let mut chars: Vec<char> = Vec::new();
        let mut prev_max: i32 = std::i32::MAX;
        let mut max_key: i32 = 0;
        while chars.len() < 5 {
            for (key, _) in &map {
                if key > &max_key && key < &prev_max {
                    max_key = *key;
                }
            }
            let new_chars = map.get(&max_key).unwrap();
            chars.extend(new_chars.chars());
            prev_max = max_key;
            max_key = 0;
        }
        chars[..5].to_vec()
    }
}

fn occurrences_of_char_in_string(c: char, s: &str) -> usize {
    s.chars().filter(|&chr| chr == c).count()
}

fn parse_instruction(instruction: &str) -> Room {
    let re = Regex::new(r"([a-z]+)([0-9]+)\[([a-z]{5})]").unwrap();
    match re.captures(&*instruction.replace("-", "")) {
        Some(capture) => {
            Room {
                name: capture.get(1).unwrap().as_str().to_string(),
                sector_id: capture.get(2).unwrap().as_str().parse::<i32>().unwrap(),
                checksum: capture.get(3).unwrap().as_str().to_string(),
            }
        },
        None => panic!("couldn't parse instruction: {}", instruction)
    }
}

pub fn solve() {
    let input: Vec<&str> = include_str!("2016-04.txt").split("\n").collect();
    let sector_id_sum = input.iter()
        .map(|input| parse_instruction(input))
        .filter(|room| room.is_real())
        .fold(0, |acc, room| acc + room.sector_id);
    print!("{}\n", sector_id_sum)
}

