use std::collections::HashMap;
use std::ops::RangeInclusive;

const INDICES: RangeInclusive<usize> = 0..=7;

fn calc_candidates(strings: &Vec<&str>) -> HashMap<(usize, char), i32> {
    let mut map: HashMap<(usize, char), i32> = HashMap::new();
    for string in strings {
        let chars: Vec<char> = string.chars().collect();
        for i in INDICES {
            let chr = chars.get(i)
                .unwrap_or_else(|| panic!("unexpected length of less than 8: {}", string))
                .to_owned();
            match map.get(&(i, chr)) {
                None => map.insert((i, chr), 1),
                Some(num) => map.insert((i, chr), num + 1),
            };
        }
    }
    map
}

fn extract_best(candidates: &HashMap<(usize, char), i32>) -> Vec<char> {
    let mut most: HashMap<usize, (char, i32)> = HashMap::new();
    let default = ('-', i32::MAX);
    for i in INDICES {
        most.insert(i, default);
        for l in 'a'..='z' {
            let (_, highest_val) = most.get(&i).unwrap_or(&default);
            let candidate = candidates.get(&(i, l)).unwrap_or(&default.1);
            if candidate > &0 && candidate < highest_val {
                most.insert(i, (l, *candidate));
            }
        }
    }
    INDICES.map(|i| most.get(&i).unwrap_or_else(|| panic!("key not found: {}", i)))
        .map(|t| t.0).collect()
}

pub fn solve() {
    let input: Vec<&str> = include_str!("2016-06.txt").split("\n").collect();
    let result: String = extract_best(&calc_candidates(&input)).iter().collect();
    print!("{}\n", result);
}

