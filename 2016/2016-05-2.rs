use std::collections::HashMap;
use md5::{Digest, Md5};

pub fn solve() {
    let input = "ojvtpuvg";
    let valid_positions: Vec<char> = (0..=7).map(|i| char::from_digit(i, 10).unwrap()).collect();
    let mut password: HashMap<char, char> = HashMap::new();
    let mut next = 0;
    while password.len() < 8 {
        let to_be_hashed = input.to_owned() + &*next.to_string();
        let mut hasher = Md5::new();
        hasher.update(to_be_hashed);
        let result = format!("{:x}", &hasher.finalize());
        if &result[0..5] == "00000" {
            let pos = result.chars().nth(5).unwrap();
            let chr = result.chars().nth(6).unwrap();
            if valid_positions.contains(&pos) && !password.contains_key(&pos) {
                password.insert(pos, chr);
            }
        }
        next += 1;
    }
    let result: String = valid_positions.iter().map(|pos| password.get(pos).unwrap()).collect();
    print!("{}\n", result);
}

