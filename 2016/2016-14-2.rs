use std::iter::repeat;
use md5::{Digest, Md5};

struct Key {
    c: char,
    i: usize,
    hashes: usize,
    valid: bool,
}

impl Key {
    fn validate(&mut self, hash: &String) {
        if self.hashes > 1000 || self.valid {
            return
        }
        let sequence: String = repeat(self.c).take(5).collect();
        if hash.contains(&sequence) {
            self.valid = true
        } else {
            self.hashes += 1
        }
    }
}

fn find_64th_key_index(input: &str) -> usize {
    let mut keys: Vec<Key> = Vec::new();
    let mut next = 0;
    loop {
        let to_be_hashed = input.to_owned() + &*next.to_string();
        let mut hash_str = to_be_hashed;
        for _ in 0..(2016+1) {
            let mut hasher = Md5::new();
            hasher.update(hash_str.to_owned());
            hash_str = format!("{:x}", &hasher.finalize());
        }
        let hash: Vec<char> = hash_str.chars().collect();
        keys.iter_mut().for_each(|k| k.validate(&hash_str));
        if keys.iter().filter(|k| k.valid).count() >= 64 {
            let keys: Vec<&Key> = keys.iter().filter(|k| k.valid).collect();
            return keys[63].i
        }
        for i in 0..(hash_str.len() - 2) {
            if hash[i] == hash[i+1] && hash[i] == hash[i+2] {
                keys.push(Key { c: hash[i], i: next, hashes: 0, valid: false });
                break
            }
        }
        next += 1
    }
}

pub fn solve() {
    let input = "zpqevtbw";
    println!("{}\n", find_64th_key_index(input))
}
