use md5::{Digest, Md5};

pub fn solve() {
    let input = "ojvtpuvg";
    let mut password: Vec<char> = Vec::new();
    let mut next = 0;
    while password.len() < 8 {
        let to_be_hashed = input.to_owned() + &*next.to_string();
        let mut hasher = Md5::new();
        hasher.update(to_be_hashed);
        let result = format!("{:x}", &hasher.finalize());
        if &result[0..5] == "00000" {
            password.push(result.chars().nth(5).unwrap());
        }
        next += 1;
    }
    let result: String = password.iter().collect();
    print!("{}\n", result);
}

