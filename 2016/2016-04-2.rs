use regex::Regex;

struct Room {
    name: String,
    sector_id: i32,
}

impl Room {
    fn decrypt(&self) -> String {
        self.name.chars().map(|c| {
            let o = (c as u8) + (self.sector_id % 26) as u8;
            (if o < 123 { o } else { 97 }) as char
        }).collect()
    }
}

fn parse_instruction(instruction: &str) -> Room {
    let re = Regex::new(r"([a-z]+)([0-9]+)\[[a-z]{5}]").unwrap();
    match re.captures(&*instruction.replace("-", "")) {
        Some(capture) => {
            Room {
                name: capture.get(1).unwrap().as_str().to_string(),
                sector_id: capture.get(2).unwrap().as_str().parse::<i32>().unwrap(),
            }
        },
        None => panic!("couldn't parse instruction: {}", instruction)
    }
}

pub fn solve() {
    let input: Vec<&str> = include_str!("2016-04.txt").split("\n").collect();
    let result = input.iter()
        .map(|input| parse_instruction(input))
        .find(|room| room.decrypt().starts_with("northpole"))
        .unwrap().sector_id;
    print!("{}\n", result)
}

