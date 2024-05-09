use regex::Regex;

struct Triangle {
    a: i32,
    b: i32,
    c: i32,
}

impl Triangle {
    fn is_possible(&self) -> bool {
        self.a + self.b > self.c &&
            self.a + self.c > self.b &&
            self.b + self.c > self.a
    }
}

fn parse_instruction(instruction: &str) -> Triangle {
    let re = Regex::new(r"\s+([0-9]+)\s+([0-9]+)\s+([0-9]+)").unwrap();
    match re.captures(instruction) {
        Some(capture) => {
            Triangle {
                a: capture.get(1).unwrap().as_str().parse::<i32>().unwrap(),
                b: capture.get(2).unwrap().as_str().parse::<i32>().unwrap(),
                c: capture.get(3).unwrap().as_str().parse::<i32>().unwrap(),
            }
        },
        None => panic!("couldn't parse instruction: {}", instruction)
    }
}

pub fn solve() {
    let input: Vec<&str> = include_str!("2016-03.txt").split("\n").collect();
    let possible_triangle_count = input.iter()
        .map(|input| parse_instruction(input))
        .filter(|triangle| triangle.is_possible())
        .count();
    print!("{}\n", possible_triangle_count)
}

