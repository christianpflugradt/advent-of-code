use regex::{Captures, Regex};

#[derive(Copy, Clone)]
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

fn unwrap_capture(captures: &Vec<Option<Captures>>, row: usize, col: usize) -> i32 {
    captures.get(row)
        .and_then(|cap| cap.as_ref().unwrap().get(col))
        .and_then(|m| Some(m.as_str().parse::<i32>().unwrap())).unwrap()
}

fn parse_instructions(instr1: Option<&&str>, instr2: Option<&&str>, instr3: Option<&&str>) -> Vec<Triangle> {
    let re = Regex::new(r"\s+([0-9]+)\s+([0-9]+)\s+([0-9]+)").unwrap();
    let instructions = [instr1.unwrap(), instr2.unwrap(), instr3.unwrap()];
    let caps: Vec<Option<Captures>> = instructions.iter().map(|instruction| re.captures(instruction)).collect();
    [1, 2, 3].map(|col| Triangle {
        a: unwrap_capture(&caps, 0, col),
        b: unwrap_capture(&caps, 1, col),
        c: unwrap_capture(&caps, 2, col),
    }).to_vec()
}

pub fn solve() {
    let input: Vec<&str> = include_str!("2016-03.txt").split("\n").collect();
    let mut triangles: Vec<Triangle> = Vec::new();
    for row in (0..input.len()-1).step_by(3) {
        triangles.extend(parse_instructions(
            input.get(row),
            input.get(row + 1),
            input.get(row + 2)
        ));
    }
    let possible_triangle_count = triangles.iter()
        .filter(|triangle| triangle.is_possible())
        .count();
    print!("{}\n", possible_triangle_count)
}

