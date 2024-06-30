use regex::Regex;

struct Disc {
    num: usize,
    size: usize,
    position: usize,
}

impl Disc {
    fn is_open(&self) -> bool {
        self.position == 0
    }

    fn tick(&mut self) {
        self.position += 1;
        if self.position == self.size {
            self.position = 0
        }
    }

    fn init(&mut self) {
        for _ in 0..self.num {
            self.tick()
        }
    }
}

fn find_solution(discs: &mut Vec<Disc>) -> usize {
    let mut seconds = 0;
    loop {
        if discs.iter().all(|d| d.is_open()) {
            return seconds
        }
        discs.iter_mut().for_each(|d| d.tick());
        seconds += 1;
    }
}

fn parse_instruction(instruction: &str) -> Disc {
    let re = Regex::new(r"Disc #([0-9]+) has ([0-9]+) positions; at time=0, it is at position ([0-9]+).").unwrap();
    match re.captures(instruction) {
        Some(capture) => Disc {
            num: capture.get(1).unwrap().as_str().parse::<usize>().unwrap(),
            size: capture.get(2).unwrap().as_str().parse::<usize>().unwrap(),
            position: capture.get(3).unwrap().as_str().parse::<usize>().unwrap(),
        },
        None => panic!("couldn't parse instruction: {}", instruction),
    }
}

pub fn solve() {
    let input: Vec<&str> = include_str!("2016-15.txt").split("\n").collect();
    let mut discs: Vec<Disc> = input.iter().map(|i| parse_instruction(i)).collect();
    discs.push(Disc {
        num: discs.len() + 1,
        size: 11,
        position: 0,
    });
    discs.iter_mut().for_each(|d| d.init());
    print!("{}\n", find_solution(&mut discs))
}
