use regex::Regex;

struct Node { x: u8, y: u8, size: u16, used: u16 }

impl Node {

    fn available(&self) -> u16 {
        self.size - self.used
    }

    fn is_viable_pair(&self, other: &Node) -> bool {
        self.used > 0
            && !(self.x == other.x && self.y == other.y)
            && self.used <= other.available()
    }
}

fn parse_instruction(line: &str) -> Node {
    let re = Regex::new(r"/dev/grid/node-x([0-9]+)-y([0-9]+)\s+([0-9]+)T\s+([0-9]+)T\s+[0-9]+T\s+[0-9]+%").unwrap();
    match re.captures(line) {
        Some(capture) => Node {
            x: capture.get(1).unwrap().as_str().parse::<u8>().unwrap(),
            y: capture.get(2).unwrap().as_str().parse::<u8>().unwrap(),
            size: capture.get(3).unwrap().as_str().parse::<u16>().unwrap(),
            used: capture.get(4).unwrap().as_str().parse::<u16>().unwrap(),
        },
        None => panic!("can't parse line: {}", line)
    }
}

pub fn solve() {
    let input: Vec<&str> = include_str!("2016-22.txt").split("\n").collect();
    let nodes: Vec<Node> = input.iter().skip(2).map(|line| parse_instruction(line)).collect();

    print!("{}\n", nodes.iter()
        .fold(0, |acc, node| acc + nodes.iter().clone()
            .fold(0, |acc, candidate| acc + if node.is_viable_pair(candidate) { 1 } else { 0 })
        )
    );
}
