use regex::Regex;

type Range = (usize, usize);

fn parse_range(range: &str) -> Range {
    let re = Regex::new(r"([0-9]+)-([0-9]+)").unwrap();
    match re.captures(range) {
        Some(capture) => return (
            capture.get(1).unwrap().as_str().parse::<usize>().unwrap(),
            capture.get(2).unwrap().as_str().parse::<usize>().unwrap(),
        ),
        None => panic!("couldn't parse range: {}", range)
    }
}

fn first_not_in_range(ranges: &Vec<Range>, lower: usize, upper: usize) -> usize {
    let mut candidates: Vec<Range> = ranges.iter()
        .filter(|r| r.1 >= lower && r.0 <= upper).cloned().collect();
    if candidates.is_empty() {
        upper
    } else {
        candidates.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
        first_not_in_range(ranges, upper, candidates[0].1 + 1)
    }
}

pub fn solve() {
    let input: Vec<&str> = include_str!("2016-20.txt").split("\n").collect();
    let maximum = 256usize.pow(4);
    let mut allowed = 0;
    let mut current = 0;
    while current <= maximum {
        current = first_not_in_range(&input.iter().map(|line| parse_range(line)).collect(), current, current) + 1;
        if current - 1 <= maximum { allowed += 1; }
    }
    println!("{}", allowed);
}