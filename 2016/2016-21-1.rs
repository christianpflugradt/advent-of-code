use regex::Regex;
use crate::s_2016_21_1::Direction::{Left, Right};
use crate::s_2016_21_1::Instruction::{Move, Reverse, RotateBy, RotateSteps, SwapLetters, SwapPositions};

const ORIGINAL: &str = "abcdefgh";

#[derive(Clone, PartialEq)]
enum Direction { Left, Right }

struct SwapPositionsInstr { a: usize, b: usize }
struct SwapLettersInstr { a: char, b: char }
struct RotateStepsInstr { steps: usize, direction: Direction }
struct RotateByInstr { letter: char }
struct ReverseInstr { a: usize, b: usize }
struct MoveInstr { a: usize, b: usize }

enum Instruction {
    SwapPositions(SwapPositionsInstr),
    SwapLetters(SwapLettersInstr),
    RotateSteps(RotateStepsInstr),
    RotateBy(RotateByInstr),
    Reverse(ReverseInstr),
    Move(MoveInstr),
}

struct Scrambler {
    instructions: Vec<Instruction>,
}

impl Scrambler {

    fn scramble(&self, password: &str) -> String {
        let mut scrambled = password.to_string();
        for instruction in &self.instructions {
            scrambled = Self::apply(&instruction, &scrambled);
        }
        scrambled
    }

    fn apply(instruction: &Instruction, password: &str) -> String {
        match instruction {
            SwapPositions(it) => Self::swap_positions(password, it.a, it.b),
            SwapLetters(it) => Self::swap_letters(password, it.a, it.b),
            RotateSteps(it) => Self::rotate_steps(password, it.steps, it.direction.clone()),
            RotateBy(it) => Self::rotate_by(password, it.letter),
            Reverse(it) => Self::reverse(password, it.a, it.b),
            Move(it) => Self::move_(password, it.a, it.b),
        }
    }

    fn swap_positions(password: &str, a: usize, b: usize) -> String {
        let mut chars: Vec<char> = password.chars().collect();
        chars.swap(a, b);
        chars.iter().collect()
    }

    fn swap_letters(password: &str, a: char, b: char) -> String {
        password.chars().map(|c| match c {
            c if c == a => b,
            c if c == b => a,
            _ => c,
        }).collect()
    }

    fn rotate_steps(password: &str, steps: usize, direction: Direction) -> String {
        let n = steps % password.len();
        let split = if direction == Left { n } else { password.len() - n };
        let (left, right) = password.split_at(split);
        format!("{}{}", right, left)
    }

    fn rotate_by(password: &str, letter: char) -> String {
        match password.find(letter) {
            Some(index) => Self::rotate_steps(password, if index >= 4 { index + 2 } else { index + 1 }, Right),
            None => panic!("not found: {} in {}", letter, password)
        }
    }

    fn reverse(password: &str, a: usize, b: usize) -> String {
        let str = password.to_string();
        let before = &str[..a];
        let center = &str[a..b+1];
        let after = &str[b+1..];
        format!("{}{}{}", before, center.chars().rev().collect::<String>(), after)
    }

    fn move_(password: &str, a: usize, b: usize) -> String {
        let mut str = password.to_string();
        let chr = str.remove(a);
        str.insert(b, chr);
        str
    }
}

fn match_swap_positions(line: &str) -> Option<SwapPositionsInstr> {
    let re = Regex::new(r"swap position ([0-9]) with position ([0-9])").unwrap();
    match re.captures(line) {
        Some(capture) => Some(SwapPositionsInstr {
            a: capture.get(1).unwrap().as_str().parse::<usize>().unwrap(),
            b: capture.get(2).unwrap().as_str().parse::<usize>().unwrap(),
        }),
        None => None
    }
}

fn match_swap_letters(line: &str) -> Option<SwapLettersInstr> {
    let re = Regex::new(r"swap letter ([a-z]) with letter ([a-z])").unwrap();
    match re.captures(line) {
        Some(capture) => Some(SwapLettersInstr {
            a: capture.get(1).unwrap().as_str().chars().next().unwrap(),
            b: capture.get(2).unwrap().as_str().chars().next().unwrap(),
        }),
        None => None
    }
}

fn match_rotate_steps(line: &str) -> Option<RotateStepsInstr> {
    let re = Regex::new(r"rotate (right|left) ([0-9]) step").unwrap();
    match re.captures(line) {
        Some(capture) => Some(RotateStepsInstr {
            direction: if capture.get(1).unwrap().as_str() == "left" { Left } else { Right },
            steps: capture.get(2).unwrap().as_str().parse::<usize>().unwrap(),
        }),
        None => None
    }
}

fn match_rotate_by(line: &str) -> Option<RotateByInstr> {
    let re = Regex::new(r"rotate based on position of letter ([a-z])").unwrap();
    match re.captures(line) {
        Some(capture) => Some(RotateByInstr {
            letter: capture.get(1).unwrap().as_str().chars().next().unwrap(),
        }),
        None => None
    }
}

fn match_reverse(line: &str) -> Option<ReverseInstr> {
    let re = Regex::new(r"reverse positions ([0-9]) through ([0-9])").unwrap();
    match re.captures(line) {
        Some(capture) => Some(ReverseInstr {
            a: capture.get(1).unwrap().as_str().parse::<usize>().unwrap(),
            b: capture.get(2).unwrap().as_str().parse::<usize>().unwrap(),
        }),
        None => None
    }
}

fn match_move(line: &str) -> Option<MoveInstr> {
    let re = Regex::new(r"move position ([0-9]) to position ([0-9])").unwrap();
    match re.captures(line) {
        Some(capture) => Some(MoveInstr {
            a: capture.get(1).unwrap().as_str().parse::<usize>().unwrap(),
            b: capture.get(2).unwrap().as_str().parse::<usize>().unwrap(),
        }),
        None => None
    }
}

pub fn solve() {
    let input: Vec<&str> = include_str!("2016-21.txt").split("\n").collect();
    let instructions: Vec<Instruction> = input.iter().map(|line| {
        if let Some(result) = match_swap_positions(&line) {
            return SwapPositions(result)
        } else if let Some(result) = match_swap_letters(&line) {
            return SwapLetters(result)
        } else if let Some(result) = match_rotate_steps(&line) {
            return RotateSteps(result)
        } else if let Some(result) = match_rotate_by(&line) {
            return RotateBy(result)
        } else if let Some(result) = match_reverse(&line) {
            return Reverse(result)
        } else if let Some(result) = match_move(&line) {
            return Move(result)
        } else {
            panic!("can't parse line: {}", line)
        }
    }).collect();
    print!("{}\n", Scrambler { instructions }.scramble(ORIGINAL));
}
