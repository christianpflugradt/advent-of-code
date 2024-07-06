use std::cmp::PartialEq;
use md5::{Digest, Md5};
use crate::s_2016_17_2::Direction::{DOWN, LEFT, RIGHT, UP};

type State = (usize, usize, Vec<Direction>);
#[derive(Clone, PartialEq, Debug, Copy)]
enum Direction { UP, DOWN, LEFT, RIGHT }

const INPUT: &str = "pxxbnzuo";

const OPEN_CHARS: [char; 5] = ['b', 'c', 'd', 'e', 'f'];
const DIRECTION_MAP: [(usize, Direction); 4] = [(0, UP), (1, DOWN), (2, LEFT), (3, RIGHT)];

fn direction_string(directions: Vec<Direction>) -> String {
    directions.iter().map(|d| match d {
        UP => 'U',
        DOWN => 'D',
        LEFT => 'L',
        RIGHT => 'R',
    }).collect()
}

fn door_is_open(hash: &str, index: usize) -> bool {
    if let Some(c) = hash.chars().nth(index) {
        OPEN_CHARS.contains(&c)
    } else {
        false
    }
}

fn open_directions(state: State) -> Vec<Direction> {
    let to_be_hashed = format!("{}{}", INPUT, direction_string(state.2));
    let mut hasher = Md5::new();
    hasher.update(to_be_hashed);
    let hash = format!("{:x}", &hasher.finalize());
    let mut directions: Vec<Direction> = Vec::new();
    for pair in DIRECTION_MAP {
        if door_is_open(&hash, pair.0) { directions.push(pair.1) }
    }
    if state.0 == 1 { directions.retain(|d| d != &LEFT) }
    if state.0 == 4 { directions.retain(|d| d != &RIGHT) }
    if state.1 == 1 { directions.retain(|d| d != &UP) }
    if state.1 == 4 { directions.retain(|d| d != &DOWN) }
    directions
}

fn next_x(x: usize, direction: Direction) -> usize {
    match direction {
        UP => x,
        DOWN => x,
        LEFT => x-1,
        RIGHT => x+1,
    }
}

fn next_y(y: usize, direction: Direction) -> usize {
    match direction {
        UP => y-1,
        DOWN => y+1,
        LEFT => y,
        RIGHT => y,
    }
}

fn find_solution() -> usize {
    let mut states: Vec<State> = Vec::new();
    states.push((1, 1, Vec::new()));
    let mut max_length= 0;
    loop {
        let mut next_states: Vec<State> = Vec::new();
        while let Some(state) = states.pop() {
            if state.0 == 4 && state.1 == 4 {
                max_length = direction_string(state.2.clone()).len();
                continue
            }
            for d in open_directions(state.clone()) {
                let mut copy = state.2.to_vec();
                copy.push(d);
                next_states.push((next_x(state.0, d), next_y(state.1, d), copy));
            }
        }
        states = next_states.to_vec();
        if states.is_empty() {
            return max_length
        }
    }
}

pub fn solve() {
    print!("{}\n", find_solution())
}
