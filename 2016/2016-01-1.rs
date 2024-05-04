use regex::Regex;
use std::cmp::PartialEq;
use crate::s_2016_01_1::CardinalDirection::{EAST, NORTH, WEST, SOUTH};
use crate::s_2016_01_1::Direction::{LEFT, RIGHT};

enum CardinalDirection {
    EAST, SOUTH, WEST, NORTH
}

#[derive(PartialEq)]
enum Direction {
    LEFT, RIGHT
}

struct Position {
    x: i32,
    y: i32,
    cardinal_direction: CardinalDirection,
}

struct Movement {
    steps: i32,
    direction: Direction,
}

impl Position {
    fn go(&mut self, m: Movement) {
        match &self.cardinal_direction {
            EAST => {
                match m.direction {
                    LEFT => {
                        self.y += m.steps;
                        self.cardinal_direction = NORTH;
                    },
                    RIGHT => {
                        self.y -= m.steps;
                        self.cardinal_direction = SOUTH;
                    },
                };
            },
            SOUTH => {
                match m.direction {
                    LEFT => {
                        self.x += m.steps;
                        self.cardinal_direction = EAST;
                    },
                    RIGHT => {
                        self.x -= m.steps;
                        self.cardinal_direction = WEST;
                    },
                };
            },
            WEST => {
                match m.direction {
                    LEFT => {
                        self.y -= m.steps;
                        self.cardinal_direction = SOUTH;
                    },
                    RIGHT => {
                        self.y += m.steps;
                        self.cardinal_direction = NORTH;
                    },
                };
            },
            NORTH => {
                match m.direction {
                    LEFT => {
                        self.x -= m.steps;
                        self.cardinal_direction = WEST;
                    },
                    RIGHT => {
                        self.x += m.steps;
                        self.cardinal_direction = EAST;
                    },
                };
            },
        }
    }
}

fn parse_instruction(instruction: &str) -> Movement {
    let re = Regex::new(r"([LR])([0-9]+)").unwrap();
    match re.captures(instruction) {
        Some(capture) => {
            Movement {
                direction: if capture.get(1).unwrap().as_str() == "L" { LEFT } else { RIGHT },
                steps: capture.get(2).unwrap().as_str().parse::<i32>().unwrap(),
            }
        },
        None => panic!("couldn't parse instruction")
    }
}

pub fn solve() {
    let input: Vec<&str> = include_str!("2016-01.txt").split(", ").collect();
    let mut pos = Position { x: 0, y: 0, cardinal_direction: NORTH };
    for instruction in input {
        pos.go(parse_instruction(instruction));
    }
    print!("{}\n", pos.x.abs() + pos.y.abs());
}

