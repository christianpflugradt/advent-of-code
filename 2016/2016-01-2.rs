use regex::Regex;
use std::cmp::PartialEq;
use std::collections::HashSet;
use crate::s_2016_01_2::CardinalDirection::{EAST, NORTH, WEST, SOUTH};
use crate::s_2016_01_2::Direction::{LEFT, RIGHT};

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
enum CardinalDirection {
    EAST, SOUTH, WEST, NORTH
}

#[derive(PartialEq)]
enum Direction {
    LEFT, RIGHT
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
struct Position {
    x: i32,
    y: i32,
}

struct Positions {
    current_position: Position,
    cardinal_direction: CardinalDirection,
    past_positions: HashSet<Position>,
    crossed_position: Option<Position>
}

impl Positions {
    fn update_positions(&mut self, is_x_axis: bool, is_positive: bool, steps: i32, cardinal_direction: CardinalDirection) {
        let signum = if is_positive { 1 } else { -1 };
        for step in 1..steps {
            let mut position = self.current_position.clone();
            if is_x_axis {
                position.x = position.x + step * signum;
            } else {
                position.y = position.y + step * signum;
            }
            if self.crossed_position.is_none() && !self.past_positions.insert(position) {
                self.crossed_position = Some(position.clone());
            }
        }
        if is_x_axis {
            self.current_position.x = self.current_position.x + steps * signum;
        } else {
            self.current_position.y = self.current_position.y + steps * signum;
        }
        self.cardinal_direction = cardinal_direction;
    }
}

struct Movement {
    steps: i32,
    direction: Direction,
}

impl Positions {
    fn go(&mut self, m: Movement) {
        match &self.cardinal_direction {
            EAST => {
                match m.direction {
                    LEFT => self.update_positions(false, true, m.steps, NORTH),
                    RIGHT => self.update_positions(false, false, m.steps, SOUTH),
                };
            },
            SOUTH => {
                match m.direction {
                    LEFT => self.update_positions(true, true, m.steps, EAST),
                    RIGHT => self.update_positions(true, false, m.steps, WEST),
                };
            },
            WEST => {
                match m.direction {
                    LEFT => self.update_positions(false, false, m.steps, SOUTH),
                    RIGHT => self.update_positions(false, true, m.steps, NORTH),
                };
            },
            NORTH => {
                match m.direction {
                    LEFT => self.update_positions(true, false, m.steps, WEST),
                    RIGHT => self.update_positions(true, true, m.steps, EAST),
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
    let p = Position { x: 0, y: 0 };
    let mut pos = Positions { current_position: p, cardinal_direction: NORTH, past_positions: HashSet::new(), crossed_position: None };
    let mut found = false;
    for instruction in input {
        pos.go(parse_instruction(instruction));
        if let Some(hq) = pos.crossed_position {
            println!("{}", hq.x.abs() + hq.y.abs());
            found = true;
            break;
        }
    }
    if !found {
        println!("no crossed position found")
    }
}

