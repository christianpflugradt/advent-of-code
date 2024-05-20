use std::collections::HashMap;

use regex::Regex;

const ROWS: u8 = 6;
const COLUMNS: u8 = 50;

type Coordinates = HashMap<(u8, u8), bool>;

struct Rectangle { x: u8, y: u8 }
struct RowRotation { y: u8, pixels: u8 }
struct ColumnRotation { x: u8, pixels: u8 }

trait Instruction {
    fn apply_on(&self, coordinates: &mut Coordinates);
}

impl Instruction for Rectangle {
    fn apply_on(&self, coordinates: &mut Coordinates) {
        for x in 0..self.x {
            for y in 0..self.y {
                coordinates.insert((x, y), true);
            }
        }
    }
}

impl Instruction for RowRotation {
    fn apply_on(&self, coordinates: &mut Coordinates) {
        for col in (0..COLUMNS).rev() {
            coordinates.insert((col + self.pixels, self.y), *coordinates.get(&(col, self.y)).unwrap());
        }
        for col in 0..self.pixels {
            coordinates.insert((col, self.y), *coordinates.get(&(COLUMNS + col, self.y)).unwrap());
        }
        for col in COLUMNS..(COLUMNS + self.pixels) {
            coordinates.remove(&(col, self.y));
        }
    }
}

impl Instruction for ColumnRotation {
    fn apply_on(&self, coordinates: &mut Coordinates) {
        for row in (0..ROWS).rev() {
            coordinates.insert((self.x, row + self.pixels), *coordinates.get(&(self.x, row)).unwrap());
        }
        for row in 0..self.pixels {
            coordinates.insert((self.x, row), *coordinates.get(&(self.x, ROWS + row)).unwrap());
        }
        for row in ROWS..(ROWS + self.pixels) {
            coordinates.remove(&(self.x, row));
        }
    }
}

fn parse_instruction(instruction: &str) -> Box<dyn Instruction> {
    parse_rectangle(instruction)
        .or(parse_row_rotation(instruction))
        .or(parse_column_rotation(instruction))
        .unwrap_or_else(|| panic!("couldn't parse instruction: {}", instruction))
}

fn parse_rectangle(instruction: &str) -> Option<Box<dyn Instruction>> {
    let re = Regex::new(r"rect ([0-9]+)x([0-9]+)").unwrap();
    match re.captures(instruction) {
        Some(capture) => Some(Box::new(Rectangle {
            x: capture.get(1).unwrap().as_str().parse::<u8>().unwrap(),
            y: capture.get(2).unwrap().as_str().parse::<u8>().unwrap(),
        })),
        None => None
    }
}

fn parse_row_rotation(instruction: &str) -> Option<Box<dyn Instruction>> {
    let re = Regex::new(r"rotate row y=([0-9]+) by ([0-9]+)").unwrap();
    match re.captures(instruction) {
        Some(capture) => Some(Box::new(RowRotation {
            y: capture.get(1).unwrap().as_str().parse::<u8>().unwrap(),
            pixels: capture.get(2).unwrap().as_str().parse::<u8>().unwrap(),
        })),
        None => None
    }
}

fn parse_column_rotation(instruction: &str) -> Option<Box<dyn Instruction>> {
    let re = Regex::new(r"rotate column x=([0-9]+) by ([0-9]+)").unwrap();
    match re.captures(instruction) {
        Some(capture) => Some(Box::new(ColumnRotation {
            x: capture.get(1).unwrap().as_str().parse::<u8>().unwrap(),
            pixels: capture.get(2).unwrap().as_str().parse::<u8>().unwrap(),
        })),
        None => None
    }
}

fn initial_screen() -> Coordinates {
    let mut screen: Coordinates = HashMap::new();
    for x in 0..COLUMNS {
        for y in 0..ROWS {
            screen.insert((x, y), false);
        }
    }
    screen
}

pub fn solve() {
    let input: Vec<&str> = include_str!("2016-08.txt").split("\n").collect();
    let mut screen = initial_screen();
    input.iter()
        .map(|&string| parse_instruction(string))
        .for_each(|instruction| instruction.apply_on(&mut screen));
    print!("{}\n", screen.values().filter(|value| **value == true).count());
}

