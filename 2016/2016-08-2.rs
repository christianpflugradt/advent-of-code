use std::collections::HashMap;
use std::thread::sleep;
use std::time::Duration;

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
        print_coordinates(coordinates);
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
        print_coordinates(coordinates);
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
        print_coordinates(coordinates);
    }
}

struct Drawer {
    filled: String,
    blank: String,
    multiplier: usize,
    reset: String,
    sleep_millis: u64,
    gap: usize,
    column_count: usize
}

impl Drawer {
    fn draw_blank_row(&self) {
        repeat(self.column_count * self.multiplier, || print!("{}", self.blank));
        println!();
    }

    fn draw_blank(&self) {
        repeat(self.multiplier, || print!("{}", self.blank));
    }

    fn draw_filled(&self) {
        repeat(self.multiplier, || print!("{}", self.filled));
    }

    fn draw_gap(&self, with_line_break: bool) {
        repeat(self.gap * self.multiplier, || print!("{}", self.blank));
        if with_line_break { println!() };
    }

    fn draw_reset(&self) {
        sleep(Duration::from_millis(self.sleep_millis));
        print!("{}", self.reset);
    }
}


fn repeat<F: Fn() -> ()>(times: usize, f: F) { for _ in 0..times { f(); } }

fn default_drawer() -> Drawer {
    Drawer {
        filled: "\u{2588}".to_string(),
        blank: " ".to_string(),
        multiplier: 2,
        gap: 1,
        column_count: (COLUMNS + COLUMNS / 5 * 1 + 1) as usize,
        reset: "\x1B[2J\x1B[1;1H".to_string(),
        sleep_millis: 40,
    }
}

fn print_coordinates(coordinates: &mut Coordinates) {
    let drawer = default_drawer();
    drawer.draw_reset();
    drawer.draw_blank_row();
    for y in 0..ROWS {
        for x in 0..COLUMNS {
            if x % 5 == 0 { drawer.draw_gap(false) };
            if *coordinates.get(&(x, y)).unwrap() { drawer.draw_filled() } else { drawer.draw_blank() };
        }
        drawer.draw_gap(true);
    }
    drawer.draw_blank_row();
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
    print_coordinates(&mut screen);
}

