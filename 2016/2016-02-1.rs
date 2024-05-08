use crate::s_2016_02_1::Key::{_1, _2, _3, _4, _5, _6, _7, _8, _9};

#[derive(Clone, Copy)]
enum Key {
    _1, _2, _3,
    _4, _5, _6,
    _7, _8, _9
}

impl Into<i32> for Key {
    fn into(self) -> i32 {
        match self {
            _1 => 1,
            _2 => 2,
            _3 => 3,
            _4 => 4,
            _5 => 5,
            _6 => 6,
            _7 => 7,
            _8 => 8,
            _9 => 9,
        }
    }
}

struct Pos { k: Key }

impl Pos {
    fn left(&mut self) {
        self.k = match self.k {
            _1 => _1,
            _2 => _1,
            _3 => _2,
            _4 => _4,
            _5 => _4,
            _6 => _5,
            _7 => _7,
            _8 => _7,
            _9 => _8,
        }
    }
    fn up(&mut self) {
        self.k = match self.k {
            _1 => _1,
            _2 => _2,
            _3 => _3,
            _4 => _1,
            _5 => _2,
            _6 => _3,
            _7 => _4,
            _8 => _5,
            _9 => _6,
        }
    }
    fn right(&mut self) {
        self.k = match self.k {
            _1 => _2,
            _2 => _3,
            _3 => _3,
            _4 => _5,
            _5 => _6,
            _6 => _6,
            _7 => _8,
            _8 => _9,
            _9 => _9,
        }
    }
    fn down(&mut self) {
        self.k = match self.k {
            _1 => _4,
            _2 => _5,
            _3 => _6,
            _4 => _7,
            _5 => _8,
            _6 => _9,
            _7 => _7,
            _8 => _8,
            _9 => _9,
        }
    }
}

pub fn solve() {
    let mut pos = Pos { k: _5 };
    let input: &str = include_str!("2016-02.txt");
    let mut numbers: Vec<i32> = Vec::new();
    for command in input.chars() {
        match command {
            'L' => pos.left(),
            'U' => pos.up(),
            'R' => pos.right(),
            'D' => pos.down(),
            '\n' => numbers.push(pos.k.clone().into()),
            other => panic!("unknown command: {}", other)
        }
    }
    numbers.push(pos.k.clone().into());
    print!("{}\n", numbers.iter().map(|&i| i.to_string()).collect::<Vec<String>>().join(""))
}

