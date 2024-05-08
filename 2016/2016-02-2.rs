use crate::s_2016_02_2::Key::{_1, _2, _3, _4, _5, _6, _7, _8, _9, _A, _B, _C, _D};

#[derive(Clone, Copy)]
enum Key {
            _1,
        _2, _3, _4,
    _5, _6, _7, _8, _9,
        _A, _B, _C,
            _D
}

impl Into<char> for Key {
    fn into(self) -> char {
        match self {
            _1 => '1',
            _2 => '2',
            _3 => '3',
            _4 => '4',
            _5 => '5',
            _6 => '6',
            _7 => '7',
            _8 => '8',
            _9 => '9',
            _A => 'A',
            _B => 'B',
            _C => 'C',
            _D => 'D',

        }
    }
}

struct Pos { k: Key }

impl Pos {
    fn left(&mut self) {
        self.k = match self.k {
            _1 => _1,
            _2 => _2,
            _3 => _2,
            _4 => _3,
            _5 => _5,
            _6 => _5,
            _7 => _6,
            _8 => _7,
            _9 => _8,
            _A => _A,
            _B => _A,
            _C => _B,
            _D => _D,
        }
    }
    fn up(&mut self) {
        self.k = match self.k {
            _1 => _1,
            _2 => _2,
            _3 => _1,
            _4 => _4,
            _5 => _5,
            _6 => _2,
            _7 => _3,
            _8 => _4,
            _9 => _9,
            _A => _6,
            _B => _7,
            _C => _8,
            _D => _B,
        }
    }
    fn right(&mut self) {
        self.k = match self.k {
            _1 => _1,
            _2 => _3,
            _3 => _4,
            _4 => _4,
            _5 => _6,
            _6 => _7,
            _7 => _8,
            _8 => _9,
            _9 => _9,
            _A => _B,
            _B => _C,
            _C => _C,
            _D => _D,
        }
    }
    fn down(&mut self) {
        self.k = match self.k {
            _1 => _3,
            _2 => _6,
            _3 => _7,
            _4 => _8,
            _5 => _5,
            _6 => _A,
            _7 => _B,
            _8 => _C,
            _9 => _9,
            _A => _A,
            _B => _D,
            _C => _C,
            _D => _D,
        }
    }
}

pub fn solve() {
    let mut pos = Pos { k: _5 };
    let input: &str = include_str!("2016-02.txt");
    let mut numbers: Vec<char> = Vec::new();
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

