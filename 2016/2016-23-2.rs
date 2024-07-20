use regex::Regex;
use crate::s_2016_23_2::Instruction::{CpyInt, CpyReg, Dec, Inc, JnzInt, JnzReg1st, JnzReg2nd, Tgl};

#[derive(Clone)]
struct IncInstr { register: char }
#[derive(Clone)]
struct DecInstr { register: char }
#[derive(Clone)]
struct CpyRegInstr { source: char, register: char }
#[derive(Clone)]
struct CpyIntInstr { value: i32, register: char }
#[derive(Clone)]
struct JnzReg1stInstr { source: char, steps: i32 }
#[derive(Clone)]
struct JnzReg2ndInstr { value: i32, target: char }
#[derive(Clone)]
struct JnzIntInstr { value: i32, steps: i32 }
#[derive(Clone, Debug)]
struct TglInstr { register: char }

enum Instruction {
    Inc(IncInstr),
    Dec(DecInstr),
    CpyReg(CpyRegInstr),
    CpyInt(CpyIntInstr),
    JnzReg1st(JnzReg1stInstr),
    JnzReg2nd(JnzReg2ndInstr),
    JnzInt(JnzIntInstr),
    Tgl(TglInstr),
}

struct Assembler {
    instructions: Vec<Instruction>,
    toggles: Vec<bool>,
    pointer: usize,
    reg_a: i32,
    reg_b: i32,
    reg_c: i32,
    reg_d: i32,
}

impl Clone for Instruction {
    fn clone(&self) -> Self {
        match self {
            Inc(it) => Inc(it.clone()),
            Dec(it) => Dec(it.clone()),
            CpyReg(it) => CpyReg(it.clone()),
            CpyInt(it) => CpyInt(it.clone()),
            JnzReg1st(it) => JnzReg1st(it.clone()),
            JnzReg2nd(it) => JnzReg2nd(it.clone()),
            JnzInt(it) => JnzInt(it.clone()),
            Tgl(it) => Tgl(it.clone()),
        }
    }
}

impl Assembler {

    fn step(&mut self) -> bool {
        let address = self.instructions.get(self.pointer).cloned();
        let current = self.pointer.to_owned();
        if let Some(instruction) = address {
            if let Some(toggle) = self.toggles.get(self.pointer) {
                match instruction {
                    Inc(it) => if *toggle { self.dec(it.register) } else { self.inc(it.register) },
                    Dec(it) => if *toggle { self.inc(it.register) } else { self.dec(it.register) },
                    CpyReg(it) => if *toggle { self.jnz_reg_3rd(it.register, it.source) } else { self.cpy_reg(it.register, it.source) },
                    CpyInt(it) => if *toggle { self.jnz_reg_2nd(it.value, it.register) } else { self.cpy_int(it.value, it.register) },
                    JnzReg1st(it) => if *toggle { /* invalid */ } else { self.jnz_reg_1st(it.source, it.steps) },
                    JnzReg2nd(it) => if *toggle { self.cpy_int(it.value, it.target) } else { self.jnz_reg_2nd(it.value, it.target) },
                    JnzInt(it) => if *toggle { /* invalid */ } else { self.jnz_int(it.steps, it.value) },
                    Tgl(it) => if *toggle { self.inc(it.register) } else { self.tgl(it.register) },
                }
                if self.pointer == current {
                    self.pointer += 1;
                }
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn inc(&mut self, register: char) {
        self.set_register(register, self.get_register(register) + 1);
    }

    fn dec(&mut self, register: char) {
        self.set_register(register, self.get_register(register) - 1);
    }

    fn cpy_reg(&mut self, register: char, source: char) {
        self.set_register(register, self.get_register(source));
    }

    fn cpy_int(&mut self, value: i32, register: char) {
        self.set_register(register, value);
    }

    fn jnz_reg_1st(&mut self, source: char, steps: i32) {
        self.jnz_int(self.get_register(source), steps);
    }

    fn jnz_reg_2nd(&mut self, value: i32, target: char) {
        self.jnz_int(value, self.get_register(target));
    }

    fn jnz_reg_3rd(&mut self, source: char, target: char) {
        self.jnz_int(self.get_register(source), self.get_register(target))
    }

    fn jnz_int(&mut self, value: i32, steps: i32) {
        if value != 0 {
            self.pointer = (self.pointer as i32 + steps) as usize
        }
    }

    fn tgl(&mut self, register: char) {
        let index: usize = (self.pointer as i32 + self.get_register(register)) as usize;
        if let Some(toggle) = self.toggles.get(index) {
            self.toggles[index] = !toggle;
        }
    }

    fn get_register(&self, x: char) -> i32 {
        match x {
            'a' => self.reg_a,
            'b' => self.reg_b,
            'c' => self.reg_c,
            'd' => self.reg_d,
            _ => panic!("can't resolve register: {}", x)
        }
    }

    fn set_register(&mut self, x: char, y: i32) {
        match x {
            'a' => self.reg_a = y,
            'b' => self.reg_b = y,
            'c' => self.reg_c = y,
            'd' => self.reg_d = y,
            _ => panic!("can't resolve register: {}", x)
        }
    }
}

fn match_inc(line: &str) -> Option<IncInstr> {
    let re = Regex::new(r"inc ([abcd])").unwrap();
    match re.captures(line) {
        Some(capture) => Some(IncInstr {
            register: capture.get(1).unwrap().as_str().chars().next().unwrap()
        }),
        None => None
    }
}

fn match_dec(line: &str) -> Option<DecInstr> {
    let re = Regex::new(r"dec ([abcd])").unwrap();
    match re.captures(line) {
        Some(capture) => Some(DecInstr {
            register: capture.get(1).unwrap().as_str().chars().next().unwrap()
        }),
        None => None
    }
}

fn match_cpy_reg(line: &str) -> Option<CpyRegInstr> {
    let re = Regex::new(r"cpy ([abcd]) ([abcd])").unwrap();
    match re.captures(line) {
        Some(capture) => Some(CpyRegInstr {
            source: capture.get(1).unwrap().as_str().chars().next().unwrap(),
            register: capture.get(2).unwrap().as_str().chars().next().unwrap(),
        }),
        None => None
    }
}

fn match_cpy_int(line: &str) -> Option<CpyIntInstr> {
    let re = Regex::new(r"cpy (-?[0-9]+) ([abcd])").unwrap();
    match re.captures(line) {
        Some(capture) => Some(CpyIntInstr {
            value: capture.get(1).unwrap().as_str().parse:: < i32>().unwrap(),
            register: capture.get(2).unwrap().as_str().chars().next().unwrap(),
        }),
        None => None
    }
}

fn match_jnz_reg_1st(line: &str) -> Option<JnzReg1stInstr> {
    let re = Regex::new(r"jnz ([abcd]) (-?[0-9]+)").unwrap();
    match re.captures(line) {
        Some(capture) => Some(JnzReg1stInstr {
            source: capture.get(1).unwrap().as_str().chars().next().unwrap(),
            steps: capture.get(2).unwrap().as_str().parse:: < i32>().unwrap(),
        }),
        None => None
    }
}

fn match_jnz_reg_2nd(line: &str) -> Option<JnzReg2ndInstr> {
    let re = Regex::new(r"jnz (-?[0-9]+) ([abcd])").unwrap();
    match re.captures(line) {
        Some(capture) => Some(JnzReg2ndInstr {
            value: capture.get(1).unwrap().as_str().parse:: < i32>().unwrap(),
            target: capture.get(2).unwrap().as_str().chars().next().unwrap(),
        }),
        None => None
    }
}

fn match_jnz_int(line: &str) -> Option<JnzIntInstr> {
    let re = Regex::new(r"jnz (-?[0-9]+) (-?[0-9]+)").unwrap();
    match re.captures(line) {
        Some(capture) => Some(JnzIntInstr {
            value: capture.get(1).unwrap().as_str().parse:: < i32>().unwrap(),
            steps: capture.get(2).unwrap().as_str().parse:: < i32>().unwrap(),
        }),
        None => None
    }
}

fn match_tgl(line: &str) -> Option<TglInstr> {
    let re = Regex::new(r"tgl ([abcd])").unwrap();
    match re.captures(line) {
        Some(capture) => Some(TglInstr {
            register: capture.get(1).unwrap().as_str().chars().next().unwrap()
        }),
        None => None
    }
}

pub fn solve() {
    let input: Vec<&str> = include_str!("2016-23.txt").split("\n").collect();
    let instructions: Vec<Instruction> = input.iter().map(|line| {
        if let Some(result) = match_inc(&line) {
            Inc(result)
        } else if let Some(result) = match_dec(&line) {
            Dec(result)
        } else if let Some(result) = match_cpy_reg(&line) {
            CpyReg(result)
        } else if let Some(result) = match_cpy_int(&line) {
            CpyInt(result)
        } else if let Some(result) = match_jnz_reg_1st(&line) {
            JnzReg1st(result)
        } else if let Some(result) = match_jnz_reg_2nd(&line) {
            JnzReg2nd(result)
        } else if let Some(result) = match_jnz_int(&line) {
            JnzInt(result)
        } else if let Some(result) = match_tgl(&line) {
            Tgl(result)
        } else {
            panic!("can't parse line: {}", line)
        }
    }).collect();
    let length = instructions.len();
    let mut assembler = Assembler {
        instructions,
        toggles: vec![false; length],
        pointer: 0,
        reg_a: 12,
        reg_b: 0,
        reg_c: 0,
        reg_d: 0,
    };
    let mut unprocessed_steps = true;
    while unprocessed_steps {
        unprocessed_steps = assembler.step();
    }
    print!("{}\n", assembler.reg_a)
}
