use regex::Regex;
use crate::s_2016_25_1::Instruction::{CpyInt, CpyReg, Dec, Inc, JnzInt, JnzReg, Out};

const MIN_SEQ_LEN: usize = 100;

#[derive(Clone)]
struct IncInstr { register: char }
#[derive(Clone)]
struct DecInstr { register: char }
#[derive(Clone)]
struct CpyRegInstr { source: char, register: char }
#[derive(Clone)]
struct CpyIntInstr { value: i32, register: char }
#[derive(Clone)]
struct JnzRegInstr { source: char, steps: i32 }
#[derive(Clone)]
struct JnzIntInstr { value: i32, steps: i32 }
#[derive(Clone)]
struct OutInstr { register: char }

enum Instruction {
    Inc(IncInstr),
    Dec(DecInstr),
    CpyReg(CpyRegInstr),
    CpyInt(CpyIntInstr),
    JnzReg(JnzRegInstr),
    JnzInt(JnzIntInstr),
    Out(OutInstr),
}

struct Assembler {
    signal: Vec<bool>,
    instructions: Vec<Instruction>,
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
            JnzReg(it) => JnzReg(it.clone()),
            JnzInt(it) => JnzInt(it.clone()),
            Out(it) => Out(it.clone())
        }
    }
}

impl Assembler {

    fn step(&mut self) {
        let address = self.instructions.get(self.pointer).cloned();
        let current = self.pointer.to_owned();
        if let Some(instruction) = address {
            match instruction {
                Inc(it) => self.inc(it),
                Dec(it) => self.dec(it),
                CpyReg(it) => self.cpy_reg(it),
                CpyInt(it) => self.cpy_int(it),
                JnzReg(it) => self.jnz_reg(it),
                JnzInt(it) => self.jnz_int(it),
                Out(it) => self.out(it),
            }
            if self.pointer == current {
                self.pointer += 1;
            }
        }
    }

    fn valid_signal(&self) -> bool {
        let (left, right): (Vec<_>, Vec<_>) = self.signal.iter().enumerate().partition(|&(i, _)| i % 2 == 0);
        let all_left_true = left.clone().into_iter().all(|e| !!e.1);
        let all_left_false = left.clone().into_iter().all(|e| !e.1);
        let all_right_true = right.clone().into_iter().all(|e| !!e.1);
        let all_right_false = left.clone().into_iter().all(|e| !e.1);
        (all_left_true && all_right_false) || (all_left_false && all_right_true)
    }

    fn inc(&mut self, inc: IncInstr) {
        self.set_register(inc.register, self.get_register(inc.register) + 1);
    }

    fn dec(&mut self, dec: DecInstr) {
        self.set_register(dec.register, self.get_register(dec.register) - 1);
    }

    fn cpy_reg(&mut self, cpy_reg: CpyRegInstr) {
        self.set_register(cpy_reg.register, self.get_register(cpy_reg.source));
    }

    fn cpy_int(&mut self, cpy_int: CpyIntInstr) {
        self.set_register(cpy_int.register, cpy_int.value);
    }

    fn jnz_reg(&mut self, jnz_reg: JnzRegInstr) {
        self.jnz_int(JnzIntInstr { value: self.get_register(jnz_reg.source), steps: jnz_reg.steps });
    }

    fn jnz_int(&mut self, jnz_int: JnzIntInstr) {
        if jnz_int.value != 0 {
            self.pointer = (self.pointer as i32 + jnz_int.steps) as usize
        }
    }

    fn out(&mut self, out: OutInstr) {
        match self.get_register(out.register) {
            0 => self.signal.push(false),
            1 => self.signal.push(true),
            n => panic!("expected 0 or 1 but got: {}", n)
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

fn match_jnz_reg(line: &str) -> Option<JnzRegInstr> {
    let re = Regex::new(r"jnz ([abcd]) (-?[0-9]+)").unwrap();
    match re.captures(line) {
        Some(capture) => Some(JnzRegInstr {
            source: capture.get(1).unwrap().as_str().chars().next().unwrap(),
            steps: capture.get(2).unwrap().as_str().parse:: < i32>().unwrap(),
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

fn match_out(line: &str) -> Option<OutInstr> {
    let re = Regex::new(r"out ([abcd])").unwrap();
    match re.captures(line) {
        Some(capture) => Some(OutInstr {
            register: capture.get(1).unwrap().as_str().chars().next().unwrap()
        }),
        None => None
    }
}

pub fn solve() {
    let input: Vec<&str> = include_str!("2016-25.txt").split("\n").collect();
    let instructions: Vec<Instruction> = input.iter().map(|line| {
        if let Some(result) = match_inc(&line) {
            return Inc(result)
        } else if let Some(result) = match_dec(&line) {
            return Dec(result)
        } else if let Some(result) = match_cpy_reg(&line) {
            return CpyReg(result)
        } else if let Some(result) = match_cpy_int(&line) {
            return CpyInt(result)
        } else if let Some(result) = match_jnz_reg(&line) {
            return JnzReg(result)
        } else if let Some(result) = match_jnz_int(&line) {
            return JnzInt(result)
        } else if let Some(result) = match_out(&line) {
            return Out(result)
        } else {
            panic!("can't parse line: {}", line)
        }
    }).collect();
    let mut initial_value = 0;
    loop {
        let mut assembler = Assembler {
            signal: Vec::new(),
            instructions: instructions.clone(),
            pointer: 0,
            reg_a: initial_value,
            reg_b: 0,
            reg_c: 0,
            reg_d: 0,
        };
        while assembler.signal.len() < MIN_SEQ_LEN {
            assembler.step();
        }
        if assembler.valid_signal() {
            break;
        }
        initial_value += 1
    }
    print!("{}\n", initial_value)
}
