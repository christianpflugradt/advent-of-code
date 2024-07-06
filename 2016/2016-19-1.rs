use std::collections::VecDeque;

fn get_last_elf(mut elves: VecDeque<usize>) -> usize {
    while elves.len() > 1 {
        let elf = elves.pop_front().unwrap();
        elves.push_back(elf);
        elves.pop_front();
    }
    elves[0]
}

pub fn solve() {
    let input = 3018458;
    println!("{}", get_last_elf((1..=input).collect()));
}
