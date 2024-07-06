const ROWS: usize = 400000;
const COLS: usize = 100;

fn val_or_panic(row: &Vec<bool>, index: usize) -> bool {
    row.get(index).unwrap_or_else(|| panic!("could not read index {} from row", index)) == &true
}

fn next_row(row: &Vec<bool>) -> Vec<bool> {
    let mut result: Vec<bool> = Vec::new();
    result.push(val_or_panic(row, 1));
    for i in 1..=COLS-2 {
        result.push(val_or_panic(row, i-1) ^ val_or_panic(row, i+1))
    }
    result.push(val_or_panic(row, COLS-2));
    result
}

fn initial_row(input: &str) -> Vec<bool> {
    input.chars().map(|c| c == '^').collect()
}

fn trap_count(row: &Vec<bool>) -> usize {
    row.iter().filter(|b| !**b).count()
}

pub fn solve() {
    let input = include_str!("2016-18.txt");
    let initial_row = initial_row(input);
    let mut sum = trap_count(&initial_row);
    let mut row = initial_row;
    for _ in 2..=ROWS {
        let next_row = next_row(&row);
        sum += trap_count(&next_row);
        row = next_row;
    }
    print!("{}\n", sum);
}
