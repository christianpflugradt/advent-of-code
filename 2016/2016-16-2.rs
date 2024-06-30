const DISC_LENGTH: usize = 35651584;

fn apply_modified_dragon_curve(data: String) -> String {
    let reverse: String = data.chars().rev().map(|c| match c {
        '1' => '0',
        '0' => '1',
        _ => panic!("unexpected character: {}", c)
    }).collect();
    format!("{}0{}", data, reverse)
}

fn checksum(data: String) -> String {
    let mut data: Vec<char> = data[0..DISC_LENGTH].chars().collect();
    while data.len() % 2 == 0 {
        let mut next: Vec<char> = Vec::new();
        for (i, _) in data.iter().enumerate().step_by(2) {
            next.push(if data[i] == data[i+1] { '1' } else { '0' });
        }
        data = next;
    }
    data.iter().collect()
}

pub fn solve() {
    let input = "11011110011011101";
    let mut data = input.to_string();
    while data.len() < DISC_LENGTH {
        data = apply_modified_dragon_curve(data.to_string());
    }
    print!("{}\n", checksum(data))
}
