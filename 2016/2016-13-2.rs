type Position = (usize, usize);

const INPUT: u16 = 1352;

fn is_open_space(x: u16, y: u16) -> bool {
    format!("{:016b}", x*x + 3*x + 2*x*y + y + y*y + INPUT).chars()
        .filter(|c| *c == '1')
        .count() % 2 == 0
}

fn find_solution() -> usize {
    let mut visited: Vec<Position> = Vec::new();
    let mut positions: Vec<Position> = Vec::new();
    visited.push((1,1));
    positions.push((1,1));
    let mut steps: usize = 0;
    loop {
        let mut next_positions: Vec<Position> = Vec::new();
        while let Some(position) = positions.pop() {
            if steps == 50 {
                return visited.iter().count();
            }
            if position.0 > 0 {
                next_positions.push((position.0 - 1, position.1));
            }
            if position.1 > 0 {
                next_positions.push((position.0, position.1 - 1));
            }
            next_positions.push((position.0 + 1, position.1));
            next_positions.push((position.0, position.1 + 1));
        }
        for candidate in next_positions.iter() {
            let c = candidate.to_owned();
            if is_open_space(c.0 as u16, c.1 as u16) {
                if !visited.iter().any(|v| v == candidate) {
                    visited.push(candidate.to_owned());
                    positions.push(candidate.to_owned())
                }
            }
        }
        steps += 1;
    }
}

pub fn solve() {
    print!("{}\n", find_solution())
}
