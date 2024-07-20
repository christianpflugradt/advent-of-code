const MAX_X: usize = 200; // you can increase these values to fit larger puzzle inputs
const MAX_Y: usize = 50; // they can be larger than the input, just not smaller

type Grid = [[bool; MAX_Y]; MAX_X];
type Position = (u16, u16);
type Segment = (Position, Position, usize);
type Path = (Vec<Position>, usize);

fn find_solution(grid: Grid, start: Position, target: Position) -> usize {
    let mut visited: Vec<Position> = Vec::new();
    let mut positions: Vec<Position> = Vec::new();
    positions.push(start);
    visited.push(start);
    let mut steps: usize = 0;
    loop {
        let mut next_positions: Vec<Position> = Vec::new();
        while let Some(pos) = positions.pop() {
            let mut directions = vec![(1, 0), (0, 1)];
            if pos.0 > 0 { directions.push((-1, 0)); }
            if pos.1 > 0 { directions.push((0, -1)); }
            for direction in directions {
                if let Some(next_pos) = new_pos(grid, pos, direction.0, direction.1) {
                    next_positions.push(next_pos);
                }
            }
        }
        for candidate in next_positions.iter() {
            if candidate == &target {
                return steps + 1
            }
            if !positions.contains(candidate) {
                positions.push(candidate.to_owned());
            }
            visited.push(candidate.to_owned());
        }
        steps += 1;
    }
}

fn new_pos(grid: Grid, pos: Position, x_offset: i16, y_offset: i16) -> Option<Position> {
    let (x, y) = pos;
    let new_x = (x as i16 + x_offset) as u16;
    let new_y = (y as i16 + y_offset) as u16;
    if grid[new_x as usize][new_y as usize] {
        Some((new_x, new_y))
    } else {
        None
    }
}

fn build_grid(lines: &Vec<&str>) -> (Grid, Position, Vec<Position>) {
    let mut grid: Grid = [[false; MAX_Y]; MAX_X];
    let mut start: Position = (0, 0);
    let mut positions: Vec<Position> = Vec::new();
    let mut y: u16 = 0;
    for line in lines {
        let mut x: u16 = 0;
        for char in line.chars() {
            match char {
                '#' => {},
                '.' => grid[x as usize][y as usize] = true,
                n => {
                    grid[x as usize][y as usize] = true;
                    if n != '0' {
                        positions.push((x, y));
                    } else {
                        start = (x, y);
                    }
                }
            }
            x += 1;
        }
        y += 1;
    }
    (grid, start, positions)
}

fn get_all_segments(context: (Grid, Position, Vec<Position>)) -> (Position, Vec<Segment>) {
    let (grid, start, positions) = context;
    let mut paths: Vec<(Position, Position, usize)> = Vec::new();
    for from in positions.clone() {
        let distance = find_solution(grid, start, from);
        paths.push((start, from, distance));
        paths.push((from, start, distance));
        for to in positions.clone() {
            let present = paths.iter()
                .any(|(a, b, _)| from == to || (a == &from && b == &to) || (a == &to && b == &from));
            if !present {
                let distance = find_solution(grid, from, to);
                paths.push((from, to, distance));
                paths.push((to, from, distance));
            }
        }
    }
    return (start, paths)
}

fn get_all_paths(context: (Position, Vec<Segment>)) -> Vec<Path> {
    let paths = add_positions(&context.1, vec![context.0], 0);
    let max = paths.iter().max_by(|p, other| p.0.len().cmp(&other.0.len()))
        .unwrap_or_else(|| panic!("expected at least one path in paths")).0.len();
    paths.iter().filter(|p| p.0.len() == max)
        .filter(|p| p.0.first() == p.0.last())
        .map(|p| p.to_owned()).collect()
}

fn min_path(paths: Vec<Path>) -> usize {
    paths.iter().map(|p| p.1).min().unwrap_or(0)
}

fn add_positions(segments: &Vec<Segment>, path: Vec<Position>, length: usize) -> Vec<Path> {
    let mut result: Vec<Path> = Vec::new();
    let position = path.last().unwrap_or_else(|| panic!("expected at least one position in path"));
    for segment in segments {
        if &segment.0 == position {
            if !path[1..].contains(&segment.1) {
                let mut new_path = path.clone();
                let new_length = length + segment.2;
                new_path.push(segment.1);
                result.push((new_path.clone(), new_length));
                result.append(&mut add_positions(segments, new_path, new_length));
            }
        }
    }
    result
}

pub fn solve() {
    let input: Vec<&str> = include_str!("2016-24.txt").split("\n").collect();
    println!("{}\n", min_path(get_all_paths(get_all_segments(build_grid(&input)))));
}