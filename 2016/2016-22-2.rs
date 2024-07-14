use std::cmp::max;
use regex::Regex;

const MAX_X: usize = 50; // you can increase these values to fit larger puzzle inputs
const MAX_Y: usize = 50; // they can be larger than the input, just not smaller

type Steps = usize;
type Position = (usize, usize);
type Empty = Position;
type Target = Position;
type Grid = [[bool; MAX_Y]; MAX_X];
type GridContext = (Grid, Empty, Target);

struct Node { x: usize, y: usize, size: u16, used: u16 }

impl Node {

    fn available(&self) -> u16 {
        self.size - self.used
    }

    fn is_viable_pair(&self, other: &Node) -> bool {
        self.used > 0
            && !(self.x == other.x && self.y == other.y)
            && self.used <= other.available()
    }

}

fn parse_instruction(line: &str) -> Node {
    let re = Regex::new(r"/dev/grid/node-x([0-9]+)-y([0-9]+)\s+([0-9]+)T\s+([0-9]+)T\s+[0-9]+T\s+[0-9]+%").unwrap();
    match re.captures(line) {
        Some(capture) => Node {
            x: capture.get(1).unwrap().as_str().parse::<usize>().unwrap(),
            y: capture.get(2).unwrap().as_str().parse::<usize>().unwrap(),
            size: capture.get(3).unwrap().as_str().parse::<u16>().unwrap(),
            used: capture.get(4).unwrap().as_str().parse::<u16>().unwrap(),
        },
        None => panic!("can't parse line: {}", line)
    }
}

fn find_path_to_target(grid_context: GridContext) -> (GridContext, Steps) {
    let (grid, empty, target) = grid_context;
    let mut visited: Vec<Position> = Vec::new();
    let mut positions: Vec<Position> = Vec::new();
    visited.push(empty);
    positions.push(empty);
    let mut steps: Steps = 0;
    loop {
        let mut next_positions: Vec<Position> = Vec::new();
        while let Some(position) = positions.pop() {
            if position == target {
                return ((grid, position, (position.0 + 1, position.1)), steps)
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
            if grid[c.0][c.1] {
                if !visited.iter().any(|v| v == candidate) {
                    visited.push(candidate.to_owned());
                    positions.push(candidate.to_owned())
                }
            }
        }
        steps += 1;
    }
}

fn build_grid(nodes: &Vec<Node>) -> GridContext {
    let mut grid: Grid = [[false; MAX_Y]; MAX_X];
    let mut empty: Position = (0, 0);
    let mut max_x = 0;
    for node in nodes.iter().clone() {
        grid[node.x][node.y] = if node.used == 0 {
            empty = (node.x, node.y);
            true
        } else {
            nodes.iter().clone().fold(false, |acc, n| acc || node.is_viable_pair(n))
        };
        max_x = max(node.x, max_x);
    }
    let target: Position = (max_x - 1, 0);
    grid[target.0 + 1][target.1] = false;
    (grid, empty, target)
}

fn find_solution(nodes: &Vec<Node>) -> Steps {
    let grid_context = build_grid(nodes);
    let ((mut grid, mut empty, mut target), mut steps) = find_path_to_target(grid_context);
    while empty != (0, 0) {
        target = (target.0 - 1, target.1);
        empty = (empty.0 + 1, empty.1);
        grid[empty.0][empty.1] = true;
        grid[target.0][target.1] = false;
        let steps_so_far = steps;
        ((grid, empty, target), steps) = find_path_to_target((grid, empty, (target.0 - 1, target.1)));
        steps += steps_so_far + 1;
    }
    steps + 1
}

pub fn solve() {
    let input: Vec<&str> = include_str!("2016-22.txt").split("\n").collect();
    let nodes: Vec<Node> = input.iter().skip(2).map(|line| parse_instruction(line)).collect();
    println!("{}\n", find_solution(&nodes));
}