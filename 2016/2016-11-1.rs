use std::cmp::PartialEq;
use std::collections::HashSet;
use regex::Regex;
use crate::s_2016_11_1::Direction::{Down, Up};
use crate::s_2016_11_1::Element::{Cobalt, Curium, Plutonium, Promethium, Ruthenium};
use crate::s_2016_11_1::Floor::{F1,F2,F3,F4};
use crate::s_2016_11_1::Status::{Completed, Invalid, Valid};
use crate::s_2016_11_1::Type::{Generator, Microchip};

#[derive(Clone, Copy, Eq, Hash, PartialEq, Debug)]
enum Element { Cobalt, Curium, Plutonium, Promethium, Ruthenium }
#[derive(Clone, Eq, PartialEq)]
enum Status { Valid, Invalid, Completed }
#[derive(Clone, Eq, PartialEq)]
enum Floor { F1, F2, F3, F4 }
#[derive(Clone, Copy, Eq, PartialEq)]
enum Direction { Up, Down }
#[derive(Clone, Copy, Eq, Hash, PartialEq, Debug)]
enum Type { Generator, Microchip }

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
struct Item {
    element: Element,
    item_type: Type
}

#[derive(Clone, Copy, Eq, PartialEq)]
struct Action {
    direction: Direction,
    item: Item,
    optional_item: Option<Item>
}

#[derive(Clone, Eq, PartialEq)]
struct Area {
    elevator_stop: Floor,
    first_floor: HashSet<Item>,
    second_floor: HashSet<Item>,
    third_floor: HashSet<Item>,
    fourth_floor: HashSet<Item>,
}

impl Area {
    fn move_elevator(&mut self, direction: Direction) {
        match direction {
            Up => match self.elevator_stop {
                F1 => self.elevator_stop = F2,
                F2 => self.elevator_stop = F3,
                F3 => self.elevator_stop = F4,
                F4 => panic!("can't go up from top floor"),
            },
            Down => match self.elevator_stop {
                F1 => panic!("can't go down from ground floor"),
                F2 => self.elevator_stop = F1,
                F3 => self.elevator_stop = F2,
                F4 => self.elevator_stop = F3,
            },
        }
    }

    fn current_floor(&mut self) -> &mut HashSet<Item> {
        match self.elevator_stop {
            F1 => &mut self.first_floor,
            F2 => &mut self.second_floor,
            F3 => &mut self.third_floor,
            F4 => &mut self.fourth_floor,
        }
    }

    fn next_floor(&mut self, direction: &Direction) -> &mut HashSet<Item> {
        match (direction, &self.elevator_stop) {
            (Up, F1) => &mut self.second_floor,
            (Up, F2) => &mut self.third_floor,
            (Up, F3) => &mut self.fourth_floor,
            (Up, F4) => panic!("can't go up from top floor"),
            (Down, F1) => panic!("can't go down from ground floor"),
            (Down, F2) => &mut self.first_floor,
            (Down, F3) => &mut self.second_floor,
            (Down, F4) => &mut self.third_floor,
        }
    }

    fn code(&self) -> u32 {
        let mut combinations: Vec<u8> = Vec::new();
        for element in ELEMENTS {
            let mut combination: u8 = self.find_pos(element, Microchip);
            combination += self.find_pos(element, Generator) * 10;
            combinations.push(match combination {
                11 => 0,
                12 => 1,
                13 => 2,
                14 => 3,
                21 => 4,
                22 => 5,
                23 => 6,
                24 => 7,
                31 => 8,
                32 => 9,
                33 => 10,
                34 => 11,
                41 => 12,
                42 => 13,
                43 => 14,
                44 => 15,
                _ => panic!("unexpected combination: {}", combination)
            });
        }
        let prime_base: u32 = 17;
        let mut hash: u32 = 0;
        combinations.sort_unstable();
        combinations.iter().for_each(|c| hash = hash * prime_base + *c as u32);
        hash = hash * prime_base + match self.elevator_stop {
            F1 => 1,
            F2 => 2,
            F3 => 3,
            F4 => 4,
        };
        hash
    }

    fn find_pos(&self, element: Element, item_type: Type) -> u8 {
        if self.third_floor.iter().any(|&item| item.element == element && item.item_type == item_type) {
            return 3
        } else if self.second_floor.iter().any(|&item| item.element == element && item.item_type == item_type) {
            return 2
        } else if self.fourth_floor.iter().any(|&item| item.element == element && item.item_type == item_type) {
            return 4
        } else if self.first_floor.iter().any(|&item| item.element == element && item.item_type == item_type) {
            return 1
        } else {
            panic!("item not found! element = {:?} type = {:?}", element, item_type);
        }
    }
}

#[derive(Clone, Eq, PartialEq)]
struct State {
    area: Area,
    actions: Vec<Action>,
    status: Status,
}

impl State {
    fn is_valid(&self) -> bool {
        self.status == Valid
    }

    fn is_completed(&self) -> bool {
        self.status == Completed
    }

    fn path_len(&self) -> usize {
        self.actions.len()
    }
}

static DIRECTIONS: [Direction; 2] = [Up, Down];
static ELEMENTS: [Element; 5] = [Cobalt, Curium, Plutonium, Promethium, Ruthenium];

fn find_solution(state: State) -> State {
    let mut area_codes: HashSet<u32> = HashSet::new();
    let mut states: Vec<State> = Vec::new();
    states.push(state);
    loop {
        let mut next_states: Vec<State> = Vec::new();
        while let Some(state) = states.pop() {
            let items = match state.area.elevator_stop {
                F1 => &state.area.first_floor,
                F2 => &state.area.second_floor,
                F3 => &state.area.third_floor,
                F4 => &state.area.fourth_floor,
            };
            for direction in DIRECTIONS {
                if state.area.elevator_stop == F1 && direction == Down {
                    continue;
                } else if state.area.elevator_stop == F4 && direction == Up {
                    continue;
                } else if state.area.elevator_stop == F2 && state.area.first_floor.is_empty() && direction == Down {
                    continue;
                } else if state.area.elevator_stop == F3 && state.area.first_floor.is_empty() && state.area.second_floor.is_empty() && direction == Down {
                    continue;
                }
                for item in items.iter() {
                    for optional_item in items.iter() {
                        if item == optional_item {
                            continue
                        }
                        let next_state = create_next_state(&mut state.clone(), Action {
                            direction: direction.clone(),
                            item: item.clone(),
                            optional_item: Some(optional_item.to_owned()),
                        });
                        if next_state.is_valid() {
                            next_states.push(next_state);
                        } else if next_state.is_completed() {
                            return next_state;
                        }
                    }
                    let next_state = create_next_state(&mut state.clone(), Action {
                        direction: direction.clone(),
                        item: item.clone(),
                        optional_item: None,
                    });
                    if next_state.is_valid() {
                        next_states.push(next_state);
                    } else if next_state.is_completed() {
                        return next_state;
                    }
                }
            };
        }
        for state in next_states.iter() {
            let ac = state.area.code();
            let contains = area_codes.iter().any(|c| c == &ac);
            if !contains {
                area_codes.insert(ac);
                states.push(state.to_owned());
            }
        };
    }
}

fn create_next_state(state: &mut State, next_action: Action) -> State {
    state.area.current_floor().retain(|&item| item != next_action.item);
    state.area.next_floor(&next_action.direction).insert(next_action.item);
    if let Some(second_item) = next_action.optional_item {
        state.area.current_floor().retain(|&item| item != second_item);
        state.area.next_floor(&next_action.direction).insert(second_item);
    }
    state.status = if has_fried_chip(&state.area) { Invalid } else { Valid };
    if state.is_valid() && state.area.first_floor.is_empty() && state.area.second_floor.is_empty() && state.area.third_floor.is_empty() {
        state.status = Completed;
    }
    state.area.move_elevator(next_action.direction);
    state.actions.push(next_action.to_owned());
    state.to_owned()
}

fn has_fried_chip(area: &Area) -> bool {
    area.first_floor.iter().any(|&item| is_fried_chip(&item, &area.first_floor))
        || area.second_floor.iter().any(|&item| is_fried_chip(&item, &area.second_floor))
        || area.third_floor.iter().any(|&item| is_fried_chip(&item, &area.third_floor))
        || area.fourth_floor.iter().any(|&item| is_fried_chip(&item, &area.fourth_floor))
}

fn is_fried_chip(item: &Item, floor_items: &HashSet<Item>) -> bool {
    if item.item_type == Microchip {
        let corresponding_generator = floor_items.iter().find(|&&other| {
            other.item_type == Generator && other.element == item.element
        });
        match corresponding_generator {
            Some(_gen) => false,
            None => floor_items.iter().any(|&other| other.item_type == Generator),
        }
    } else {
        false
    }
}

fn create_area_from_input() -> Area {
    let mut area = Area {
        elevator_stop: F1,
        first_floor: HashSet::with_capacity(10),
        second_floor: HashSet::with_capacity(10),
        third_floor: HashSet::with_capacity(10),
        fourth_floor: HashSet::with_capacity(10),
    };
    let lines: Vec<&str> = include_str!("2016-11.txt").split("\n").collect();
    for line in lines {
        let set_ref: &mut HashSet<Item>;
        let re_floor = Regex::new(r"The (first|second|third|fourth) floor.*").unwrap();
        match re_floor.captures(line) {
            Some(capture) => set_ref = match capture.get(1).unwrap().as_str() {
                "first" => &mut area.first_floor,
                "second" => &mut area.second_floor,
                "third" => &mut area.third_floor,
                "fourth" => &mut area.fourth_floor,
                _ => panic!("can't determine floor from input: {}", line),
            },
            None => panic!("can't determine floor from input: {}", line)
        }
        let re_items = Regex::new(r"(\w+)(-compatible microchip| generator)").unwrap();
        for cap in re_items.captures_iter(line) {
            set_ref.insert(Item {
                element: match cap[1].to_string().as_str() {
                    "cobalt" => Cobalt,
                    "curium" => Curium,
                    "plutonium" => Plutonium,
                    "promethium" => Promethium,
                    "ruthenium" => Ruthenium,
                    _ => panic!("can't determine element from input: {}", cap[1].to_string().as_str())
                },
                item_type: if cap[2].to_string() == " generator" { Generator } else { Microchip }
            });
        }
    }
    area
}

pub fn solve() {
    let shortest = find_solution(State {
        area: create_area_from_input(),
        actions: vec![],
        status: Valid,
    });
    println!("{}", shortest.path_len());
}
