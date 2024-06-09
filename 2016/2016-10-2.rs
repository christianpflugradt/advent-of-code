use std::cmp::PartialEq;
use regex::Regex;
use crate::s_2016_10_2::TargetType::{BotTarget, OutputTarget};

#[derive(Copy, Clone, PartialEq)]
enum TargetType { BotTarget, OutputTarget }

#[derive(Copy, Clone)]
struct Target { number: usize, target_type: TargetType }

struct Output { number: usize, chip: Option<usize> }

#[derive(Copy, Clone)]
struct Bot {
    number: usize,
    higher_target: Option<Target>,
    lower_target: Option<Target>,
    chip1: Option<usize>,
    chip2: Option<usize>,
}

struct Repository { bots: Vec<Bot>, outputs: Vec<Output> }

fn run_actions(repository: &mut Repository) {
    let mut action_applied = true;
    while action_applied {
        action_applied = false;
        let mut current_bot: Option<Bot> = None;
        for bot in repository.bots.iter() {
            if bot.chip1.is_some() && bot.chip2.is_some() {
                if let Some(higher) = &bot.higher_target {
                   if let Some(lower) = &bot.lower_target {
                       let found_higher = if higher.target_type == BotTarget {
                           repository.bots.iter().find(|b| b.number == higher.number).is_some()
                       } else { true };
                       let found_lower = if lower.target_type == BotTarget {
                           repository.bots.iter().find(|b| b.number == lower.number).is_some()
                       } else { true };
                       if found_higher && found_lower {
                           current_bot = Some(*bot);
                           break;
                       }
                   }
                };
            }
        };
        if let Some(current) = current_bot {
            apply_action(repository, current);
            action_applied = true;
        };
    }
}

fn apply_action(repository: &mut Repository, bot: Bot) {
    let higher = bot.higher_target.as_ref().unwrap();
    let lower = bot.lower_target.as_ref().unwrap();
    let chip1 = bot.chip1.as_ref().unwrap();
    let chip2 = bot.chip2.as_ref().unwrap();
    let higher_chip = if chip1 > chip2 { chip1 } else { chip2 };
    let lower_chip = if chip1 < chip2 { chip1 } else { chip2 };
    for (target, chip) in vec![(higher, higher_chip), (lower, lower_chip)] {
        if target.target_type == BotTarget {
            let target = repository.bots.iter_mut().find(|bot| bot.number == target.number).unwrap();
            add_chip(target, *chip);
        } else {
            if let Some(target) = repository.outputs.iter_mut().find(|output| output.number == target.number) {
                if target.chip.is_none() {
                    target.chip = Some(*chip)
                } else {
                    panic!("chip already set for output {}", target.number)
                }
            } else {
                repository.outputs.push(Output { number: target.number, chip: Some(*chip) })
            };
        }
    }
    let bot = repository.bots.iter_mut().find(|b| b.number == bot.number).unwrap();
    bot.chip1 = None;
    bot.chip2 = None;
}

fn apply_instruction_bot_gives(repository: &mut Repository, bot_number: usize, lower: Target, higher: Target) {
    let mut bot = repository.bots.iter_mut().find(|bot| bot.number == bot_number);
    match &mut bot {
        Some(bot) => {
            if bot.lower_target.is_none() && bot.higher_target.is_none() {
                bot.higher_target = Some(higher);
                bot.lower_target = Some(lower);
            } else {
                panic!("targets already set for bot {}", bot_number)
            }
        },
        None => repository.bots.push(Bot {
            number: bot_number,
            higher_target: Some(higher),
            lower_target: Some(lower),
            chip1: None,
            chip2: None,
        }),
    }
}

fn apply_instruction_bot_receives(repository: &mut Repository, bot_number: usize, chip: usize) {
    let mut bot = repository.bots.iter_mut().find(|bot| bot.number == bot_number);
    match &mut bot {
        Some(bot) => add_chip(bot, chip),
        None => repository.bots.push(Bot {
            number: bot_number,
            higher_target: None,
            lower_target: None,
            chip1: Some(chip),
            chip2: None,
        }),
    }
}

fn add_chip(bot: &mut Bot, chip: usize) {
    if bot.chip1.is_none() {
        bot.chip1 = Some(chip)
    } else if bot.chip2.is_none() {
        bot.chip2 = Some(chip)
    } else {
        panic!("assigned more than 2 chips to bot {}", bot.number)
    }
}

fn parse_and_apply_instruction(instruction: &str, repository: &mut Repository) {
    let re_gives = Regex::new(r"bot ([0-9]+) gives low to (bot|output) ([0-9]+) and high to (bot|output) ([0-9]+)").unwrap();
    let re_receives= Regex::new(r"value ([0-9]+) goes to bot ([0-9]+)").unwrap();
    match re_gives.captures(instruction) {
        Some(capture) => apply_instruction_bot_gives(
            repository,
            capture.get(1).unwrap().as_str().parse::<usize>().unwrap(),
            Target {
                number: capture.get(3).unwrap().as_str().parse::<usize>().unwrap(),
                target_type: if capture.get(2).unwrap().as_str() == "bot" { BotTarget } else { OutputTarget },
            },
            Target {
                number: capture.get(5).unwrap().as_str().parse::<usize>().unwrap(),
                target_type: if capture.get(4).unwrap().as_str() == "bot" { BotTarget } else { OutputTarget },
            }
        ),
        None => match re_receives.captures(instruction) {
            Some(capture) => apply_instruction_bot_receives(
                repository,
                capture.get(2).unwrap().as_str().parse::<usize>().unwrap(),
                capture.get(1).unwrap().as_str().parse::<usize>().unwrap(),
            ),
            None => panic!("couldn't parse instruction"),
        },
    }
}

pub fn solve() {
    let input: Vec<&str> = include_str!("2016-10.txt").split("\n").collect();
    let mut repository = Repository { bots: Vec::new(), outputs: Vec::new() };
    for instruction in input.iter() {
        parse_and_apply_instruction(instruction, &mut repository);
        run_actions(&mut repository);
    }
    println!("{}", &repository.outputs.iter()
        .filter(|output| vec![0, 1, 2].contains(&output.number))
        .fold(1, |acc, output| acc * output.chip.unwrap()));
}
