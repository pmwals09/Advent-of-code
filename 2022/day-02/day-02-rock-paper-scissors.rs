use std::cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd};
use std::convert::TryFrom;
use std::fs;

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
enum RPSOption {
    Rock = 1,
    Paper = 2,
    Scissors = 3,
}

impl RPSOption {
    fn cmp_round(&self, other: &Self) -> RPSResult {
        match self.cmp(other) {
            Ordering::Less => RPSResult::Lose,
            Ordering::Equal => RPSResult::Draw,
            Ordering::Greater => RPSResult::Win,
        }
    }

    fn get_desired_result(&self, desired_result: &RPSResult) -> Self {
        match self {
            Self::Rock => match desired_result {
                RPSResult::Win => Self::Paper,
                RPSResult::Lose => Self::Scissors,
                RPSResult::Draw => Self::Rock,
            },
            Self::Paper => match desired_result {
                RPSResult::Win => Self::Scissors,
                RPSResult::Lose => Self::Rock,
                RPSResult::Draw => Self::Paper,
            },
            Self::Scissors => match desired_result {
                RPSResult::Win => Self::Rock,
                RPSResult::Lose => Self::Paper,
                RPSResult::Draw => Self::Scissors,
            },
        }
    }
}

impl TryFrom<&String> for RPSOption {
    type Error = String;

    fn try_from(value: &String) -> Result<Self, Self::Error> {
        match value {
            x if x == "A" || x == "X" => Ok(RPSOption::Rock),
            x if x == "B" || x == "Y" => Ok(RPSOption::Paper),
            x if x == "C" || x == "Z" => Ok(RPSOption::Scissors),
            o => Err(format!("Invalid option! {}", o)),
        }
    }
}

impl Ord for RPSOption {
    fn cmp(&self, other: &Self) -> Ordering {
        if *self == Self::Scissors && *other == Self::Rock {
            Ordering::Less
        } else if *self == Self::Rock && *other == Self::Scissors || (*self as u8) > (*other as u8)
        {
            Ordering::Greater
        } else if (*self as u8) == (*other as u8) {
            Ordering::Equal
        } else {
            Ordering::Less
        }
    }
}

impl PartialOrd for RPSOption {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if *self == Self::Scissors && *other == Self::Rock {
            Some(Ordering::Less)
        } else if *self == Self::Rock && *other == Self::Scissors || (*self as u8) > (*other as u8)
        {
            Some(Ordering::Greater)
        } else if (*self as u8) == (*other as u8) {
            Some(Ordering::Equal)
        } else {
            Some(Ordering::Less)
        }
    }
}

#[derive(Debug)]
enum RPSResult {
    Win = 6,
    Lose = 0,
    Draw = 3,
}

impl TryFrom<&String> for RPSResult {
    type Error = String;

    fn try_from(value: &String) -> Result<Self, Self::Error> {
        match value.as_str() {
            "X" => Ok(Self::Lose),
            "Y" => Ok(Self::Draw),
            "Z" => Ok(Self::Win),
            o => Err(format!("Invalid option! {}", o)),
        }
    }
}

fn main() {
    let file_path = "./day-02-input.txt";
    let input = parse_input(file_path);

    let mut part_one_score = 0;
    let mut part_two_score = 0;

    for round in input {
        part_one_score += score_round_part_one(&round);
        part_two_score += score_round_part_two(&round);
    }

    println!("Part one: {}", part_one_score);
    println!("Part two: {}", part_two_score);
}

fn parse_input(file_path: &str) -> Vec<(String, String)> {
    let contents = fs::read_to_string(file_path).unwrap();

    contents
        .trim()
        .lines()
        .map(|l| {
            let parsed_line = l.split_once(' ').unwrap();
            (parsed_line.0.to_string(), parsed_line.1.to_string())
        })
        .collect()
}

fn score_round_part_one(round: &(String, String)) -> i32 {
    let (them, you) = round;
    let them = RPSOption::try_from(them).unwrap();
    let you = RPSOption::try_from(you).unwrap();
    let result = you.cmp_round(&them);

    (result as i32) + (you as i32)
}

fn score_round_part_two(round: &(String, String)) -> i32 {
    let (them, desired_result) = round;
    let them = RPSOption::try_from(them).unwrap();
    let result = RPSResult::try_from(desired_result).unwrap();
    let you = them.get_desired_result(&result);

    (result as i32) + (you as i32)
}

#[cfg(test)]
mod day_two_tests {
    use super::*;

    #[test]
    fn part_one_example() {
        let input = parse_input("./test.txt");
        let mut total = 0;
        for round in input {
            total += score_round_part_one(&round);
        }
        assert_eq!(total, 15)
    }

    #[test]
    fn part_two_example() {
        let input = parse_input("./test.txt");
        let mut total = 0;
        for round in input {
            total += score_round_part_two(&round);
        }
        assert_eq!(total, 12)
    }
}
