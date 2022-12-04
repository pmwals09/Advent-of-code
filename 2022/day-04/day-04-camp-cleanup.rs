use std::ops::RangeInclusive;
use std::str::FromStr;
use std::{fs, path::PathBuf};

trait WrapsAround {
    fn wraps_around(&self, other: &Self) -> bool;
    fn from_input(s: &str) -> Self;
    fn overlaps(&self, other: &Self) -> bool;
}

impl WrapsAround for RangeInclusive<usize> {
    fn wraps_around(&self, other: &Self) -> bool {
        other.start() >= self.start() && other.end() <= self.end()
    }

    fn from_input(s: &str) -> Self {
        let (start, end) = s.split_once('-').unwrap();
        Self::new(
            start.parse::<usize>().unwrap(),
            end.parse::<usize>().unwrap(),
        )
    }

    fn overlaps(&self, other: &Self) -> bool {
        self.contains(other.start()) || other.contains(self.start())
    }
}

fn main() {
    let file_path = PathBuf::from_str("./day-04-data.txt").unwrap();
    let input = parse_input(file_path);

    let mut part_one_total = 0;
    let mut part_two_total = 0;
    for (first, second) in input {
        if first.wraps_around(&second) || second.wraps_around(&first) {
            part_one_total += 1;
        }

        if first.overlaps(&second) {
            part_two_total += 1;
        }
    }

    println!("Part one: {}", part_one_total);
    println!("Part two: {}", part_two_total);
}

fn parse_input(file_path: PathBuf) -> Vec<(RangeInclusive<usize>, RangeInclusive<usize>)> {
    let contents = fs::read_to_string(file_path).unwrap();
    contents
        .trim()
        .lines()
        .map(|l| {
            let ranges = l.split_once(',').unwrap();
            (
                RangeInclusive::from_input(ranges.0),
                RangeInclusive::from_input(ranges.1),
            )
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    #[test]
    fn part_one_test() {
        let file_path = PathBuf::from_str("./test.txt").unwrap();
        let input = parse_input(file_path);

        let mut total = 0;
        for (first, second) in input {
            if first.wraps_around(&second) || second.wraps_around(&first) {
                total += 1;
            }
        }

        assert_eq!(total, 2);
    }

    #[test]
    fn part_two_test() {
        let file_path = PathBuf::from_str("./test.txt").unwrap();
        let input = parse_input(file_path);

        let mut total = 0;
        for (first, second) in input {
            if first.overlaps(&second) {
                total += 1;
            }
        }

        assert_eq!(total, 4);
    }
}
