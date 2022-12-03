use std::{collections::HashSet, fs, iter::FromIterator};

fn main() {
    let file_path = "./day-03-data.txt";
    let input = parse_input(file_path);
    let mut part_one_total = 0;
    let mut part_two_total = 0;
    for (i, line) in input.iter().enumerate() {
        part_one_total += score_line(line);
        if (i + 1) % 3 == 0 {
            part_two_total += score_group(&input[i - 2..=i]);
        }
    }

    println!("Part one: {}", part_one_total);
    println!("Part two: {}", part_two_total);
}

fn parse_input(file_path: &str) -> Vec<String> {
    let contents = fs::read_to_string(file_path).unwrap();
    contents.lines().map(|x| x.trim().to_string()).collect()
}

fn score_line(input: &str) -> i32 {
    let front_set: HashSet<char> = HashSet::from_iter(input[..input.len() / 2].chars());
    let back_set: HashSet<char> = HashSet::from_iter(input[input.len() / 2..].chars());

    let c = front_set.intersection(&back_set).collect::<Vec<&char>>()[0];

    score_char(c)
}

fn score_group(group: &[String]) -> i32 {
    let sets = group
        .iter()
        .map(|x| {
            let mut set = HashSet::new();
            for c in x.chars() {
                set.insert(c);
            }
            set
        })
        .collect::<Vec<HashSet<char>>>();

    let first_intersection = sets[0]
        .intersection(&sets[1])
        .copied()
        .collect::<Vec<char>>();

    let first_i_hs = HashSet::from_iter(first_intersection);

    let second_intersection = first_i_hs.intersection(&sets[2]).collect::<Vec<&char>>();

    let c = second_intersection[0];

    score_char(c)
}

fn score_char(c: &char) -> i32 {
    match *c as i32 {
        x if x <= 'z' as i32 && x >= 'a' as i32 => x - ('a' as i32) + 1,
        x if x <= 'Z' as i32 && x >= 'A' as i32 => x - ('A' as i32) + 27,
        _ => 0,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one_test() {
        let test_data = "vJrwpWtwJgWrhcsFMMfFFhFp
        jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
        PmmdzqPrVvPwwTWBwg
        wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
        ttgJtRGJQctTZtZT
        CrZsJsPPZsGzwwsLwLmpwMDw";

        let mut total = 0;
        for line in test_data
            .lines()
            .map(|x| x.trim().to_string())
            .collect::<Vec<String>>()
        {
            total += score_line(&line);
        }
        assert_eq!(total, 157)
    }

    #[test]
    fn part_two_test() {
        let test_data = "vJrwpWtwJgWrhcsFMMfFFhFp
        jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
        PmmdzqPrVvPwwTWBwg
        wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
        ttgJtRGJQctTZtZT
        CrZsJsPPZsGzwwsLwLmpwMDw";

        let mut total = 0;
        let input = test_data
            .lines()
            .map(|x| x.trim().to_string())
            .collect::<Vec<String>>();
        for (i, _line) in input.iter().enumerate() {
            if (i + 1) % 3 == 0 {
                total += score_group(&input[i - 2..=i]);
            }
        }

        assert_eq!(total, 70)
    }
}
