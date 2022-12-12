use std::{
    collections::VecDeque,
    fmt::{Debug, Formatter},
    fs,
    path::PathBuf,
};

struct Monkey {
    monkey_n: u8,
    items: VecDeque<i64>,
    op: Box<dyn Fn(i64) -> i64>,
    test_div: i64,
    success_n: u8,
    fail_n: u8,
    inspections: i64,
}

impl Debug for Monkey {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Monkey")
            .field("monkey_n", &self.monkey_n)
            .field("items", &self.items)
            .field("op", &"[Box<dyn Fn(i64) -> i64>]")
            .field("test_div", &self.test_div)
            .field("success_n", &self.success_n)
            .field("fail_n", &self.fail_n)
            .field("inspections", &self.inspections)
            .finish()
    }
}

impl Monkey {
    fn from_file_part(s: String) -> Self {
        let mut lines = s.lines().map(|ea| ea.to_string());

        let next = lines.next().unwrap();
        let (_, monkey_n) = next.split_once(' ').unwrap();
        let monkey_n = monkey_n.chars().next().unwrap().to_digit(10).unwrap();
        let monkey_n = monkey_n as u8;

        let next = lines.next().unwrap();
        let (_, items_str) = next.split_once(": ").unwrap();
        let items = VecDeque::from_iter(items_str.split(", ").map(|x| x.parse::<i64>().unwrap()));

        let next = lines.next().unwrap();
        let op_line = next.split(" = ").collect::<Vec<&str>>()[1];
        let op: Box<dyn Fn(i64) -> i64> = match op_line.split(' ').collect::<Vec<&str>>()[..] {
            ["old", sign, "old"] => match sign {
                "*" => Box::new(move |n: i64| n * n),
                "+" => Box::new(move |n: i64| n + n),
                _ => panic!(),
            },
            ["old", sign, arg] => {
                let val: i64 = arg.parse().unwrap();
                match sign {
                    "*" => Box::new(move |n: i64| n * val),
                    "+" => Box::new(move |n: i64| n + val),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        };

        let next = lines.next().unwrap();
        let (_, test_div) = next.split_once(" by ").unwrap();
        let test_div: i64 = test_div.parse().unwrap();

        let next = lines.next().unwrap();
        let (_, success_n) = next.split_once(" monkey ").unwrap();
        let success_n: u8 = success_n.parse().unwrap();

        let next = lines.next().unwrap();
        let (_, fail_n) = next.split_once(" monkey ").unwrap();
        let fail_n: u8 = fail_n.parse().unwrap();

        Self {
            monkey_n,
            items,
            op,
            test_div,
            success_n,
            fail_n,
            inspections: 0,
        }
    }
}

fn main() {
    let file_path = PathBuf::from("./day-11-data.txt");
    let contents = fs::read_to_string(file_path).unwrap();
    let mut monkeys_part_one = parse_file_to_monkeys(&contents);
    let mut monkeys_part_two = parse_file_to_monkeys(&contents);
    let group_test_div = monkeys_part_two.iter().fold(1, |acc, ea| acc * ea.test_div);
    for _ in 0..20 {
        run_round_part_one(&mut monkeys_part_one);
        run_round_part_two(&mut monkeys_part_two, group_test_div);
    }
    for _ in 20..10000 {
        run_round_part_two(&mut monkeys_part_two, group_test_div);
    }

    monkeys_part_one.sort_by(|a, b| b.inspections.cmp(&a.inspections));
    monkeys_part_two.sort_by(|a, b| b.inspections.cmp(&a.inspections));
    println!(
        "Part one: {}",
        monkeys_part_one[0..2]
            .iter()
            .fold(1, |out, ea| out * ea.inspections)
    );
    println!(
        "Part one: {}",
        monkeys_part_two[0..2]
            .iter()
            .fold(1, |out, ea| out * ea.inspections)
    );
}

fn parse_file_to_monkeys(contents: &str) -> Vec<Monkey> {
    let monkey_strs = contents.split("\n\n");
    monkey_strs
        .map(|ea| Monkey::from_file_part(ea.to_string()))
        .collect()
}

fn run_round_part_one(monkeys: &mut Vec<Monkey>) {
    let mut moves: Vec<VecDeque<i64>> = vec![VecDeque::new(); monkeys.len()];
    for monkey in &mut *monkeys {
        while let Some(item) = monkey.items.pop_front() {
            let new_worry = (monkey.op)(item) / 3;
            monkey.inspections += 1;
            if new_worry % monkey.test_div == 0 {
                moves[monkey.success_n as usize].push_back(new_worry);
            } else {
                moves[monkey.fail_n as usize].push_back(new_worry);
            }
        }

        while let Some(item) = moves[monkey.monkey_n as usize].pop_front() {
            let new_worry = (monkey.op)(item) / 3;
            monkey.inspections += 1;
            if new_worry % monkey.test_div == 0 {
                moves[monkey.success_n as usize].push_back(new_worry);
            } else {
                moves[monkey.fail_n as usize].push_back(new_worry);
            }
        }
    }

    for monkey in monkeys {
        monkey.items.append(&mut moves[monkey.monkey_n as usize])
    }
}

fn run_round_part_two(monkeys: &mut Vec<Monkey>, group_test_div: i64) {
    let mut moves: Vec<VecDeque<i64>> = vec![VecDeque::new(); monkeys.len()];
    for monkey in &mut *monkeys {
        while let Some(item) = monkey.items.pop_front() {
            let new_worry = (monkey.op)(item) % group_test_div;
            monkey.inspections += 1;
            if new_worry % monkey.test_div == 0 {
                moves[monkey.success_n as usize].push_back(new_worry);
            } else {
                moves[monkey.fail_n as usize].push_back(new_worry);
            }
        }

        while let Some(item) = moves[monkey.monkey_n as usize].pop_front() {
            let new_worry = (monkey.op)(item) % group_test_div;
            monkey.inspections += 1;
            if new_worry % monkey.test_div == 0 {
                moves[monkey.success_n as usize].push_back(new_worry);
            } else {
                moves[monkey.fail_n as usize].push_back(new_worry);
            }
        }
    }

    for monkey in monkeys {
        monkey.items.append(&mut moves[monkey.monkey_n as usize])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_INPUT: &str = "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1";

    #[test]
    fn part_one_test() {
        let contents = TEST_INPUT.trim().to_string();
        let mut monkeys = parse_file_to_monkeys(&contents);
        for _ in 0..20 {
            run_round_part_one(&mut monkeys)
        }

        monkeys.sort_by(|a, b| b.inspections.cmp(&a.inspections));
        assert_eq!(
            monkeys[0..2]
                .iter()
                .fold(1, |acc, ea: &Monkey| acc * ea.inspections),
            10605
        )
    }

    #[test]
    fn part_two_test() {
        let contents = TEST_INPUT.trim().to_string();
        let mut monkeys = parse_file_to_monkeys(&contents);
        let group_test_div = monkeys.iter().fold(1, |acc, ea| acc * ea.test_div);
        for _ in 0..10000 {
            run_round_part_two(&mut monkeys, group_test_div);
        }

        monkeys.sort_by(|a, b| b.inspections.cmp(&a.inspections));
        assert_eq!(
            monkeys[0..2]
                .iter()
                .fold(1, |acc, ea: &Monkey| acc * ea.inspections),
            2713310158
        )
    }
}
