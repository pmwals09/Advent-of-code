use std::{fmt::Error, fs, path::PathBuf, str::FromStr};

enum Op {
    Noop,
    AddX(i32),
}

impl FromStr for Op {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split_once(' ') {
            None => Ok(Op::Noop),
            Some((_, amt)) => {
                let amt: i32 = amt.parse().unwrap();
                Ok(Op::AddX(amt))
            }
        }
    }
}

fn main() {
    let file_path = PathBuf::from("./day-10-data.txt");
    let contents = fs::read_to_string(file_path).unwrap();
    let lines: Vec<&str> = contents.lines().collect();
    let mut part_one_clock = 0;
    let mut part_two_clock = 0;
    let mut x_reg = 1;
    let mut vals = vec![];
    let mut crt = String::new();
    for line in lines {
        let (x_adjust, clock_adjust) = parse_line(&Op::from_str(line).unwrap());
        for _ in 0..clock_adjust {
            part_one_tick(&mut part_one_clock, &mut vals, &x_reg);
            part_two_tick(&x_reg, &mut part_two_clock, &mut crt);
        }
        x_reg += x_adjust;
    }

    println!("Part one: {}", vals.iter().sum::<i32>());
    println!("Part two: \n{}", parse_crt(crt));
}

fn parse_line(op: &Op) -> (i32, i32) {
    match op {
        Op::Noop => (0, 1),
        Op::AddX(v) => (*v, 2),
    }
}

fn part_one_tick(clock: &mut i32, vals: &mut Vec<i32>, x_reg: &i32) {
    *clock += 1;
    if clock == &20 || (clock > &mut 20 && ((*clock - 20) % 40 == 0)) {
        vals.push(x_reg * *clock);
    }
}

fn part_two_tick(x_reg: &i32, clock: &mut i32, crt: &mut String) {
    if (x_reg - 1..=x_reg + 1).contains(&(*clock % 40)) {
        crt.push('#');
    } else {
        crt.push('.');
    }
    *clock += 1;
}

fn parse_crt(crt: String) -> String {
    let mut res = vec![];
    for i in 0..crt.len() / 40 {
        let to_push = &crt[i * 40..i * 40 + 40];
        res.push(to_push);
    }
    res.join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_INPUT: &str = "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop";

    #[test]
    fn part_one_test_large() {
        let lines: Vec<&str> = TEST_INPUT.trim().lines().collect();
        let mut clock = 0;
        let mut x_reg = 1;
        let mut vals = vec![];
        for line in lines {
            let (x_adjust, clock_adjust) = parse_line(&Op::from_str(&line).unwrap());
            for _ in 0..clock_adjust {
                part_one_tick(&mut clock, &mut vals, &x_reg)
            }
            x_reg += x_adjust;
        }

        assert_eq!(vals.iter().sum::<i32>(), 13140)
    }

    #[test]
    fn part_two_test() {
        let lines: Vec<&str> = TEST_INPUT.trim().lines().collect();
        let mut clock = 0;
        let mut x_reg = 1;
        let mut crt = String::new();
        for line in lines {
            let (x_adjust, clock_adjust) = parse_line(&Op::from_str(&line).unwrap());
            for _ in 0..clock_adjust {
                part_two_tick(&x_reg, &mut clock, &mut crt);
            }
            x_reg += x_adjust;
        }

        assert_eq!(
            parse_crt(crt),
            vec![
                "##..##..##..##..##..##..##..##..##..##..",
                "###...###...###...###...###...###...###.",
                "####....####....####....####....####....",
                "#####.....#####.....#####.....#####.....",
                "######......######......######......####",
                "#######.......#######.......#######....."
            ]
            .join("\n")
        )
    }
}

// 191 too low
