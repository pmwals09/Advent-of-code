use std::fs;
use std::collections::HashMap;

struct Wire {
    name: String,
    signal: Option<i32>
}

enum LeftSide {
    Value(i32),
    And((Wire, Wire)),
    Or((Wire, Wire)),
    LShift((Wire, i32)),
    RShift((Wire, i32)),
    Not(Wire)
}

impl LeftSide {
    fn eval(&self) -> i32 {
        match self {
            Self::Value(x) => *x,
            Self::And((a, b)) => {
                0
            },
            Self::Or((a, b)) => {
                0
            },
            Self::LShift((w, rot)) => {
                0
            },
            Self::RShift((w, rot)) => {
                0
            },
            Self::Not(w) => {
                0
            }
        }
    }
}

fn main() {
    let filepath = "./day-07-data.txt";
    let input = parse_input(filepath);
    for line in input {
        println!("{}", line);
    }
}

fn parse_input<'fp>(filepath: &'fp str) -> Vec<String> {
    let contents = fs::read_to_string(filepath).unwrap();
    contents.trim().lines().map(|l| String::from(l)).collect()
}

fn get_wire_val(input: Vec<String>, target_wire: &str) -> i32 {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    const RAW_INPUT: &str = "123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i";

    #[test]
    fn part_one_d() {
        let input = RAW_INPUT.trim().lines().map(|l| String::from(l)).collect();
        assert_eq!(get_wire_val(input, "d"), 72)
    }

    #[test]
    fn part_one_e() {
        let input = RAW_INPUT.trim().lines().map(|l| String::from(l)).collect();
        assert_eq!(get_wire_val(input, "e"), 507)
    }

    #[test]
    fn part_one_f() {
        let input = RAW_INPUT.trim().lines().map(|l| String::from(l)).collect();
        assert_eq!(get_wire_val(input, "f"), 492)
    }

    #[test]
    fn part_one_g() {
        let input = RAW_INPUT.trim().lines().map(|l| String::from(l)).collect();
        assert_eq!(get_wire_val(input, "g"), 114)
    }

    #[test]
    fn part_one_h() {
        let input = RAW_INPUT.trim().lines().map(|l| String::from(l)).collect();
        assert_eq!(get_wire_val(input, "h"), 65412)
    }

    #[test]
    fn part_one_i() {
        let input = RAW_INPUT.trim().lines().map(|l| String::from(l)).collect();
        assert_eq!(get_wire_val(input, "i"), 65079)
    }

    #[test]
    fn part_one_x() {
        let input = RAW_INPUT.trim().lines().map(|l| String::from(l)).collect();
        assert_eq!(get_wire_val(input, "x"), 123)
    }

    #[test]
    fn part_one_y() {
        let input = RAW_INPUT.trim().lines().map(|l| String::from(l)).collect();
        assert_eq!(get_wire_val(input, "y"), 456)
    }
}
