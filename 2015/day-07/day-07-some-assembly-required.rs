use std::fs;
use std::collections::HashMap;

struct Wire<'a> {
    identifier: &'a str,
    signal: i32,
}

enum GateType {
    And,
    Or,
    LShift,
    RShift,
    Not
}

struct Gate {
    gate_type: GateType,
    inputs: Vec<i32>,
}

fn main() {
    let filepath = "./day-07-data.txt";
    let input = parse_input(filepath);
    for line in input {
    }
}

fn parse_input<'fp>(filepath: &'fp str) -> Vec<String> {
    let contents = fs::read_to_string(filepath).unwrap();
    contents.trim().split('\n').map(|s| s.to_string()).collect()
}
