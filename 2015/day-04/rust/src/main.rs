use std::fs;
use md5;

fn main() {
    let input = parse_input("../day-04-data.txt");
    let mut result = String::new();
    let mut leader = 0;

    while !result.starts_with("00000"){
        result = get_advent_hash(leader, &input);
        leader += 1;
    }

    println!("Part one: {}", leader - 1);

    while !result.starts_with("000000") {
        result = get_advent_hash(leader, &input);
        leader += 1;
    }

    println!("Part two: {}", leader - 1);
}

fn parse_input(filepath: &str) -> String {
    let contents = fs::read_to_string(filepath).unwrap();
    contents.trim().to_string()
}

fn get_advent_hash(num: i32, input: &String) -> String {
    let digest = md5::compute(format!("{}{}", input, num.to_string()));
    format!("{:x}", digest)
}
