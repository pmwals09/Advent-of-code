use std::fs;

fn main() {
    let input = fs::read_to_string("./day-01-data.json")
        .expect("cannot read file");

    let mut floor = 0;
    let mut first_negative = 0;
    let mut index = 0;

    for c in input.chars() {
        match c {
            '(' => {
                floor += 1;
                index += 1;
            },
            ')' => {
                floor -= 1;
                index += 1;
                if floor < 0 && first_negative == 0 {
                    first_negative = index;
                }
            },
            _ => println!("End.")
        }
    };

    println!("Floor: {}", floor);
    println!("First negative: {}", first_negative);
}
