use std::collections::HashSet;
use std::fs;

#[derive(Eq, PartialEq, Hash)]
struct Point {
    x: i32,
    y: i32
}

struct Santa {
    current_location: Point,
    visited: HashSet<Point>
}

impl Santa {
    fn new() -> Self {
        Self {
            current_location: Point { x: 0, y: 0 },
            visited: HashSet::from([Point { x: 0, y: 0 }])
        }
    }

    fn visit(&mut self) {
        let house = Point { x: self.current_location.x, y: self.current_location.y };
        self.visited.insert(house);
    }
}

fn main() {
    let filepath = "./day-03-data.txt";
    let file = fs::read_to_string(filepath).expect("Cannot read file");
    let input = file.trim();

    let mut santa = Santa::new();
    let mut another_santa = Santa::new();
    let mut robo_santa = Santa::new();

    for (i, c) in input.chars().enumerate() {
        let this_santa = match i {
            i if i % 2 == 0 => &mut another_santa,
            _ => &mut robo_santa
        };
        match c {
            '^' => {
                santa.current_location.y += 1;
                this_santa.current_location.y += 1;
                santa.visit();
                this_santa.visit();
            },
            '>' => {
                santa.current_location.x += 1;
                this_santa.current_location.x += 1;
                santa.visit();
                this_santa.visit();
            },
            'v' => {
                santa.current_location.y -= 1;
                this_santa.current_location.y -= 1;
                santa.visit();
                this_santa.visit();
            },
            '<' => {
                santa.current_location.x -= 1;
                this_santa.current_location.x -= 1;
                santa.visit();
                this_santa.visit();
            }
            _ => {}
        }
    }

    println!("Part one: {}", santa.visited.len());

    for item in robo_santa.visited {
        another_santa.visited.insert(item);
    }
    println!("Part two: {}", another_santa.visited.len())

}
