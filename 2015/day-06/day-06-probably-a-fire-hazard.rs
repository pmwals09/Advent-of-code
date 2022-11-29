use std::fs;

#[derive(Debug)]
enum Action {
    TurnOn,
    TurnOff,
    Toggle,
    NoAction,
}

#[derive(Debug)]
struct Point {
    x: usize,
    y: usize,
}

impl Point {
    fn from_line(text: String) -> Self {
        let (x_str, y_str) = text.split_once(',').unwrap();
        Self {
            x: x_str.to_string().parse::<usize>().unwrap(),
            y: y_str.to_string().parse::<usize>().unwrap(),
        }
    }
}
#[derive(Debug)]
struct Instruction {
    action: Action,
    range_start: Point,
    range_end: Point,
}

impl Instruction {
    fn from_line(line: String) -> Self {
        let parts = line
            .split(" through ")
            .map(|s| s.to_string())
            .collect::<Vec<String>>();
        let range_end = parts[1].clone();
        let (action, range_start) = parts[0].rsplit_once(' ').unwrap();
        Self {
            action: {
                match action {
                    "toggle" => Action::Toggle,
                    "turn on" => Action::TurnOn,
                    "turn off" => Action::TurnOff,
                    _ => Action::NoAction,
                }
            },
            range_start: Point::from_line(range_start.to_string()),
            range_end: Point::from_line(range_end),
        }
    }
}

fn main() {
    let filename = "./day-06-data.txt";
    let input = parse_input(filename);
    let mut lights: [[i32; 1000]; 1000] = [[0; 1000]; 1000];
    let mut bright_lights: [[i32; 1000]; 1000] = [[0; 1000]; 1000];
    for line in input {
        match line.action {
            Action::TurnOn => {
                turn_on(&mut lights, &line.range_start, &line.range_end);
                brighten(&mut bright_lights, &line.range_start, &line.range_end)
            }
            Action::TurnOff => {
                turn_off(&mut lights, &line.range_start, &line.range_end);
                dim(&mut bright_lights, &line.range_start, &line.range_end)
            }
            Action::Toggle => {
                toggle(&mut lights, &line.range_start, &line.range_end);
                really_brighten(&mut bright_lights, &line.range_start, &line.range_end)
            }
            _ => {}
        }
    }

    let mut total_lights = 0;
    let mut total_brightness = 0;
    for i in 0..1000 {
        for j in 0..1000 {
            total_lights += lights[i][j];
            total_brightness += bright_lights[i][j];
        }
    }

    println!("Part one: {}", total_lights);
    println!("Part two: {}", total_brightness);
}

fn parse_input(filepath: &str) -> Vec<Instruction> {
    let mut res = Vec::new();
    let contents = fs::read_to_string(filepath).unwrap();
    for line in contents.trim().split('\n') {
        res.push(Instruction::from_line(line.to_string()));
    }
    res
}

fn turn_on(lights: &mut [[i32; 1000]; 1000], range_start: &Point, range_end: &Point) {
    for i in range_start.x..=range_end.x {
        for j in range_start.y..=range_end.y {
            lights[i][j] = 1;
        }
    }
}

fn turn_off(lights: &mut [[i32; 1000]; 1000], range_start: &Point, range_end: &Point) {
    for i in range_start.x..=range_end.x {
        for j in range_start.y..=range_end.y {
            lights[i][j] = 0;
        }
    }
}

fn toggle(lights: &mut [[i32; 1000]; 1000], range_start: &Point, range_end: &Point) {
    for i in range_start.x..=range_end.x {
        for j in range_start.y..=range_end.y {
            lights[i][j] = {
                if lights[i][j] == 0 {
                    1
                } else {
                    0
                }
            };
        }
    }
}

fn brighten(lights: &mut [[i32; 1000]; 1000], range_start: &Point, range_end: &Point) {
    for i in range_start.x..=range_end.x {
        for j in range_start.y..=range_end.y {
            lights[i][j] += 1;
        }
    }
}

fn dim(lights: &mut [[i32; 1000]; 1000], range_start: &Point, range_end: &Point) {
    for i in range_start.x..=range_end.x {
        for j in range_start.y..=range_end.y {
            lights[i][j] = {
                if lights[i][j] == 0 {
                    0
                } else {
                    lights[i][j] - 1
                }
            };
        }
    }
}

fn really_brighten(lights: &mut [[i32; 1000]; 1000], range_start: &Point, range_end: &Point) {
    for i in range_start.x..=range_end.x {
        for j in range_start.y..=range_end.y {
            lights[i][j] += 2;
        }
    }
}
