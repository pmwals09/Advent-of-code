use std::fs;

#[derive(Debug)]
struct Package {
    width: u32,
    height: u32,
    length: u32,
}

impl Package {
    fn new(sides: Vec<u32>) -> Self {
        Self {
            width: sides[0],
            height: sides[1],
            length: sides[2],
        }
    }

    fn surface_area(&self) -> u32 {
        2 * self.length * self.width + 2 * self.width * self.height + 2 * self.height * self.length
    }

    fn volume(&self) -> u32 {
        self.length * self.width * self.height
    }

    fn smallest_side_area(&self) -> u32 {
        self.width * self.height
    }

    fn smallest_face_circum(&self) -> u32 {
        2 * self.width + 2 * self.height
    }
}

fn main() {
    let filepath = "./day-02-data.txt";
    let input = parse_input(filepath);
    let mut total_wrapping_paper = 0;
    let mut total_ribbon = 0;
    for package in input {
        total_wrapping_paper += package.surface_area() + package.smallest_side_area();

        total_ribbon += package.smallest_face_circum() + package.volume();
    }

    println!("Part one: {}", total_wrapping_paper);
    println!("Part two: {}", total_ribbon);
}

fn parse_input(filepath: &str) -> Vec<Package> {
    let mut packages = Vec::new();

    let file = fs::read_to_string(filepath).expect("Cannot read file");

    for line in file.split('\n') {
        // filter out empty line at the end
        if !line.is_empty() {
            let mut split: Vec<u32> = line
                .split('x')
                .map(|x| match x.parse::<u32>() {
                    Ok(y) => y,
                    Err(_e) => 0,
                })
                .collect();
            split.sort();
            packages.push(Package::new(split));
        }
    }

    packages
}
