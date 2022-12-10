use std::{cmp::Ordering, collections::HashSet, fmt::Error, fs, path::PathBuf, str::FromStr};

enum MoveDirection {
    U,
    UR,
    R,
    DR,
    D,
    DL,
    L,
    UL,
}

struct Direction {
    dir: MoveDirection,
    amt: u8,
}

impl FromStr for Direction {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (dir, amt) = s.split_once(' ').unwrap();
        let amt: u8 = amt.parse().unwrap();
        let dir = match dir {
            "U" => MoveDirection::U,
            "UR" => MoveDirection::UR,
            "R" => MoveDirection::R,
            "DR" => MoveDirection::DR,
            "D" => MoveDirection::D,
            "DL" => MoveDirection::DL,
            "L" => MoveDirection::L,
            "UL" => MoveDirection::UL,
            _ => MoveDirection::U,
        };

        Ok(Direction { dir, amt })
    }
}

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn new() -> Self {
        Self { x: 0, y: 0 }
    }
}

#[derive(Clone)]
struct Cursor {
    pos: Point,
    history: HashSet<Point>,
}

impl Cursor {
    fn new() -> Self {
        let start = Point::new();
        let mut n = Self {
            pos: start,
            history: HashSet::new(),
        };
        n.history.insert(start);
        n
    }

    fn follow(&mut self, other: &Self) {
        if (self.pos.x - other.pos.x).abs() > 1 || (self.pos.y - other.pos.y).abs() > 1 {
            match self.pos.x.cmp(&other.pos.x) {
                Ordering::Greater => match self.pos.y.cmp(&other.pos.y) {
                    Ordering::Greater => self.move_cursor(&MoveDirection::DL),
                    Ordering::Less => self.move_cursor(&MoveDirection::UL),
                    Ordering::Equal => self.move_cursor(&MoveDirection::L),
                },
                Ordering::Less => match self.pos.y.cmp(&other.pos.y) {
                    Ordering::Greater => self.move_cursor(&MoveDirection::DR),
                    Ordering::Less => self.move_cursor(&MoveDirection::UR),
                    Ordering::Equal => self.move_cursor(&MoveDirection::R),
                },
                Ordering::Equal => match self.pos.y.cmp(&other.pos.y) {
                    Ordering::Greater => self.move_cursor(&MoveDirection::D),
                    Ordering::Less => self.move_cursor(&MoveDirection::U),
                    Ordering::Equal => {}
                },
            }
        }
    }

    fn move_cursor(&mut self, direction: &MoveDirection) {
        match direction {
            MoveDirection::U => {
                self.pos.y += 1;
            }
            MoveDirection::UR => {
                self.pos.x += 1;
                self.pos.y += 1;
            }
            MoveDirection::R => {
                self.pos.x += 1;
            }
            MoveDirection::DR => {
                self.pos.x += 1;
                self.pos.y -= 1;
            }
            MoveDirection::D => {
                self.pos.y -= 1;
            }
            MoveDirection::DL => {
                self.pos.x -= 1;
                self.pos.y -= 1;
            }
            MoveDirection::L => {
                self.pos.x -= 1;
            }
            MoveDirection::UL => {
                self.pos.x -= 1;
                self.pos.y += 1;
            }
        }
        self.history.insert(self.pos);
    }
}

fn main() {
    let file_path = PathBuf::from("./day-09-data.txt");
    let contents = fs::read_to_string(file_path).unwrap();
    let lines: Vec<&str> = contents.trim().lines().collect();
    let mut h = Cursor::new();
    let mut t = Cursor::new();
    let mut ts = Vec::new();
    for _ in 0..9 {
        ts.push(Cursor::new());
    }
    for line in lines {
        let direction = Direction::from_str(line).unwrap();
        for _ in 0..direction.amt {
            h.move_cursor(&direction.dir);
            t.follow(&h);
            ts[0].follow(&h);
            for i in 1..ts.len() {
                let to_follow = ts[i - 1].clone();
                ts[i].follow(&to_follow);
            }
        }
    }

    println!("Part one: {}", t.history.len());
    println!("Part two: {}", ts.last().unwrap().history.len());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one_test() {
        let test_input = "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2";
        let lines: Vec<&str> = test_input.trim().lines().collect();
        let mut h = Cursor::new();
        let mut t = Cursor::new();
        for line in lines {
            let direction = Direction::from_str(line).unwrap();
            for _ in 0..direction.amt {
                h.move_cursor(&direction.dir);
                t.follow(&h);
            }
        }

        assert_eq!(t.history.len(), 13)
    }

    #[test]
    fn part_two_test() {
        let test_input = "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20";
        let lines: Vec<&str> = test_input.trim().lines().collect();
        let mut h = Cursor::new();
        let mut ts = Vec::new();
        for _ in 0..9 {
            ts.push(Cursor::new());
        }
        for line in lines {
            let direction = Direction::from_str(line).unwrap();
            for _ in 0..direction.amt {
                h.move_cursor(&direction.dir);
                ts[0].follow(&h);
                for i in 1..ts.len() {
                    let to_follow = ts[i - 1].clone();
                    ts[i].follow(&to_follow);
                }
            }
        }

        assert_eq!(ts.last().unwrap().history.len(), 36)
    }
}
