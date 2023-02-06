// After many attempts, this was taken from https://fasterthanli.me/series/advent-of-code-2022/part-12
// I've annotated the code below to try and understand better

use std::{fmt, collections::HashSet};

// Start by defining types to structure the problem
#[derive(Clone, Copy, PartialEq, Eq)]
enum Val {
    Start,
    End,
    Cell(u8),
}

// Helper to put values to the letter that is easier to work with
impl Val {
    fn height(self) -> u8 {
        match self {
            Val::Start => 0,
            Val::End => 25,
            Val::Cell(c) => c
        }
    }

}

// Traditional OO grids are tough in Rust because of the lack of memory safety
// cf: https://github.com/nrc/r4cppp/blob/master/graphs/README.md for a discussion
// of the problem. A suggested alternative, used here by Amos, is to have a graph
// data saved as Vecs: the full data as a Vec<Val>, and visited and next_to_visit
// data as HashSet<Point>
struct Grid {
    width:  usize,
    height: usize,
    contents: Vec<Val>,
    visited: HashSet<Point>,
    current: HashSet<Point>,
    num_steps: usize,
}

impl Grid {
    /// Here we read in the string and parse out the raw grid data into the 
    /// proper Val::* items, omitting the line breaks since we'll use **Math**
    /// to find what's up, down, left, and right when traversing
    fn parse(input: &str) -> Self {
        // We will use the width and height of the grid later when traversing
        let width = input.lines().next().unwrap().len();
        let height = input.lines().count();
        let mut contents = vec![];

        for c in input.chars() {
            let ea = match c {
                'S' => Val::Start,
                'E' => Val::End,
                'a'..='z' => Val::Cell(c as u8 - b'a'),
                '\r' | '\n' => continue,
                _ => panic!("Invalid character: {}", c),
            };
            contents.push(ea);
        }

        Self {
            width,
            height,
            contents,
            current: Default::default(),
            visited: Default::default(),
            num_steps: 0,
        }
    }

    /// Helper to avoid going out of the grid when assessing neighbors
    fn in_bounds(&self, pt: Point) -> bool {
        pt.x < self.width && pt.y < self.height
    }

    /// getter for a single item in the grid
    fn get(&self, pt: Point) -> Option<&Val> {
        if !self.in_bounds(pt) {
            return None;
        }
        // this is where we use **Math** to translate x and y coordinates into
        // a single index value
        Some(&self.contents[pt.y * self.width + pt.x])
    }

    /// Helper to find a point's valid neighbors
    // I find this type annotation interesting - before I copied this in, the
    // compiler was giving me something crazy: a <Filter<Filter<Filter<Map<Option>>>>>
    // or something like that. In the end, though, we don't care about the exact
    // type, only that we can iterate over it
    fn get_valid_neighbors(&self, pt: Point, part_two: bool) -> impl Iterator<Item = Point> + '_ {
        let curr_height = self.get(pt).unwrap().height();
        let neighbor_map: [(isize, isize); 4] = [(-1, 0), (1, 0), (0, 1), (0, -1)];
        neighbor_map.into_iter().filter_map(move |(dx, dy)| {
            Some(Point {
                // We use this weird method to add a usize to an isize.
                // I hate it, and it means we need to add a rust-toolchain.toml
                // in the root to use nightly. This is one of the things that
                // frustrates me about Rust - without a guide, I would have very 
                // much struggled over something that I would expect to be easy...
                x: pt.x.checked_add_signed(dx)?,
                y: pt.y.checked_add_signed(dy)?,
            })
        })
        .filter(|&pt| self.in_bounds(pt))
        .filter(move |&pt| {
            let other_height = self.get(pt).unwrap().height();
            if part_two {
                // TODO: This might be wrong...
                return curr_height <= other_height + 1;
            } else {
                return other_height <= curr_height + 1;
            }
        })
    }

    /// The magic - move the frontier of our BFS by one iteration
    fn step(&mut self, part_two: bool) {
        // if our current - i.e., our next-to-visit list, is empty, we initialize it
        if self.current.is_empty() {
            let mut start_pt: Option<Point> = None;
            // here we iterate through the graph to find the starting point and
            // set it in our self.current later
            for y in 0..self.height {
                for x in 0..self.width {
                    let pt: Point = (x, y).into();
                    if let Val::Start = self.get(pt).unwrap(){
                        if !part_two {
                            start_pt = Some(pt);
                            break;
                        }
                    }

                    if let Val::End = self.get(pt).unwrap() {
                        if part_two {
                            start_pt = Some(pt);
                            break;
                        }
                    }
                }
            }

            let start_pt = start_pt.unwrap();
            self.current.insert(start_pt);
            self.visited.insert(start_pt);
            return;
        }

        // https://doc.rust-lang.org/std/mem/fn.take.html
        // So it looks like we move the self.current into a local variable?
        let current = std::mem::take(&mut self.current);
        let mut next = HashSet::new();
        // We move the self.visited into a local variable?
        let mut visited = std::mem::take(&mut self.visited);

        // we iterate through current i.e. next-to-visit
        for curr in current {
            // we get the valid neighbors of the next-to-visit point
            for neighbor in self.get_valid_neighbors(curr, part_two) {
                // if we've already hit this point, let's not re-do it
                // I wonder if this is what I was missing before, and why it was
                // taking so long?
                if visited.contains(&neighbor) {
                    continue;
                }

                // add this new neighbor to the visited list
                // add it to the next HashSet
                visited.insert(neighbor);
                next.insert(neighbor);
            }
        }

        // after we've gone through all the previous self.current items (since
        // we moved them into a local variable), we replace self.current with the
        // new list of next-to-visit items from this iteration
        self.current = next;
        // Put that thing back where it came from or so help me...
        self.visited = visited;
        // Update the number of iterations we've taken
        self.num_steps += 1;
    }
}

/// Pretty representation of a grid
impl fmt::Debug for Grid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}x{} grid:", self.width, self.height)?;
        for y in 0..self.height {
            for x in 0..self.width {
                let item = self.get((x, y).into()).unwrap();
                let i = match item {
                    Val::Start => 'S',
                    Val::End => 'E',
                    Val::Cell(c) => (b'a' + c) as char,
                };
                write!(f, "{}", i)?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

/// A coordinate in the grid
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Point {
    x: usize,
    y: usize
}

/// Get a point from grid indices
impl From<(usize, usize)> for Point {
    fn from((x, y): (usize, usize)) -> Self {
        Self { x, y }
    }
}

fn main() {
    part_one();
    part_two();
}

fn part_one() {
    let mut grid = Grid::parse(include_str!("../day-12-data.txt"));
    // I feel like ideally we would associate a point with a value to make this
    // particular check easier, but I think part of the value of this solution
    // is that we don't need to build the entire graph. To associate a Point
    // with a value here would require building the entire graph structure
    while !grid.current.iter().any(|&x| grid.get(x).unwrap().clone() == Val::End) {
        grid.step(false);
    }
    println!("{}", grid.num_steps);
}

fn part_two() {
    let mut grid = Grid::parse(include_str!("../day-12-data.txt"));
    while !grid.current.iter().any(|&x| grid.get(x).unwrap().clone().height() == 0) {
        grid.step(true);
    }

    println!("{}", grid.num_steps);
}
