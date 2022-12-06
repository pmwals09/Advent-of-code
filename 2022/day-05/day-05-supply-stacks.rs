use std::fs;
use std::num::ParseIntError;
use std::path::PathBuf;
use std::str::FromStr;

#[derive(Debug, Clone)]
struct Supplies {
    stacks: Vec<Vec<char>>,
}

impl Supplies {
    fn from_stacks(stacks: Vec<String>) -> Self {
        let stacks = stacks
            .iter()
            .map(|s| {
                let mut trimmed_chars: Vec<char> = s.trim().chars().collect();
                trimmed_chars.reverse();
                trimmed_chars
            })
            .collect();

        Self { stacks }
    }

    fn move_item(&mut self, instruction: &Instruction) {
        for _ in 0..instruction.quantity {
            let item = self.stacks[instruction.from_index].pop().unwrap();
            self.stacks[instruction.to_index].push(item);
        }
    }

    fn get_top_boxes(&self) -> String {
        let mut res = String::new();
        for stack in &self.stacks {
            res.push(stack[stack.len() - 1])
        }

        res
    }

    fn move_item_group(&mut self, instruction: &Instruction) {
        let from_stack = &mut self.stacks[instruction.from_index];
        let sub_stack: Vec<char> = from_stack
            .drain(from_stack.len() as usize - instruction.quantity as usize..)
            .collect();
        self.stacks[instruction.to_index].append(&mut sub_stack.clone());
    }
}

#[derive(Debug)]
struct Instruction {
    quantity: i32,
    from_index: usize,
    to_index: usize,
}

impl FromStr for Instruction {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (front, to_name) = s.split_once(" to ").unwrap();
        let (front, from_name) = front.split_once(" from ").unwrap();
        let (_, quantity_str) = front.split_once(' ').unwrap();

        let to_index = to_name.parse::<usize>()?;
        let from_index = from_name.parse::<usize>()?;
        let quantity = quantity_str.parse::<i32>()?;

        Ok(Self {
            quantity,
            from_index: from_index - 1,
            to_index: to_index - 1,
        })
    }
}

fn main() {
    let file_path = PathBuf::from_str("./day-05-data.txt").unwrap();
    let (mut layout, instructions) = parse_input(file_path);
    let mut part_two_layout = layout.clone();

    for instruction in instructions {
        layout.move_item(&instruction);
        part_two_layout.move_item_group(&instruction);
    }

    println!("Part one: {}", layout.get_top_boxes());
    println!("Part two: {}", part_two_layout.get_top_boxes());
}

fn parse_input(file_path: PathBuf) -> (Supplies, Vec<Instruction>) {
    let contents = fs::read_to_string(file_path).unwrap();
    let (init_layout, instructions) = contents.split_once("\n\n").unwrap();

    let supplies = parse_supplies(init_layout);
    let instructions = parse_instructions(instructions);

    (supplies, instructions)
}

fn parse_supplies(layout: &str) -> Supplies {
    let mut layout_lines: Vec<&str> = layout.lines().collect();
    let columns = layout_lines.pop().unwrap();
    let num_columns = columns
        .split("   ")
        .map(|n| n.trim().parse::<i32>().unwrap())
        .max()
        .unwrap();
    let mut stacks: Vec<String> = Vec::new();
    for _ in 0..num_columns {
        stacks.push(String::new())
    }
    for line in layout_lines {
        for i in (1..line.len()).step_by(4) {
            let stack_index = (i - 1) / 4;
            let c = line.chars().nth(i).unwrap();
            stacks[stack_index].push(c);
        }
    }

    Supplies::from_stacks(stacks)
}

fn parse_instructions(instructions: &str) -> Vec<Instruction> {
    let instruction_lines: Vec<&str> = instructions.lines().collect();
    let mut instructions: Vec<Instruction> = Vec::new();
    for line in instruction_lines {
        instructions.push(Instruction::from_str(line).unwrap())
    }

    instructions
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_INPUT: &'static str = "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2";

    #[test]
    fn part_one_test() {
        let (init_layout, instructions) = TEST_INPUT.split_once("\n\n").unwrap();

        let mut supplies = parse_supplies(init_layout);
        let instructions = parse_instructions(instructions);

        for instruction in instructions {
            supplies.move_item(&instruction);
        }

        assert_eq!(supplies.get_top_boxes(), "CMZ".to_string())
    }

    #[test]
    fn part_two_test() {
        let (init_layout, instructions) = TEST_INPUT.split_once("\n\n").unwrap();

        let mut supplies = parse_supplies(init_layout);
        let instructions = parse_instructions(instructions);

        for instruction in instructions {
            supplies.move_item_group(&instruction);
        }

        assert_eq!(supplies.get_top_boxes(), "MCD".to_string())
    }
}
