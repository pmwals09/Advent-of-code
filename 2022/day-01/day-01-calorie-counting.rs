use std::fs;

fn main() {
    let file_path = "./day-01-input.txt";
    let input = parse_input(file_path);

    println!("Part one: {}", get_max_calories(&input)[0]);
    println!("Part two: {}", get_max_calories(&input)[0..3].iter().sum::<i32>());
}

fn parse_input(file_path: &str) -> Vec<String> {
    let contents = fs::read_to_string(file_path).unwrap();
    contents
        .trim()
        .split("\n\n")
        .map(|ea| ea.to_string())
        .collect()
}

fn get_max_calories(input: &Vec<String>) -> Vec<i32> {
    let mut v = input
        .iter()
        .map(|elf| {
            let mut elf_total = 0;
            for food_item in elf.split('\n') {
                elf_total += food_item.parse::<i32>().unwrap();
            }
            elf_total
        })
        .collect::<Vec<i32>>();
    v.sort();
    v.reverse();
    v
}

#[cfg(test)]
mod part_one_tests {
    use super::*;

    #[test]
    fn part_one_example() {
        let test_input = vec![
            "1000\n2000\n3000",
            "4000",
            "5000\n6000",
            "7000\n8000\n9000",
            "10000",
        ]
            .iter()
            .map(|x| x.to_string())
            .collect();
        assert_eq!(get_max_calories(&test_input)[0], 24000);
    }
}

#[cfg(test)]
mod part_two_tests {
    use super::*;

    #[test]
    fn part_two_example() {
        let test_input = vec![
            "1000\n2000\n3000",
            "4000",
            "5000\n6000",
            "7000\n8000\n9000",
            "10000",
        ]
            .iter()
            .map(|x| x.to_string())
            .collect();
        assert_eq!(get_max_calories(&test_input)[0..3].iter().sum::<i32>(), 45000)
    }
}
