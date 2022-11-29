use std::fs;

fn main() {
    let filepath = "./day-05-data.txt";
    let input = parse_input(filepath);
    let mut part_one_nice_count = 0;
    let mut part_two_nice_count = 0;
    for line in input {
        if part_one_is_nice(&line) {
            part_one_nice_count += 1;
        }
        if part_two_is_nice(&line) {
            part_two_nice_count += 1;
        }
    }

    println!("Part one: {}", part_one_nice_count);
    println!("Part two: {}", part_two_nice_count);
}

fn parse_input(filepath: &str) -> Vec<String> {
    let contents = fs::read_to_string(filepath).unwrap();
    contents.trim().split('\n').map(|l| l.to_string()).collect()
}

fn part_one_is_nice(line: &String) -> bool {
    let bad_strings = ["ab", "cd", "pq", "xy"];
    for bad_string in bad_strings {
        if line.contains(bad_string) {
            return false;
        }
    }

    let vowels = ['a', 'e', 'i', 'o', 'u'];
    let mut vowel_count = 0;
    let mut has_double = false;
    for (i, c) in line.chars().enumerate() {
        if !has_double && i < line.len() - 1 && c == line[i + 1..i + 2].chars().nth(0).unwrap() {
            has_double = true;
        }
        if vowels.contains(&c) {
            vowel_count += 1;
        }
    }

    vowel_count >= 3 && has_double
}

fn part_two_is_nice(line: &String) -> bool {
    let mut non_overlapping_double = false;
    let mut apart_by_one = false;

    for (i, c) in line.chars().enumerate() {
        if !apart_by_one && i < line.len() - 2 && c == line[i + 2..i + 3].chars().nth(0).unwrap() {
            apart_by_one = true;
        }

        if !non_overlapping_double && i < line.len() - 3 {
            let current = &line[i..i + 2];
            for j in i + 2..line.len() - 1 {
                let temp = &line[j..j + 2];
                if current == temp {
                    non_overlapping_double = true
                }
            }
        }
    }

    non_overlapping_double && apart_by_one
}

#[cfg(test)]
mod part_one_tests {
    use super::*;

    #[test]
    fn ugknbfddgicrmopn() {
        assert!(part_one_is_nice("ugknbfddgicrmopn".to_string()));
    }

    #[test]
    fn aaa() {
        assert!(part_one_is_nice("aaa".to_string()));
    }

    #[test]
    fn jchzalrnumimnmhp() {
        assert!(!part_one_is_nice("jchzalrnumimnmhp".to_string()))
    }

    #[test]
    fn haegwjzuvuyypxyu() {
        assert!(!part_one_is_nice("haegwjzuvuyypxyu".to_string()))
    }

    #[test]
    fn dvszwmarrgswjxmb() {
        assert!(!part_one_is_nice("dvszwmarrgswjxmb".to_string()))
    }
}

#[cfg(test)]
mod part_two_tests {
    use super::*;

    #[test]
    fn qjhvhtzxzqqjkmpb() {
        assert!(part_two_is_nice("qjhvhtzxzqqjkmpb".to_string()))
    }

    #[test]
    fn xxyxx() {
        assert!(part_two_is_nice("xxyxx".to_string()))
    }

    #[test]
    fn uurcxstgmygtbstg() {
        assert!(!part_two_is_nice("uurcxstgmygtbstg".to_string()))
    }

    #[test]
    fn ieodomkazucvgmuy() {
        assert!(!part_two_is_nice("ieodomkazucvgmuy".to_string()))
    }
}
