use std::{fs, path::PathBuf};

fn main() {
    let file_path = PathBuf::from("./day-08-data.txt");
    let contents = fs::read_to_string(file_path).unwrap();
    let lines: Vec<&str> = contents.lines().collect();
    let part_one = count_visible(&lines);
    let part_two = get_max_scenic_score(&lines);
    println!("Part one: {}", part_one);
    println!("Part two: {}", part_two);
}

fn count_visible(lines: &Vec<&str>) -> u32 {
    let col_len = &lines.len();
    let row_len = lines[0].len();
    let mut num_visible = 0;
    let mut left_max = 0;
    let mut top_max = vec![0; row_len];
    for (row_idx, line) in lines.iter().enumerate() {
        for (col_idx, c) in line.chars().enumerate() {
            let val = c.to_digit(10).unwrap();
            let col = lines
                .iter()
                .map(|l| &l[col_idx..col_idx + 1])
                .collect::<Vec<&str>>()
                .join("");

            if val > left_max || col_idx == 0 {
                left_max = val;
                if val > top_max[col_idx] {
                    top_max[col_idx] = val;
                }
                num_visible += 1;
                continue;
            }

            if val > top_max[col_idx] || row_idx == 0 {
                top_max[col_idx] = val;
                if val > left_max {
                    left_max = val;
                }
                num_visible += 1;
                continue;
            }

            if val > get_max(&line[col_idx + 1..])
                || val > get_max(&col[row_idx + 1..])
                || row_idx == row_len - 1
                || col_idx == col_len - 1
            {
                num_visible += 1;
                continue;
            }
        }
    }
    num_visible
}

fn get_max_scenic_score(lines: &Vec<&str>) -> u32 {
    let mut max_score = 0;
    for (row_idx, line) in lines.iter().enumerate() {
        for (col_idx, c) in line.chars().enumerate() {
            let mut scores = (0, 0, 0, 0);
            let val = c.to_digit(10).unwrap();
            let col = lines
                .iter()
                .map(|l| &l[col_idx..col_idx + 1])
                .collect::<Vec<&str>>();

            if row_idx == 0 {
                scores.0 = 0;
            } else {
                for (i, item) in col[..row_idx].iter().rev().enumerate() {
                    let item = item.chars().next().unwrap().to_digit(10).unwrap();
                    if item >= val {
                        scores.0 = i + 1;
                        break;
                    }
                }
                if scores.0 == 0 {
                    scores.0 = row_idx;
                }
            }

            if col_idx == line.len() - 1 {
                scores.1 = 0;
            } else {
                for (i, item) in line[col_idx + 1..].chars().enumerate() {
                    let item = item.to_digit(10).unwrap();
                    if item >= val {
                        scores.1 = i + 1;
                        break;
                    }
                }
                if scores.1 == 0 {
                    scores.1 = line.len() - 1 - col_idx;
                }
            }

            if row_idx == lines.len() - 1 {
                scores.2 = 0;
            } else {
                for (i, item) in col[row_idx + 1..].iter().enumerate() {
                    let item = item.chars().next().unwrap().to_digit(10).unwrap();
                    if item >= val {
                        scores.2 = i + 1;
                        break;
                    }
                }
                if scores.2 == 0 {
                    scores.2 = lines.len() - 1 - row_idx;
                }
            }

            if col_idx == 0 {
                scores.3 = 0;
            } else {
                for (i, item) in line[..col_idx].chars().rev().enumerate() {
                    let item = item.to_digit(10).unwrap();
                    if item >= val {
                        scores.3 = i + 1;
                        break;
                    }
                }
                if scores.3 == 0 {
                    scores.3 = col_idx;
                }
            }

            let c_score = scores.0 * scores.1 * scores.2 * scores.3;
            if c_score > max_score {
                max_score = c_score;
            }
        }
    }

    max_score as u32
}

fn get_max(line_portion: &str) -> u32 {
    let nums: Vec<u32> = line_portion
        .chars()
        .map(|c| c.to_digit(10).unwrap())
        .collect();

    match nums.iter().max() {
        Some(v) => *v,
        None => 0,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_INPUT: &str = "30373
25512
65332
33549
35390";

    #[test]
    fn part_one_test() {
        let content: Vec<&str> = TEST_INPUT.trim().lines().collect();
        assert_eq!(count_visible(&content), 21)
    }

    #[test]
    fn part_two_test() {
        let content: Vec<&str> = TEST_INPUT.trim().lines().collect();
        assert_eq!(get_max_scenic_score(&content), 8)
    }
}
