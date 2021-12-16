const INPUT: &str = include_str!("day-01-input.txt");

fn main() {
    let numbers = INPUT
        .trim()
        .lines()
        .map(|x| x.parse::<u32>().unwrap())
        .collect::<Vec<_>>();
    
    println!("Part one: {}", part_one(&numbers));
    println!("Part two: {}", part_two(&numbers));
}

fn part_one(numbers: &Vec<u32>) -> u32 {
    let mut ups = 0;
    for i in 0..numbers.len() {
        if i > 0 && numbers[i] > numbers[i - 1]{
            ups += 1;
        }
    }

    ups
}

fn part_two(numbers: &Vec<u32>) -> u32 {
  let mut ups = 0;
  for i in 0..numbers.len() {
    if i > 2 {
      let upper_sum: u32 = numbers[i - 2..i + 1].iter().sum();
      let lower_sum: u32 = numbers[i - 3..i].iter().sum();
      if upper_sum > lower_sum {
        ups += 1
      }
    }
  }

  ups
}

#[cfg(test)]
mod tests {
  use part_one;
  use part_two;

  #[test]
  fn test_part_one() {
        let numbers: Vec<u32> = vec![199, 200, 208, 210, 200, 207, 240, 269, 260, 263];
        let expected = 7;
        let res = part_one(&numbers);
        assert_eq!(expected, res);
  }

  #[test]
  fn test_part_two() {
        let numbers: Vec<u32> = vec![199, 200, 208, 210, 200, 207, 240, 269, 260, 263];
        let expected = 5;
        let res = part_two(&numbers);
        assert_eq!(expected, res);
    }
}