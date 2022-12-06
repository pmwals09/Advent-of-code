use std::{
    collections::{HashSet, VecDeque},
    fs,
    hash::Hash,
};

struct FiniteQueue<T> {
    size_limit: usize,
    q: VecDeque<T>,
}

impl<T: Eq + Hash> FiniteQueue<T> {
    fn new(size_limit: usize) -> Self {
        Self {
            size_limit,
            q: VecDeque::new(),
        }
    }

    fn enq(&mut self, item: T) -> Option<T> {
        if self.q.len() < self.size_limit {
            self.q.push_back(item);
            None
        } else {
            self.q.push_back(item);
            self.q.pop_front()
        }
    }

    fn all_unique(&self) -> bool {
        let hs: HashSet<&T> = HashSet::from_iter(self.q.iter());
        hs.len() == self.q.len()
    }

    fn full(&self) -> bool {
        self.q.len() == self.size_limit
    }
}

fn main() {
    let file_name = "day-06-data.txt";
    let contents = fs::read_to_string(file_name).unwrap();
    let input = contents.trim();
    let (part_one, part_two) = get_first_marker(input);

    println!("Part one: {}", part_one);
    println!("Part two: {}", part_two);
}

fn get_first_marker(input: &str) -> (usize, usize) {
    let mut part_one_q: FiniteQueue<char> = FiniteQueue::new(4);
    let mut part_two_q: FiniteQueue<char> = FiniteQueue::new(14);
    let mut res = (0, 0);
    for (i, c) in input.chars().enumerate() {
        part_one_q.enq(c);
        part_two_q.enq(c);
        if part_one_q.all_unique() && part_one_q.full() && res.0 == 0 {
            res.0 = i + 1;
        }
        if part_two_q.all_unique() && part_two_q.full() && res.1 == 0 {
            res.1 = i + 1;
        }
    }
    res
}

#[cfg(test)]
mod part_one_tests {
    use super::*;

    #[test]
    fn mjqjpqmgbljsphdztnvjfqwrcgsmlb() {
        assert_eq!(get_first_marker("mjqjpqmgbljsphdztnvjfqwrcgsmlb").0, 7)
    }

    #[test]
    fn bvwbjplbgvbhsrlpgdmjqwftvncz() {
        assert_eq!(get_first_marker("bvwbjplbgvbhsrlpgdmjqwftvncz").0, 5)
    }

    #[test]
    fn nppdvjthqldpwncqszvftbrmjlhg() {
        assert_eq!(get_first_marker("nppdvjthqldpwncqszvftbrmjlhg").0, 6)
    }

    #[test]
    fn nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg() {
        assert_eq!(get_first_marker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg").0, 10)
    }

    #[test]
    fn zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw() {
        assert_eq!(get_first_marker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw").0, 11)
    }
}

#[cfg(test)]
mod part_two_tests {
    use super::*;

    #[test]
    fn mjqjpqmgbljsphdztnvjfqwrcgsmlb() {
        assert_eq!(get_first_marker("mjqjpqmgbljsphdztnvjfqwrcgsmlb").1, 19)
    }

    #[test]
    fn bvwbjplbgvbhsrlpgdmjqwftvncz() {
        assert_eq!(get_first_marker("bvwbjplbgvbhsrlpgdmjqwftvncz").1, 23)
    }

    #[test]
    fn nppdvjthqldpwncqszvftbrmjlhg() {
        assert_eq!(get_first_marker("nppdvjthqldpwncqszvftbrmjlhg").1, 23)
    }

    #[test]
    fn nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg() {
        assert_eq!(get_first_marker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg").1, 29)
    }

    #[test]
    fn zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw() {
        assert_eq!(get_first_marker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw").1, 26)
    }
}
