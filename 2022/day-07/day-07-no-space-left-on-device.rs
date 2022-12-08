use std::{collections::HashMap, fs, path::PathBuf};

struct FileTree<'a> {
    file_tree: HashMap<String, u32>,
    pwd: Vec<&'a str>,
}

impl<'a> FileTree<'a> {
    fn new() -> Self {
        Self {
            file_tree: HashMap::new(),
            pwd: Vec::new(),
        }
    }

    fn read_line(&mut self, line: &'a str) {
        let line_parts: Vec<&str> = line.split(' ').collect();
        if line_parts[0] == "$" {
            if line_parts[1] == "cd" {
                if line_parts[2] == ".." {
                    self.pwd.pop().unwrap();
                } else {
                    self.pwd.push(line_parts[2]);
                }
            }
        } else if line_parts[0] != "dir" {
            let size: u32 = line_parts[0].parse().unwrap();
            let mut path = String::from("/");
            for i in 0..self.pwd.len() {
                if i > 0 {
                    path += &(String::from(self.pwd[i]) + "/");
                }
                self.file_tree
                    .entry(String::from(&path))
                    .and_modify(|x| *x += size)
                    .or_insert(size);
            }
        }
    }

    fn get_directory_size(&self, target_dir_name: &str) -> u32 {
        let mut total = 0;
        if target_dir_name == "/" {
            return *self.file_tree.get("/").unwrap();
        }
        for path in self.file_tree.keys() {
            if path.ends_with(&String::from(target_dir_name)) {
                total += self.file_tree.get(path).unwrap();
            }
        }

        total
    }
}

fn main() {
    let file_path = PathBuf::from("./day-07-data.txt");
    let contents = fs::read_to_string(file_path).unwrap();
    let input = contents.trim().lines();

    let mut ft = FileTree::new();
    for line in input {
        ft.read_line(line)
    }

    let mut total = 0;
    let mut viable_sizes = Vec::new();

    let unused = 70000000 - ft.get_directory_size("/");
    let to_free = 30000000 - unused;
    for dir_path in ft.file_tree.keys() {
        let dir_size = ft.get_directory_size(dir_path);
        if dir_size < 100000 {
            total += dir_size;
        }
        if dir_size >= to_free {
            viable_sizes.push(dir_size)
        }
    }

    println!("Part one: {}", total);
    println!("Part two: {}", viable_sizes.iter().min().unwrap());
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_INPUT: &str = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k";

    #[test]
    fn e() {
        let mut ft = FileTree::new();
        for line in TEST_INPUT.trim().lines() {
            ft.read_line(line)
        }

        assert_eq!(ft.get_directory_size("e/"), 584)
    }

    #[test]
    fn a() {
        let mut ft = FileTree::new();
        for line in TEST_INPUT.trim().lines() {
            ft.read_line(line)
        }

        assert_eq!(ft.get_directory_size("a/"), 94853)
    }

    #[test]
    fn d() {
        let mut ft = FileTree::new();
        for line in TEST_INPUT.trim().lines() {
            ft.read_line(line)
        }

        assert_eq!(ft.get_directory_size("d/"), 24933642)
    }

    #[test]
    fn slash() {
        let mut ft = FileTree::new();
        for line in TEST_INPUT.trim().lines() {
            ft.read_line(line)
        }

        assert_eq!(ft.get_directory_size("/"), 48381165)
    }

    #[test]
    fn all_under_100000() {
        let mut ft = FileTree::new();
        for line in TEST_INPUT.trim().lines() {
            ft.read_line(line)
        }

        let mut total = 0;
        for key in ft.file_tree.keys() {
            let dir_size = ft.get_directory_size(key);
            if dir_size <= 100000 {
                total += dir_size;
            }
        }

        assert_eq!(total, 95437)
    }

    #[test]
    fn part_two() {
        let mut ft = FileTree::new();
        for line in TEST_INPUT.trim().lines() {
            ft.read_line(line)
        }

        let root = ft.get_directory_size("/");
        let unused = 70000000 - root;
        let to_free = 30000000 - unused;
        let mut candidates = Vec::new();
        for key in ft.file_tree.keys() {
            let dir_size = ft.get_directory_size(key);
            if dir_size >= to_free {
                candidates.push(dir_size)
            }
        }

        assert_eq!(*candidates.iter().min().unwrap(), 24933642 as u32)
    }
}
