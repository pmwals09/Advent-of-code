use std::{collections::VecDeque, fmt::Error, fs, path::PathBuf, str::FromStr};

#[derive(Debug)]
struct Cursor {
    row_idx: usize,
    col_idx: usize
}

impl Cursor {
    fn from_idx(row_idx: usize, col_idx: usize) -> Self {
        Self {
            row_idx,
            col_idx
        }
    }
}

#[derive(Debug)]
struct Node {
    shortest_path: Option<i32>,
    val: char,
    visited: bool,
    row_idx: Option<usize>,
    col_idx: Option<usize>,
}

impl Node {
    fn new() -> Self {
        Self {
            shortest_path: None,
            val: ' ',
            visited: false,
            row_idx: None,
            col_idx: None,
        }
    }

    fn from_c(c: char, row_idx: usize, col_idx: usize) -> Self {
        Self {
            shortest_path: None,
            val: c,
            visited: false,
            row_idx: Some(row_idx),
            col_idx: Some(col_idx)
        }
    }
}

struct Graph {
    g: Vec<Vec<Node>>,
    // start: Node,
    // end: Node
}

impl Graph {
    fn new() -> Self {
        Self {
            g: Vec::new(),
            // start: Node::new(),
            // end: Node::new()
        }
    }

    fn val_at_idx(&self, row_idx: usize, col_idx: usize) -> char {
        self.g[row_idx][col_idx].val
    }
}

fn main() {
    let file_path = PathBuf::from("./day-12-data.txt");
    let contents = fs::read_to_string(file_path).unwrap();
    let mut g = build_graph_from_contents(&contents);
    let mut to_visit_next: VecDeque<Cursor> = VecDeque::new();

    for (row_idx, row) in g.g.iter().enumerate() {
        for (col_idx, n) in row.iter().enumerate() {
            match n.val {
                'S' => {
                    add_neighbors(row_idx, col_idx, &g, &mut to_visit_next);
                }
                _ => {}
            }
        }

    }

    while let Some(cursor) = to_visit_next.pop_front() {
        let mut n = &mut g.g[cursor.row_idx][cursor.col_idx];
        n.visited = true;

        let shortest_path = n.shortest_path;
        // doesn't this need to compare the source node's path?
        n.shortest_path = match shortest_path {
            None => Some(1),
            Some(amt) => Some(amt + 1)
        };

        // do I only add neighbors when I haven't already visited this node before?
        add_neighbors(cursor.row_idx, cursor.col_idx, &g, &mut to_visit_next);
    }
}

fn add_neighbors<'a>(row_idx: usize, col_idx: usize, graph: &'a Graph, node_queue: &mut VecDeque<Cursor>) {
    let current_val = graph.val_at_idx(row_idx, col_idx);
    if row_idx > 0 && graph.val_at_idx(row_idx - 1, col_idx) <= char::from_u32(current_val as u32 + 1).unwrap() {
        node_queue.push_back(Cursor::from_idx(row_idx - 1, col_idx));
    }
    if row_idx < graph.g.len() - 1 && graph.val_at_idx(row_idx - 1, col_idx) <= char::from_u32(current_val as u32 + 1).unwrap() {
        node_queue.push_back(Cursor::from_idx(row_idx + 1, col_idx));
    }
    if col_idx > 0 && graph.val_at_idx(row_idx, col_idx - 1) <= char::from_u32(current_val as u32 + 1).unwrap() {
        node_queue.push_back(Cursor::from_idx(row_idx, col_idx - 1));
    }
    if col_idx < graph.g[row_idx].len() - 1 && graph.val_at_idx(row_idx, col_idx + 1) <= char::from_u32(current_val as u32 + 1).unwrap(){
        node_queue.push_back(Cursor::from_idx(row_idx, col_idx + 1));
    }
}

fn build_graph_from_contents(contents: &str) -> Graph {
    let lines = contents.trim().lines();
    let mut g = Graph::new();
    for (row_idx, line) in lines.enumerate() {
        let mut g_line = Vec::new();
        for (col_idx, c) in line.chars().enumerate() {
            let node = Node::from_c(c, row_idx, col_idx);
            g_line.push(node);
        }
        g.g.push(g_line);
    }
    g
}
