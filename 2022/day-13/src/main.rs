#[derive(Debug)]
enum Token {
    LBracket,
    RBracket,
    Num(u8),
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum Item {
    List(Vec<Item>),
    Num(u8),
}

impl Item {
    fn to_vec(&self) -> Vec<Item> {
        match self {
            Self::List(l) => l.to_vec(),
            Self::Num(n) => vec![Self::Num(n.clone())],
        }
    }
}

impl PartialOrd for Item {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Item::Num(n), Item::Num(o)) => n.partial_cmp(o),
            (left, right) => {
                let left = left.to_vec();
                let right = right.to_vec();
                left.partial_cmp(&right)
            }
        }
    }
}

impl Ord for Item {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

#[derive(Debug)]
struct Scanner {
    cursor: usize,
    characters: Vec<char>,
    tokens: Vec<Token>,
}

impl Scanner {
    fn new(str: &str) -> Self {
        Self {
            cursor: 0,
            characters: str.chars().collect(),
            tokens: Vec::new(),
        }
    }

    fn peek(&self) -> Option<&char> {
        self.characters.get(self.cursor)
    }

    fn pop(&mut self) -> Option<&char> {
        match self.characters.get(self.cursor) {
            Some(c) => {
                self.cursor += 1;

                Some(c)
            }
            None => None,
        }
    }

    fn tokenize(&mut self) {
        while let Some(c) = self.peek() {
            match *c {
                '[' => {
                    self.tokens.push(Token::LBracket);
                    self.pop();
                }
                ']' => {
                    self.tokens.push(Token::RBracket);
                    self.pop();
                }
                c if c.is_digit(10) => {
                    let n = self.number();
                    self.tokens.push(Token::Num(n));
                    // no need to pop the char off; handled in number
                }
                _ => {
                    self.pop();
                    Some(());
                }
            }
        }
    }

    fn number(&mut self) -> u8 {
        let mut n = 0u8;
        while self.peek().unwrap().is_digit(10) {
            n *= 10;
            n += *self.pop().unwrap() as u8 - 48;
        }
        n
    }
}

struct Parser {
    tokens: Vec<Token>,
    idx: usize,
    out: Option<Item>,
}

impl Parser {
    fn from_tokens(t: Vec<Token>) -> Self {
        Self {
            tokens: t,
            idx: 0,
            out: None,
        }
    }

    fn parse(&mut self) {
        self.advance();
        self.out = Some(self.list());
    }

    fn list(&mut self) -> Item {
        let mut items: Vec<Item> = Vec::new();
        while !self.is_done() {
            match self.pop() {
                Some(t) => match t {
                    Token::LBracket => items.push(self.list()),
                    Token::Num(n) => items.push(Item::Num(*n)),
                    Token::RBracket => return Item::List(items),
                },
                None => {}
            }
        }
        return Item::List(items);
    }

    fn advance(&mut self) {
        self.idx += 1;
    }

    fn is_done(&self) -> bool {
        self.idx == self.tokens.len()
    }

    fn pop(&mut self) -> Option<&Token> {
        match self.tokens.get(self.idx) {
            Some(t) => {
                self.idx += 1;

                Some(t)
            }
            None => None,
        }
    }
}

fn main() {
    let mut total = 0;
    for (i, pair) in include_str!("../day-13-data.txt").split("\n\n").enumerate() {
        let mut packets = pair.lines();

        let mut scanner = Scanner::new(packets.next().unwrap());
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        let left = parser.out.unwrap();

        let mut scanner = Scanner::new(packets.next().unwrap());
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        let right = parser.out.unwrap();

        if left < right {
            total += i + 1;
        }
    }

    let dividers = vec![
        Item::List(vec![
            Item::List(vec![
                Item::Num(2)
            ])
        ]),
        Item::List(vec![
            Item::List(vec![
                Item::Num(6)
            ])
        ]),
    ];
    let mut packets: Vec<Item> = include_str!("../day-13-data.txt")
        .split('\n')
        .filter(|l| !l.is_empty())
        .map(|p| {
            let mut scanner = Scanner::new(p);
            scanner.tokenize();
            let mut parser = Parser::from_tokens(scanner.tokens);
            parser.parse();
            parser.out.unwrap()
        })
        .chain(dividers.iter().cloned())
        .collect();

    packets.sort();

    let part_two = dividers.iter().map(|d| packets.binary_search(d).unwrap() + 1).product::<usize>();

    println!("Part 1: {total}");
    println!("Part 2: {part_two}");
}

#[cfg(test)]
mod test {
    use std::cmp::Ordering;

    use super::*;

    #[test]
    fn parse_1() {
        let item = "[1,1,3,1,1]";
        let mut scanner = Scanner::new(item);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        assert_eq!(
            Item::List(vec![
                Item::Num(1),
                Item::Num(1),
                Item::Num(3),
                Item::Num(1),
                Item::Num(1),
            ]),
            parser.out.unwrap()
        );
    }

    #[test]
    fn parse_2() {
        let item = "[1,1,5,1,1]";
        let mut scanner = Scanner::new(item);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        assert_eq!(
            Item::List(vec![
                Item::Num(1),
                Item::Num(1),
                Item::Num(5),
                Item::Num(1),
                Item::Num(1),
            ]),
            parser.out.unwrap()
        );
    }

    #[test]
    fn parse_3() {
        let item = "[[1],[2,3,4]]";
        let mut scanner = Scanner::new(item);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        assert_eq!(
            Item::List(vec![
                Item::List(vec![Item::Num(1),]),
                Item::List(vec![Item::Num(2), Item::Num(3), Item::Num(4),])
            ]),
            parser.out.unwrap()
        );
    }

    #[test]
    fn parse_4() {
        let item = "[[1],4]";
        let mut scanner = Scanner::new(item);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        assert_eq!(
            Item::List(vec![Item::List(vec![Item::Num(1),]), Item::Num(4),]),
            parser.out.unwrap()
        );
    }

    #[test]
    fn parse_5() {
        let item = "[9]";
        let mut scanner = Scanner::new(item);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        assert_eq!(Item::List(vec![Item::Num(9),]), parser.out.unwrap());
    }

    #[test]
    fn parse_6() {
        let item = "[[8,7,6]]";
        let mut scanner = Scanner::new(item);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        assert_eq!(
            Item::List(vec![Item::List(vec![
                Item::Num(8),
                Item::Num(7),
                Item::Num(6),
            ]),]),
            parser.out.unwrap()
        );
    }

    #[test]
    fn parse_7() {
        let item = "[[4,4],4,4]";
        let mut scanner = Scanner::new(item);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        assert_eq!(
            Item::List(vec![
                Item::List(vec![Item::Num(4), Item::Num(4),]),
                Item::Num(4),
                Item::Num(4),
            ]),
            parser.out.unwrap()
        );
    }

    #[test]
    fn parse_8() {
        let item = "[[4,4],4,4,4]";
        let mut scanner = Scanner::new(item);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        assert_eq!(
            Item::List(vec![
                Item::List(vec![Item::Num(4), Item::Num(4),]),
                Item::Num(4),
                Item::Num(4),
                Item::Num(4),
            ]),
            parser.out.unwrap()
        );
    }

    #[test]
    fn parse_9() {
        let item = "[7,7,7,7]";
        let mut scanner = Scanner::new(item);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        assert_eq!(
            Item::List(vec![Item::Num(7), Item::Num(7), Item::Num(7), Item::Num(7),]),
            parser.out.unwrap()
        );
    }

    #[test]
    fn parse_10() {
        let item = "[7,7,7]";
        let mut scanner = Scanner::new(item);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        assert_eq!(
            Item::List(vec![Item::Num(7), Item::Num(7), Item::Num(7),]),
            parser.out.unwrap()
        );
    }

    #[test]
    fn parse_11() {
        let item = "[]";
        let mut scanner = Scanner::new(item);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        assert_eq!(Item::List(vec![]), parser.out.unwrap());
    }

    #[test]
    fn parse_12() {
        let item = "[3]";
        let mut scanner = Scanner::new(item);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        assert_eq!(Item::List(vec![Item::Num(3)]), parser.out.unwrap());
    }

    #[test]
    fn parse_13() {
        let item = "[[[]]]";
        let mut scanner = Scanner::new(item);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        assert_eq!(
            Item::List(vec![Item::List(vec![Item::List(vec![]),]),]),
            parser.out.unwrap()
        );
    }

    #[test]
    fn parse_14() {
        let item = "[[]]";
        let mut scanner = Scanner::new(item);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        assert_eq!(Item::List(vec![Item::List(vec![]),]), parser.out.unwrap());
    }

    #[test]
    fn parse_15() {
        let item = "[1,[2,[3,[4,[5,6,7]]]],8,9]";
        let mut scanner = Scanner::new(item);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        assert_eq!(
            Item::List(vec![
                Item::Num(1),
                Item::List(vec![
                    Item::Num(2),
                    Item::List(vec![
                        Item::Num(3),
                        Item::List(vec![
                            Item::Num(4),
                            Item::List(vec![Item::Num(5), Item::Num(6), Item::Num(7),])
                        ])
                    ])
                ]),
                Item::Num(8),
                Item::Num(9),
            ]),
            parser.out.unwrap()
        );
    }

    #[test]
    fn parse_16() {
        let item = "[1,[2,[3,[4,[5,6,0]]]],8,9]";
        let mut scanner = Scanner::new(item);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        assert_eq!(
            Item::List(vec![
                Item::Num(1),
                Item::List(vec![
                    Item::Num(2),
                    Item::List(vec![
                        Item::Num(3),
                        Item::List(vec![
                            Item::Num(4),
                            Item::List(vec![Item::Num(5), Item::Num(6), Item::Num(0),])
                        ])
                    ])
                ]),
                Item::Num(8),
                Item::Num(9),
            ]),
            parser.out.unwrap()
        );
    }

    #[test]
    fn pair_1() {
        let pair = "[1,1,3,1,1]\n[1,1,5,1,1]";
        let (left, right) = pair.split_once('\n').unwrap();
        let mut scanner = Scanner::new(left);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        let left = parser.out.unwrap();
        let mut scanner = Scanner::new(right);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        let right = parser.out.unwrap();
        assert_eq!(Ordering::Less, left.cmp(&right))
    }

    #[test]
    fn pair_2() {
        let pair = "[[1],[2,3,4]]\n[[1],4]";
        let (left, right) = pair.split_once('\n').unwrap();
        let mut scanner = Scanner::new(left);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        let left = parser.out.unwrap();
        let mut scanner = Scanner::new(right);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        let right = parser.out.unwrap();
        assert_eq!(Ordering::Less, left.cmp(&right))
    }

    #[test]
    fn pair_3() {
        let pair = "[9]\n[[8,7,6]]";
        let (left, right) = pair.split_once('\n').unwrap();
        let mut scanner = Scanner::new(left);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        let left = parser.out.unwrap();
        let mut scanner = Scanner::new(right);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        let right = parser.out.unwrap();
        assert_eq!(Ordering::Greater, left.cmp(&right))
    }

    #[test]
    fn pair_4() {
        let pair = "[[4,4],4,4]\n[[4,4],4,4,4]";
        let (left, right) = pair.split_once('\n').unwrap();
        let mut scanner = Scanner::new(left);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        let left = parser.out.unwrap();
        let mut scanner = Scanner::new(right);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        let right = parser.out.unwrap();
        assert_eq!(Ordering::Less, left.cmp(&right))
    }

    #[test]
    fn pair_5() {
        let pair = "[7,7,7,7]\n[7,7,7]";
        let (left, right) = pair.split_once('\n').unwrap();
        let mut scanner = Scanner::new(left);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        let left = parser.out.unwrap();
        let mut scanner = Scanner::new(right);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        let right = parser.out.unwrap();
        assert_eq!(Ordering::Greater, left.cmp(&right))
    }

    #[test]
    fn pair_6() {
        let pair = "[]\n[3]";
        let (left, right) = pair.split_once('\n').unwrap();
        let mut scanner = Scanner::new(left);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        let left = parser.out.unwrap();
        let mut scanner = Scanner::new(right);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        let right = parser.out.unwrap();
        assert_eq!(Ordering::Less, left.cmp(&right))
    }

    #[test]
    fn pair_7() {
        let pair = "[[[]]]\n[[]]";
        let (left, right) = pair.split_once('\n').unwrap();
        let mut scanner = Scanner::new(left);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        let left = parser.out.unwrap();
        let mut scanner = Scanner::new(right);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        let right = parser.out.unwrap();
        assert_eq!(Ordering::Greater, left.cmp(&right))
    }

    #[test]
    fn pair_8() {
        let pair = "[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]";
        let (left, right) = pair.split_once('\n').unwrap();
        let mut scanner = Scanner::new(left);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        let left = parser.out.unwrap();
        let mut scanner = Scanner::new(right);
        scanner.tokenize();
        let mut parser = Parser::from_tokens(scanner.tokens);
        parser.parse();
        let right = parser.out.unwrap();
        assert_eq!(Ordering::Greater, left.cmp(&right))
    }
}
