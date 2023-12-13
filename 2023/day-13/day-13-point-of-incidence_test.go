package main

import (
	"testing"
)

func TestParts(t *testing.T) {
	sample := `#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#`
  tests := map[string]struct{
    fn func(string)int
    expected int
  }{
    "Part One": {
      fn: PartOne,
      expected: 405,
    },
    "Part Two": {
      fn: PartTwo,
      expected: 400,
    },
  }
  for name, test := range tests {
    t.Run(name, func(t *testing.T) {
      output := test.fn(sample)
      if output != test.expected {
        t.Errorf("Expected %v, received %v", test.expected, output)
      }
    })
  }
}
