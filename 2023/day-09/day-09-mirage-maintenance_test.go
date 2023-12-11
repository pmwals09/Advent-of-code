package main

import "testing"

func TestParts(t *testing.T) {
  sample := `0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45`
  tests := map[string]struct{
    fn func(string)int
    expected int
  }{
    "Part One": {
      fn: PartOne,
      expected: 114,
    },
    "Part Two": {
      fn: PartTwo,
      expected: 2,
    },
  }

  for name, test := range tests {
    t.Run(name, func(t *testing.T) {
      output := test.fn(sample)
      if output!= test.expected {
        t.Errorf("Expected %d, got %d", test.expected, output)
      }
    })
  }
}
