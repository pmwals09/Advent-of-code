package main

import "testing"

func TestPartOne(t *testing.T) {
  tests := map[string]struct{
    sample string
    expected int
  }{
    "2-step": {
      sample: `RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)`,
      expected: 2,
    },
    "6-step": {
      sample: `LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)`,
      expected: 6,
    },
  }
  for name, tc := range tests {
    t.Run(name, func(t *testing.T) {
      output := PartOne(tc.sample)
      if output != tc.expected {
        t.Errorf("Expected %d, got %d", tc.expected, output)
      }
    })
  }
}

func TestPartTwo(t *testing.T) {
  sample := `LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)`
  expected := 6
  output := PartTwo(sample)
  if output != expected {
    t.Errorf("Expected %d, got %d", expected, output)
  }
}
