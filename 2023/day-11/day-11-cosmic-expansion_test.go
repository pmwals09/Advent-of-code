package main

import "testing"

func TestPartOne(t *testing.T) {
	sample := `...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....`
	expected := 374
	output := PartOne(sample)
	if output != expected {
		t.Errorf("Expected %d, got %d", expected, output)
	}
}

func TestPartTwo(t *testing.T) {
	sample := `...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....`
	tests := map[string]struct {
		scale    int
		expected int
	}{
		"scale 10": {
			scale:    10,
			expected: 1030,
		},
		"scale 100": {
			scale:    100,
			expected: 8410,
		},
	}
  for name, test := range tests {
    t.Run(name, func(t *testing.T) {
      output := PartTwo(sample, test.scale - 1)
      if output!= test.expected {
        t.Errorf("Expected %d, got %d", test.expected, output)
      }
    })
  }
}
