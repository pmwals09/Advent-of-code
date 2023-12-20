package main

import "testing"

func TestParts(t *testing.T) {
	sample := `R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)`
	tests := map[string]struct {
		fn       func(string) int
		expected int
	}{
		"Part One": {
			fn:       PartOne,
			expected: 62,
		},
		"Part Two": {
			fn:       PartTwo,
			expected: 952408144115,
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
