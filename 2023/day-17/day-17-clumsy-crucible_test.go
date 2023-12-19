package main

import "testing"

func TestPartOne(t *testing.T) {
	sample := `2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533`

	tests := map[string]struct {
		fn       func(string) int
		expected int
	}{
		"Part One": {
			fn:       PartOne,
			expected: 102,
		},
		"Part Two": {
			fn:       PartTwo,
			expected: 94,
		},
	}
	for name, test := range tests {
		t.Run(name, func(t *testing.T) {
			output := test.fn(sample)
			if output != test.expected {
				t.Errorf("Expected %d, got %d", test.expected, output)
			}
		})
	}
}
