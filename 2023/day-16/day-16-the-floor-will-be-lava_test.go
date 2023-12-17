package main

import "testing"

func TestPartOne(t *testing.T) {
	sample := `.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....`
	tests := map[string]struct {
		fn       func(string) int
		expected int
	}{
		"Part One": {
			fn:       PartOne,
			expected: 46,
		},
		"Part Two": {
			fn:       PartTwo,
			expected: 51,
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
