package main

import "testing"

func TestRace_RunRace(t *testing.T) {
	sample := Race{Time: 7, Distance: 9}
	tests := map[string]struct {
		holdTime int
		expected int
	}{
		"0 ms": {holdTime: 0, expected: 0},
		"1 ms": {holdTime: 1, expected: 6},
		"2 ms": {holdTime: 2, expected: 10},
		"3 ms": {holdTime: 3, expected: 12},
		"4 ms": {holdTime: 4, expected: 12},
		"5 ms": {holdTime: 5, expected: 10},
		"6 ms": {holdTime: 6, expected: 6},
		"7 ms": {holdTime: 7, expected: 0},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			output := sample.RunRace(tc.holdTime)
			if output != tc.expected {
				t.Errorf("Expected %v, received %v", tc.expected, output)
			}
		})
	}
}

func TestParts(t *testing.T) {
	sample := `Time:      7  15   30
  Distance:  9  40  200`
	tests := map[string]struct {
		fn       func(string) int
		expected int
	}{
		"Part One": {
			fn:       PartOne,
			expected: 288,
		},
		"Part Two": {
			fn:       PartTwo,
			expected: 71503,
		},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			output := tc.fn(sample)
			if output != tc.expected {
				t.Errorf("Expected %v, received %v", tc.expected, output)
			}
		})
	}
}
