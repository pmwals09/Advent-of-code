package main

import (
	"reflect"
	"testing"
)

func TestSchematic_GetNeighbors(t *testing.T) {
	sample := `1..2..3
4..5..6
7..8..9`

	tests := map[string]struct {
		num      Number
		expected []int
	}{
		"Happy path": {
			num:      Number{Raw: "5", Offset: 11, Length: 1, Value: 5},
			expected: []int{2, 3, 4, 12, 18, 19, 20, 10},
		},
		"Top-left corner": {
			num:      Number{Raw: "1", Offset: 0, Length: 1, Value: 1},
			expected: []int{1, 8, 9},
		},
		"Top-right corner": {
			num:      Number{Raw: "3", Offset: 6, Length: 1, Value: 3},
			expected: []int{13, 14, 5},
		},
		"Left edge": {
			num:      Number{Raw: "4", Offset: 8, Length: 1, Value: 4},
			expected: []int{0, 1, 9, 16, 17},
		},
		"Right edge": {
			num:      Number{Raw: "6", Offset: 14, Length: 1, Value: 6},
			expected: []int{5, 6, 21, 22, 13},
		},
		"Bottom-left corner": {
			num:      Number{Raw: "7", Offset: 16, Length: 1, Value: 7},
			expected: []int{8, 9, 17},
		},
		"Bottom-right corner": {
			num:      Number{Raw: "9", Offset: 22, Length: 1, Value: 9},
			expected: []int{13, 14, 21},
		},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			output := NewSchematic(sample).GetNeighborIndices(tc.num)
			if !reflect.DeepEqual(output, tc.expected) {
				t.Errorf("Expected %v, got %v", tc.expected, output)
			}
		})
	}
}

func TestSchematic_CappedRange(t *testing.T) {
	sample := `1..2..3
4..5..6
7..8..9`

	tests := map[string]struct {
		input       [2]int
		expected    Range
		shouldError bool
	}{
		"Runs before beginning": {
			input:       [2]int{-1, 1},
			expected:    Range{Start: 0, End: 1},
			shouldError: false,
		},
		"Runs beyond end": {
			input:       [2]int{21, 23},
			expected:    Range{Start: 21, End: 22},
			shouldError: false,
		},
		"Runs within bounds": {
			input:       [2]int{1, 3},
			expected:    Range{Start: 1, End: 3},
			shouldError: false,
		},
		"Runs totally out of bounds": {
			input:       [2]int{23, 25},
			expected:    Range{},
			shouldError: true,
		},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			output, err := NewSchematic(sample).CappedRange(tc.input[0], tc.input[1])
			if tc.shouldError && err == nil {
			}
			if !tc.shouldError && err != nil {
				t.Errorf("Unexpected error: %v", err)
			}
			if output.Start != tc.expected.Start || output.End != tc.expected.End {
				t.Errorf("Expected %v, got %v", tc.expected, output)
			}
		})
	}
}

func TestSchematic_IdxInBounds(t *testing.T) {
	sample := `1..2..3
4..5..6
7..8..9`

	tests := map[string]struct {
		input    int
		expected bool
	}{
		"End of line": {input: 7, expected: false},
		"Too low":     {input: -1, expected: false},
		"Too high":    {input: 25, expected: false},
		"In bounds":   {input: 1, expected: true},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			output := NewSchematic(sample).IdxInBounds(tc.input)
			if output != tc.expected {
				t.Errorf("Expected %v, got %v", tc.expected, output)
			}
		})
	}
}

func TestParts(t *testing.T) {
	sample := NewSchematic(`467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..`)

	tests := map[string]struct {
		fn       func(Schematic) int
		expected int
	}{
		"Part One": {
			fn:       PartOne,
			expected: 4361,
		},
		"Part Two": {
			fn:       PartTwo,
			expected: 467835,
		},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			output := tc.fn(sample)
			if output != tc.expected {
				t.Errorf("Expected %v, got %v", tc.expected, output)
			}
		})
	}
}

func TestRelevantItem_Intersects(t *testing.T) {
	sample := `1.*2..3
4..5.*6
7..8*.9`
	tests := map[string]struct {
		this     RelevantItem
		other    RelevantItem
		expected bool
	}{
		"Gear detects intersection w/ Number": {
			this:     Gear{Offset: 2},
			other:    Number{Raw: "2", Offset: 3, Length: 1, Value: 2},
			expected: true,
		},
		"Gear detects no intersection w/ Number": {
			this:     Gear{Offset: 2},
			other:    Number{Raw: "3", Offset: 6, Length: 1, Value: 3},
			expected: false,
		},
		"Number detects intersection w/ Gear": {
			this:     Number{Raw: "2", Offset: 3, Length: 1, Value: 2},
			other:    Gear{Offset: 2},
			expected: true,
		},
		"Number detects no intersection w/ Gear": {
			this:     Number{Raw: "3", Offset: 6, Length: 1, Value: 3},
			other:    Gear{Offset: 2},
			expected: false,
		},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			output := tc.this.Intersects(tc.other, NewSchematic(sample))
			if output != tc.expected {
				t.Errorf("Expected %v, got %v", tc.expected, output)
			}
		})
	}
}
