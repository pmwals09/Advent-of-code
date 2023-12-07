package main

import "testing"

func TestParts(t *testing.T) {
	sample := `32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483`
	tests := map[string]struct {
		expected int
		fn       func(string) int
	}{
		"Part One": {
			expected: 6440,
			fn:       PartOne,
		},
		"Part Two": {
			expected: 5905,
			fn:       PartTwo,
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
