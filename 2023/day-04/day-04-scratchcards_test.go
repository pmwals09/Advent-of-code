package main

import (
	"reflect"
	"testing"
)

func TestCardFromString(t *testing.T) {
	tests := map[string]struct {
		input    string
		expected Card
	}{
		"No double spaces": {
			input: "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
			expected: Card{
				Id: 2,
				WinningNumbers: map[int]bool{
					13: true,
					32: true,
					20: true,
					16: true,
					61: true,
				},
				CandidateNumbers: map[int]bool{
					61: true,
					30: true,
					68: true,
					82: true,
					17: true,
					32: true,
					24: true,
					19: true,
				},
				Matches: 2,
        Count: 1,
			},
		},
		"Some double spaces": {
			input: "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
			expected: Card{
				Id: 1,
				WinningNumbers: map[int]bool{
					41: true,
					48: true,
					83: true,
					86: true,
					17: true,
				},
				CandidateNumbers: map[int]bool{
					83: true,
					86: true,
					6:  true,
					31: true,
					17: true,
					9:  true,
					48: true,
					53: true,
				},
				Matches: 4,
        Count: 1,
			},
		},
		"More double spaces": {
			input: "Card 7: 41 48 83 86  4 | 83 86  6 31 17  9 48 53",
			expected: Card{
				Id: 7,
				WinningNumbers: map[int]bool{
					41: true,
					48: true,
					83: true,
					86: true,
					4:  true,
				},
				CandidateNumbers: map[int]bool{
					83: true,
					86: true,
					6:  true,
					31: true,
					17: true,
					9:  true,
					48: true,
					53: true,
				},
				Matches: 3,
        Count: 1,
			},
		},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			output := CardFromString(tc.input)
			if !reflect.DeepEqual(output, tc.expected) {
				t.Errorf("Expected %v, received %v", tc.expected, output)
			}
		})
	}
}

func TestPartOne(t *testing.T) {
	tests := map[string]struct {
		input    string
		expected int
	}{
		"Provided": {
			input: `Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11`,
			expected: 13,
		},
		"More double spaces": {
			input: `Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
Card 7: 41 48 83 86  4 | 83 86  6 31 17  9 48 53`,
			expected: 13 + 4,
		},
	}
	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			output := PartOne(tc.input)
			if output != tc.expected {
				t.Errorf("Expected %v, received %v", tc.expected, output)
			}
		})
	}
}

func TestPartTwo(t *testing.T) {
	input := `Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11`
	expected := 30
	output := PartTwo(input)
	if output != expected {
		t.Errorf("Expected %v, received %v", expected, output)
	}
}
