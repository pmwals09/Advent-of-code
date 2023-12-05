package main

import (
	"reflect"
	"testing"
)

func TestNewMaterialMap(t *testing.T) {
	input := `seed-to-soil map:
50 98 2
52 50 48`
	expected := MaterialMap{
		FromType: "seed",
		ToType:   "soil",
		Ranges: []MaterialRange{
			{
				FromRange: Range{Start: 98, End: 98 + 2},
				ToRange:   Range{Start: 50, End: 50 + 2},
			},
			{
				FromRange: Range{Start: 50, End: 50 + 48},
				ToRange:   Range{Start: 52, End: 52 + 48},
			},
		},
	}

	output := NewMaterialMap(input)
	if !reflect.DeepEqual(output, expected) {
		t.Errorf("Expected %v, received %v", expected, output)
	}
}

func TestGroupToSeedRanges(t *testing.T) {
	input := "seeds: 79 14 55 13"
	output := []int{}
	expected := []int{
    79, 80, 81, 82, 83,
    84, 85, 86, 87, 88,
    89, 90, 91, 92,
    55, 56, 57, 58, 59,
    60, 61, 62, 63, 64,
    65, 66, 67,
  }
	GroupToSeedRanges(input, &output)
	if !reflect.DeepEqual(output, expected) {
		t.Errorf("Expected %v, received %v", expected, output)
	}
}

func TestMaterialRange_Translate(t *testing.T) {
  rangeString := "49 53 8"
  tests := map[string]struct{
    expected int
    input int
  }{
    "Happy path": {
      expected: 50,
      input: 54,
    },
    "Bottom of range": {
      expected: 49,
      input: 53,
    },
    "Top of range": {
      input: 60,
      expected: 56,

    },
  }

  for name, tc := range tests {
    t.Run(name, func(t *testing.T) {
      r := NewMaterialRange(rangeString)
      output := r.Translate(tc.input)
      if output != tc.expected {
        t.Errorf("Expected %v, received %v", tc.expected, output)
      }
    })
  }
}

func TestParts(t *testing.T) {
	input := `seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4`

	tests := map[string]struct {
		fn       func(string) int
		expected int
	}{
		"PartOne": {
			expected: 35,
			fn:       PartOne,
		},
		"PartTwo": {
			expected: 46,
			fn:       PartTwo,
		},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			output := tc.fn(input)
			if output != tc.expected {
				t.Errorf("Expected %v, received %v", tc.expected, output)
			}
		})
	}
}
