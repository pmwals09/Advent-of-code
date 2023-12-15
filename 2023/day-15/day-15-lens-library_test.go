package main

import "testing"

func TestGetHash(t *testing.T) {
	input := []byte("HASH")
	expected := 52
	output := GetHash(input)
	if output != expected {
		t.Errorf("Expected %d, got %d", expected, output)
	}
}

func TestParts(t *testing.T) {
	input := []byte("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")
	tests := map[string]struct {
		fn       func([]byte) int
		expected int
	}{
		"Part One": {
			fn:       PartOne,
			expected: 1320,
		},
    "Part Two": {
      fn: PartTwo,
      expected: 145,
    },
	}
  for name, test := range tests {
    t.Run(name, func(t *testing.T) {
      output := test.fn(input)
      if output != test.expected {
        t.Errorf("Expected %d, got %d", test.expected, output)
      }
    })
  }
}
