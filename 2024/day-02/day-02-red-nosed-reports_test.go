package main

import "testing"

const INPUT = `
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
`

func TestPartOne(t *testing.T) {
	expected := 2
	actual := PartOne(INPUT)
	if actual != expected {
		t.Errorf("expected=%d, got=%d", expected, actual)
	}
}

func TestPartTwo(t *testing.T) {
	expected := 4
	actual := PartTwo(INPUT)
	if actual != expected {
		t.Errorf("expected=%d, got=%d", expected, actual)
	}
}
