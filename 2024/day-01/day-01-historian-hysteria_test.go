package main

import "testing"

func TestPartOne(t *testing.T) {
	f := func(input string, expected int) {
		t.Helper()

		actual := PartOne(input)
		if actual != expected {
			t.Errorf("expected=%d, got=%d", expected, actual)
		}
	}

	f(`
3   4
4   3
2   5
1   3
3   9
3   3
        `, 11)
}

func TestPartTwo(t *testing.T) {
	f := func(input string, expected int) {
		t.Helper()

		actual := PartTwo(input)
		if actual != expected {
			t.Errorf("expected=%d, got=%d", expected, actual)
		}
	}

	f(`
3   4
4   3
2   5
1   3
3   9
3   3
`, 31)
}
