package main

import (
	"testing"
)

var INPUT = `MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX`

func TestPartOne(t *testing.T) {
	expected := 18
	actual := PartOne(INPUT)
	if actual != expected {
		t.Errorf("expected=%d, got=%d", expected, actual)
	}
}

func TestGetNeighbors(t *testing.T) {
	f := func(idx, maxIdx, lineLen int, direction, expected []int) {
		neighbors := getNeighbors(idx, maxIdx, lineLen, direction)
		toMatch := make([]int, len(neighbors))
		for i, n := range neighbors {
			toMatch[i] = n.idx
		}
		if mismatch, ok := testSliceValuesNoOrder(toMatch, expected); !ok {
			t.Errorf("neighbors does not match expected. mismatch=%v", mismatch)
		}
	}

	f(0, 108, 10, nil, []int{1, 10, 11, 12})
	f(12, 108, 10, nil, []int{0, 1, 2, 11, 13, 22, 23, 24})
	f(108, 108, 10, nil, []int{96, 97, 98, 107})
	f(0, 108, 10, []int{1, 1}, []int{12})
	f(0, 108, 10, []int{0, 1}, []int{1})
	f(12, 108, 10, []int{-1, -1}, []int{0})
}

func TestPartTwo(t *testing.T) {
	expected := 9
	actual := PartTwo(INPUT)
	if actual != expected {
		t.Errorf("expected=%d, got=%d", expected, actual)
	}
}

func TestGetIdxFromLetterIdx(t *testing.T) {
	f := func(p path, letterIdx, lineLen, expected int) {
		actual := p.getIdxFromLetterIdx(letterIdx, lineLen)
		if actual != expected {
			t.Errorf("expected=%d, got=%d", expected, actual)
		}
	}

	f(path{
		currIdx:     25,
		currRuneIdx: 2,
		direction:   []int{1, 1},
		startIdx:    1,
	}, 1, 10, 13)
	f(path{
		currIdx:     3,
		currRuneIdx: 2,
		direction:   []int{-1, 1},
		startIdx:    23,
	}, 1, 10, 13)
}

func testSliceValuesNoOrder[T comparable](a []T, b []T) ([]struct {
	item T
	from string
}, bool) {
	diff := make(map[T]*struct {
		count int
		from  []string
	})
	for _, item := range a {
		diff[item] = &struct {
			count int
			from  []string
		}{count: 1, from: []string{"a"}}
	}

	for _, item := range b {
		if _, ok := diff[item]; ok {
			diff[item].from = append(diff[item].from, "b")
			diff[item].count++
		} else {
			diff[item] = &struct {
				count int
				from  []string
			}{count: 1, from: []string{"b"}}
		}
	}

	mismatch := make([]struct {
		item T
		from string
	}, 0)

	for k, v := range diff {
		if v.count != 2 {
			mismatch = append(mismatch, struct {
				item T
				from string
			}{k, v.from[0]})
		}
	}

	if len(mismatch) > 0 {
		return mismatch, false
	}
	return nil, true
}
