package main

import (
	"reflect"
	"testing"
)

func TestParts(t *testing.T) {
	sample := `O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....`
	tests := map[string]struct {
		fn       func(string) int
		expected int
	}{
		"Part One": {
			fn:       PartOne,
			expected: 136,
		},
		"Part Two": {
			fn:       PartTwo,
			expected: 64,
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

func TestRotations(t *testing.T) {
	sample := `O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....`

	tests := map[string]struct {
		fn       func(*[][]rune)
		expected [][]rune
	}{
		"Clockwise": {
			fn: RotateClockwise,
			expected: [][]rune{
				{'#', '#', '.', '.', 'O', '.', 'O', '.', 'O', 'O'},
				{'O', '.', '.', '.', '.', 'O', 'O', '.', '.', '.'},
				{'O', '.', '.', 'O', '#', '.', '.', '.', 'O', '.'},
				{'.', '.', '.', '.', '.', '.', '#', '.', 'O', '.'},
				{'.', '.', '.', '.', '.', '.', 'O', '.', '#', '.'},
				{'#', '#', '.', '#', 'O', '.', '.', '#', '.', '#'},
				{'.', '#', '.', 'O', '.', '.', '.', '#', '.', '.'},
				{'.', '#', 'O', '.', '#', 'O', '.', '.', '.', '.'},
				{'.', '.', '.', '.', '.', '#', '.', '.', '.', '.'},
				{'.', '.', '.', 'O', '#', '.', 'O', '.', '#', '.'},
			},
		},
	}

	for name, test := range tests {
		t.Run(name, func(t *testing.T) {
			arr := GetRockMap(sample)
			test.fn(&arr)
			if !reflect.DeepEqual(arr, test.expected) {
				t.Errorf("Expected %v, got %v", test.expected, arr)
			}
		})
	}

}

func TestCycles(t *testing.T) {
	sample := `O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....`
	tests := map[string]struct {
		numCycles int
		expected  [][]rune
	}{
		"1 cycle": {
			numCycles: 1,
			expected: [][]rune{
				{'.', '.', '.', '.', '.', '#', '.', '.', '.', '.'},
				{'.', '.', '.', '.', '#', '.', '.', '.', 'O', '#'},
				{'.', '.', '.', 'O', 'O', '#', '#', '.', '.', '.'},
				{'.', 'O', 'O', '#', '.', '.', '.', '.', '.', '.'},
				{'.', '.', '.', '.', '.', 'O', 'O', 'O', '#', '.'},
				{'.', 'O', '#', '.', '.', '.', 'O', '#', '.', '#'},
				{'.', '.', '.', '.', 'O', '#', '.', '.', '.', '.'},
				{'.', '.', '.', '.', '.', '.', 'O', 'O', 'O', 'O'},
				{'#', '.', '.', '.', 'O', '#', '#', '#', '.', '.'},
				{'#', '.', '.', 'O', 'O', '#', '.', '.', '.', '.'},
			},
		},
		"2 cycles": {
			numCycles: 2,
			expected: [][]rune{
				{'.', '.', '.', '.', '.', '#', '.', '.', '.', '.'},
				{'.', '.', '.', '.', '#', '.', '.', '.', 'O', '#'},
				{'.', '.', '.', '.', '.', '#', '#', '.', '.', '.'},
				{'.', '.', 'O', '#', '.', '.', '.', '.', '.', '.'},
				{'.', '.', '.', '.', '.', 'O', 'O', 'O', '#', '.'},
				{'.', 'O', '#', '.', '.', '.', 'O', '#', '.', '#'},
				{'.', '.', '.', '.', 'O', '#', '.', '.', '.', 'O'},
				{'.', '.', '.', '.', '.', '.', '.', 'O', 'O', 'O'},
				{'#', '.', '.', 'O', 'O', '#', '#', '#', '.', '.'},
				{'#', '.', 'O', 'O', 'O', '#', '.', '.', '.', 'O'},
			},
		},
		"3 cycles": {
			numCycles: 3,
			expected: [][]rune{
				{'.', '.', '.', '.', '.', '#', '.', '.', '.', '.'},
				{'.', '.', '.', '.', '#', '.', '.', '.', 'O', '#'},
				{'.', '.', '.', '.', '.', '#', '#', '.', '.', '.'},
				{'.', '.', 'O', '#', '.', '.', '.', '.', '.', '.'},
				{'.', '.', '.', '.', '.', 'O', 'O', 'O', '#', '.'},
				{'.', 'O', '#', '.', '.', '.', 'O', '#', '.', '#'},
				{'.', '.', '.', '.', 'O', '#', '.', '.', '.', 'O'},
				{'.', '.', '.', '.', '.', '.', '.', 'O', 'O', 'O'},
				{'#', '.', '.', '.', 'O', '#', '#', '#', '.', 'O'},
				{'#', '.', 'O', 'O', 'O', '#', '.', '.', '.', 'O'},
			},
		},
	}

	for name, test := range tests {
		t.Run(name, func(t *testing.T) {
			arr := GetRockMap(sample)
			rockTracker := make([]int, len(arr[0]))
			for i := 0; i < test.numCycles; i++ {
				TiltMapNorth(&arr, &rockTracker)
				TiltMapWest(&arr, &rockTracker)
				TiltMapSouth(&arr, &rockTracker)
				TiltMapEast(&arr, &rockTracker)
			}
			if !reflect.DeepEqual(arr, test.expected) {
				t.Errorf("Expected %v, got %v", test.expected, arr)
			}
		})
	}
}

/*
 */
