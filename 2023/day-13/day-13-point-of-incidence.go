package main

import (
	"fmt"
	"os"
	"strings"
)

func main() {
	input := getInput("./day-13-input.txt")
	fmt.Println("Part One:", PartOne(input))
	fmt.Println("Part Two:", PartTwo(input))
}

func getInput(filepath string) string {
	ba, err := os.ReadFile(filepath)
	if err != nil {
		panic(err)
	}
	return string(ba)
}

func PartOne(input string) int {
	mirrorFields := strings.Split(input, "\n\n")
	var total int
	for _, m := range mirrorFields {
		total += GetScore(m)
	}
	return total
}

func GetScore(m string) int {
	fieldMap := strings.Split(m, "\n")
	rowMirrorCandidates := GetRowMirrorCandidates(fieldMap)

	for _, mirrorIdx := range rowMirrorCandidates {
		if IsValidMirror(mirrorIdx, fieldMap) {
			s := (mirrorIdx + 1) * 100
			return s
		}
	}

	rotatedFieldMap := RotateArrayClockwise(fieldMap)
	colMirrorCandidates := GetRowMirrorCandidates(rotatedFieldMap)

	for _, mirrorIdx := range colMirrorCandidates {
		if IsValidMirror(mirrorIdx, rotatedFieldMap) {
			s := mirrorIdx + 1
			return s
		}
	}

	panic("Should not reach")
}

func PartTwo(input string) int {
	mirrorFields := strings.Split(input, "\n\n")
	var total int
	for _, m := range mirrorFields {
		total += GetAdjustedScore(m)
	}
	return total
}

func GetAdjustedScore(m string) int {
	fieldMap := strings.Split(m, "\n")
	rowMirror := -1
	colMirror := -1
	originalRowMirrorCandidates := GetRowMirrorCandidates(fieldMap)
	for _, mirrorIdx := range originalRowMirrorCandidates {
		if IsValidMirror(mirrorIdx, fieldMap) {
			rowMirror = mirrorIdx
			break
		}
	}

	rotatedFieldMap := RotateArrayClockwise(fieldMap)
	originalColMirrorCandidates := GetRowMirrorCandidates(rotatedFieldMap)

	for _, mirrorIdx := range originalColMirrorCandidates {
		if IsValidMirror(mirrorIdx, rotatedFieldMap) {
			colMirror = mirrorIdx
			break
		}
	}

	count := 0
	for rowIdx, row := range fieldMap {
		for colIdx, char := range row {
			count++
			thisFieldMap := make([]string, len(fieldMap))
			copy(thisFieldMap, fieldMap)
			if char == '.' {
				thisFieldMap[rowIdx] = thisFieldMap[rowIdx][:colIdx] + "#" + thisFieldMap[rowIdx][colIdx+1:]
			} else {
				thisFieldMap[rowIdx] = thisFieldMap[rowIdx][:colIdx] + "." + thisFieldMap[rowIdx][colIdx+1:]
			}

			rowMirrorCandidates := GetRowMirrorCandidates(thisFieldMap)
			for _, mirrorIdx := range rowMirrorCandidates {
				if IsValidMirror(mirrorIdx, thisFieldMap) && mirrorIdx != rowMirror {
					s := (mirrorIdx + 1) * 100
					return s
				}
			}

			// find all options for a column mirror
			thisRotatedFieldMap := RotateArrayClockwise(thisFieldMap)
			colMirrorCandidates := GetRowMirrorCandidates(thisRotatedFieldMap)

			for _, mirrorIdx := range colMirrorCandidates {
				if IsValidMirror(mirrorIdx, thisRotatedFieldMap) && mirrorIdx != colMirror {
					s := mirrorIdx + 1
					return s
				}
			}

		}
	}
  panic("Should not get here")
}

func GetRowMirrorCandidates(fieldMap []string) []int {
	rowMirrorCandidates := []int{}
	for i := 1; i < len(fieldMap); i++ {
		prevRow := fieldMap[i-1]
		row := fieldMap[i]
		if row == prevRow {
			rowMirrorCandidates = append(rowMirrorCandidates, i-1)
		}
	}
	return rowMirrorCandidates
}

func IsValidMirror(candidateIdx int, fieldMap []string) bool {
	startIdx := candidateIdx
	delta := 0
	isValid := true
	for isValid && startIdx-delta >= 0 && startIdx+1+delta < len(fieldMap) {
		if fieldMap[startIdx-delta] == fieldMap[startIdx+1+delta] {
			delta++
		} else {
			isValid = false
		}
	}
	return isValid
}

func RotateArrayClockwise(arr []string) []string {
	out := []string{}
	for charIdx := 0; charIdx < len(arr[0]); charIdx++ {
		charIdxRow := strings.Builder{}
		for rowIdx := len(arr) - 1; rowIdx >= 0; rowIdx-- {
			charIdxRow.WriteByte(arr[rowIdx][charIdx])
		}
		out = append(out, charIdxRow.String())
	}
	return out
}
func GetRowLine(fieldArr []string) int {
	for i := 1; i < len(fieldArr); i++ {
		row := fieldArr[i]
		prevRow := fieldArr[i-1]
		if row == prevRow {
			return i - 1
		}
	}
	return -1
}
