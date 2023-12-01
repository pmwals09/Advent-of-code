package main

import (
  "strings"
  "testing"
)

func TestPartOne(t *testing.T) {
  sampleInput := `1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet`
  lines := strings.Split(sampleInput, "\n")
  expect := 142
  if partOne(lines)!= expect {
    t.Fatalf("Part one failed. Got %d, expected %d", partOne(lines), expect)
  }
}

func TestPartTwo(t *testing.T) {
  sampleInput := `two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen`
  lines := strings.Split(sampleInput, "\n")
  expect := 281
  if partTwo(lines)!= expect {
    t.Fatalf("Part two failed. Got %d, expected %d", partTwo(lines), expect)
  }
}
