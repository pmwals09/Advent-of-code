package main

import (
	"bytes"
	"fmt"
	"os"
)

func main() {
	input := getInput("./day-15-input.txt")
	fmt.Println("Part One:", PartOne(input))
	fmt.Println("Part Two:", PartTwo(input))
}

func getInput(path string) []byte {
	ba, err := os.ReadFile(path)
	if err != nil {
		panic(err)
	}
	return ba
}

func PartOne(input []byte) int {
	strs := bytes.Split(input, []byte(","))
	var total int
	for _, ea := range strs {
		total += GetHash(ea)
	}
	return total
}

type Instruction struct {
	Label       []byte
	Op          byte
	FocalLength int
}

func NewInstruction(instruction []byte) *Instruction {
	out := &Instruction{}
	preOp := true
	for _, b := range instruction {
		if preOp {
			if b == '=' || b == '-' {
				out.Op = b
				preOp = false
			} else {
				out.Label = append(out.Label, b)
			}
		} else {
			out.FocalLength = int(b) - 48
		}
	}
	return out
}

func PartTwo(input []byte) int {
	strs := bytes.Split(input, []byte(","))
	instructions := make([]*Instruction, len(strs))
	for i, ea := range strs {
		instructions[i] = NewInstruction(ea)
	}

	mirrors := make([][]*Instruction, 256)

	for _, instruction := range instructions {
		hash := GetHash(instruction.Label)
		box := mirrors[hash]
    inserted := false
		if instruction.Op == '=' {
			for i, lens := range mirrors[hash] {
				if bytes.Equal(lens.Label, instruction.Label) {
          mirrors[hash][i] = instruction
          inserted = true
          break
				}
			}
      if !inserted {
        mirrors[hash] = append(mirrors[hash], instruction)
      }
		} else {
      // remove the lens with this label from the box
      // shift the rest of the lenses up so there are no gaps
      for i, lens := range box {
        if bytes.Equal(lens.Label, instruction.Label) {
          mirrors[hash] = append(box[:i], box[i+1:]...)
        }
      }
		}
	}

  total := 0
  for i, mirror := range mirrors {
    if len(mirror) > 0 {
      for j, lens := range mirror {
        total += ((i + 1) * (j + 1) * lens.FocalLength)
      }
    }
  }

	return total
}

func GetHash(input []byte) int {
	current := 0
	for _, ea := range input {
		current += int(ea)
		current *= 17
		current %= 256
	}
	return current
}
