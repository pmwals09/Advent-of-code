package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Part struct {
	X int
	M int
	A int
	S int
}

func NewPart(text string) Part {
	p := Part{}
	ba := text[1 : len(text)-1]
	parts := strings.Split(ba, ",")
	x, m, a, s := parts[0], parts[1], parts[2], parts[3]

	p.X, _ = strconv.Atoi(strings.Split(x, "=")[1])
	p.M, _ = strconv.Atoi(strings.Split(m, "=")[1])
	p.A, _ = strconv.Atoi(strings.Split(a, "=")[1])
	p.S, _ = strconv.Atoi(strings.Split(s, "=")[1])
	return p
}

type Step struct {
	Attribute      string
	ComparatorChar rune
	Limit          int
	Comparator     func(p Part) bool
	Result         string
}

type Workflow struct {
	Name  string
	Steps []Step
}

func NewWorkflow(text string) (string, Workflow) {
	name, tail, _ := strings.Cut(text, "{")
	tail = tail[:len(tail)-1]
	stepParts := strings.Split(tail, ",")

	steps := []Step{}
	for _, p := range stepParts {
		if strings.Contains(p, "<") {
			attribute, tail, _ := strings.Cut(p, "<")
			val, result, _ := strings.Cut(tail, ":")
			v, _ := strconv.Atoi(val)
			steps = append(steps, Step{
				Attribute:      attribute,
				ComparatorChar: '<',
				Limit:          v,
				Comparator: func(part Part) bool {
					return LT(part, attribute, v)
				},
				Result: result,
			})
		} else if strings.Contains(p, ">") {
			attribute, tail, _ := strings.Cut(p, ">")
			val, result, _ := strings.Cut(tail, ":")
			v, _ := strconv.Atoi(val)
			steps = append(steps, Step{
				Attribute:      attribute,
				ComparatorChar: '>',
				Limit:          v,
				Comparator: func(part Part) bool {
					return GT(part, attribute, v)
				},
				Result: result,
			})
		} else {
			steps = append(steps, Step{
				Attribute:      p,
				ComparatorChar: '=',
				Limit:          0,
				Comparator:     func(part Part) bool { return true },
				Result:         p,
			})
		}
	}

	return name, Workflow{Name: name, Steps: steps}
}

func LT(p Part, attribute string, value int) bool {
	switch attribute {
	case "x":
		return p.X < value
	case "m":
		return p.M < value
	case "a":
		return p.A < value
	case "s":
		return p.S < value
	default:
		return false
	}
}
func GT(p Part, attribute string, value int) bool {
	switch attribute {
	case "x":
		return p.X > value
	case "m":
		return p.M > value
	case "a":
		return p.A > value
	case "s":
		return p.S > value
	default:
		return false
	}
}

func main() {
	input := getInput("./day-19-input.txt")
	fmt.Println("Part One:", PartOne(input))
	fmt.Println("Part Two:", PartTwo(input))
}

func getInput(path string) string {
	ba, err := os.ReadFile(path)
	if err != nil {
		panic(err)
	}
	return string(ba)
}

func PartOne(input string) int {
	workflows, parts := ParseInput(input)
	var total int
	for _, p := range parts {
		curr := "in"
		for curr != "A" && curr != "R" {
			wf := workflows[curr]
			for _, step := range wf.Steps {
				if step.Comparator(p) {
					curr = step.Result
					break
				}
			}
		}
		if curr == "A" {
			partSum := p.X + p.M + p.A + p.S
			total += partSum
		}
	}

	return total
}

type Range struct {
	Low  int
	High int
}

func NewRange() Range {
	return Range{
		Low:  0,
		High: 4000,
	}
}

func (r Range) Contains(val int) bool {
  return val >= r.Low && val <= r.High
}

type RangeGroup struct {
	X Range
	M Range
	A Range
	S Range
}

type Path struct {
	RangeGroup
	Keys []string
}

func (p Path) Copy() Path {
	newPath := Path{}
	newPath.Keys = make([]string, len(p.Keys))
	copy(newPath.Keys, p.Keys)
	newPath.RangeGroup = RangeGroup{
		X: p.X,
		M: p.M,
		A: p.A,
		S: p.S,
	}
	return newPath
}

func FindAcceptedPaths(toVisit []Path, workflows map[string]Workflow, accepted []RangeGroup, visited map[string]bool) []RangeGroup {
  if len(toVisit) == 0 {
    return accepted
  }

  curr := toVisit[0]
  currKey := curr.Keys[len(curr.Keys) - 1]
  for _, step := range curr.Steps {
    return FindAcceptedPaths(toVisit[1:], workflows, accepted, visited)
  }
  panic("Shouldn't get here...")
}
func PartTwo(input string) int {
	workflows, _ := ParseInput(input)

	accepted := []RangeGroup{}
	// paths := []Path{{RangeGroup{NewRange(), NewRange(), NewRange(), NewRange()}, []string{"in"}}}
  out := FindAcceptedPaths()
  fmt.Println(out)
	// traverse the paths through the graph
	// if it ends in A, we should calculate the min range for each letter
	// - maybe make it dfs so you aren't calculating lots of invalid paths at once
	// we should save the ranges for each valid path
	// get the minimum range among all the paths
	// multiply all the numbers together
	// accepted := []RangeGroup{}
	// paths := []Path{{RangeGroup{NewRange(), NewRange(), NewRange(), NewRange()}, []string{"in"}}}
	// for len(paths) > 0 {
	// 	curr := paths[0]
	// 	latestKey := curr.Keys[len(curr.Keys)-1]
	// 	if latestKey == "A" {
	// 		accepted = append(accepted, curr.RangeGroup)
	// 	} else if latestKey != "R" {
	// 		wf := workflows[latestKey]
	// 		for _, step := range wf.Steps {
	// 			newPath := curr.Copy()
	// 			switch step.Attribute {
	// 			case "x":
	// 				if step.ComparatorChar == '>' && newPath.RangeGroup.X.Low < step.Limit {
	// 					newPath.RangeGroup.X.Low = step.Limit
	// 				} else if step.ComparatorChar == '<' && newPath.RangeGroup.X.High > step.Limit {
	// 					newPath.RangeGroup.X.High = step.Limit
	// 				}
	// 			case "m":
	// 				if step.ComparatorChar == '>' && newPath.RangeGroup.M.Low < step.Limit {
	// 					newPath.RangeGroup.M.Low = step.Limit
	// 				} else if step.ComparatorChar == '<' && newPath.RangeGroup.M.High > step.Limit {
	// 					newPath.RangeGroup.M.High = step.Limit
	// 				}
	// 			case "a":
	// 				if step.ComparatorChar == '>' && newPath.RangeGroup.A.Low < step.Limit {
	// 					newPath.RangeGroup.A.Low = step.Limit
	// 				} else if step.ComparatorChar == '<' && newPath.RangeGroup.A.High > step.Limit {
	// 					newPath.RangeGroup.A.High = step.Limit
	// 				}
	// 			case "s":
	// 				if step.ComparatorChar == '>' && newPath.RangeGroup.S.Low < step.Limit {
	// 					newPath.RangeGroup.S.Low = step.Limit
	// 				} else if step.ComparatorChar == '<' && newPath.RangeGroup.S.High > step.Limit {
	// 					newPath.RangeGroup.S.High = step.Limit
	// 				}
	// 			}
	// 			newPath.Keys = append(newPath.Keys, step.Result)
	// 			paths = append(paths, newPath)
	// 		}
	// 	}
	// }
	fmt.Println(accepted)
	return 0
}

func ParseInput(input string) (map[string]Workflow, []Part) {
	inputParts := strings.Split(input, "\n\n")
	workflowStrings, partStrings := inputParts[0], inputParts[1]
	workflows := map[string]Workflow{}
	for _, line := range strings.Split(workflowStrings, "\n") {
		name, wf := NewWorkflow(line)
		workflows[name] = wf
	}
	parts := []Part{}
	for _, line := range strings.Split(partStrings, "\n") {
		parts = append(parts, NewPart(line))
	}

	return workflows, parts
}
