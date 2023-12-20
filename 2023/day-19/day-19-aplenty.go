package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	input := getInput("./day-19-input.txt")
	fmt.Println("Part One:", PartOne(input))
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

type Workflow struct {
	Name  string
	Steps []Step
}

type Step struct {
	Comparator func(p Part) bool
	Result     string
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
				Comparator: func(part Part) bool {
          return GT(part, attribute, v)
				},
				Result: result,
			})
		} else {
      steps = append(steps, Step{
        Comparator: func(part Part) bool { return true },
        Result: p,
      })
		}
	}

  return name, Workflow{ Name: name, Steps: steps }
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
