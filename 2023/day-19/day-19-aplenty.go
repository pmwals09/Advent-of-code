package main

import (
	"bytes"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Rule struct {
	Key         string
	Test        func(Part) bool
	Destination string
}

func (r Rule) TestPart(part Part) bool {
	return r.Test(part)
}

type Workflow struct {
	Name  string
	Rules []Rule
}

func (w *Workflow) UnmarshalText(ba []byte) error {
	name, rest, _ := bytes.Cut(ba, []byte{'{'})
	w.Name = string(name)
	noSpace := bytes.TrimSpace(rest)
	ruleParts := bytes.Split(noSpace[:len(noSpace)-1], []byte{','})
	w.Rules = make([]Rule, 0, len(ruleParts))
	for _, rule := range ruleParts {
		var r Rule
		if string(rule) == "R" || string(rule) == "A" {
			r.Test = func(p Part) bool { return true }
			r.Destination = string(rule)
			w.Rules = append(w.Rules, r)
			continue
		}
		pre, dest, found := bytes.Cut(rule, []byte{':'})
		if !found {
			r.Destination = string(rule)
			r.Test = func(p Part) bool { return true }
			w.Rules = append(w.Rules, r)
			continue
		}
		r.Destination = string(dest)
		key, val, found := bytes.Cut(pre, []byte{'<'})
		if !found {
			key, val, _ = bytes.Cut(pre, []byte{'>'})
		}
		r.Key = string(key)
		v, err := strconv.Atoi(string(val))
		if err != nil {
			return err
		}
		r.Test = func(p Part) bool {
			if found {
				return p.GetByKeyname(string(key)) < v
			}
			return p.GetByKeyname(string(key)) > v
		}
		w.Rules = append(w.Rules, r)
	}
	return nil
}

type Part struct {
	X int
	M int
	A int
	S int
}

func (p *Part) UnmarshalText(ba []byte) error {
	if len(ba) < 2 {
		return fmt.Errorf("insufficient data: %s", string(ba))
	}
	parts := bytes.Split(ba[1:len(ba)-1], []byte{','})
	for _, prt := range parts {
		k, v, _ := bytes.Cut(prt, []byte{'='})
		val, _ := strconv.Atoi(string(v))
		switch string(k) {
		case "x":
			p.X = val
		case "m":
			p.M = val
		case "a":
			p.A = val
		case "s":
			p.S = val
		default:
			return fmt.Errorf("invalid key in obj: %s", string(ba))
		}
	}
	return nil
}

func (p Part) GetByKeyname(keyname string) int {
	switch strings.ToLower(keyname) {
	case "x":
		return p.X
	case "m":
		return p.M
	case "a":
		return p.A
	case "s":
		return p.S
	default:
		return 0
	}
}

func main() {
	ba, err := os.ReadFile("./day-19-input.txt")
	if err != nil {
		fmt.Println("Err", err.Error())
		os.Exit(1)
	}

	fmt.Println("Part One:", PartOne(ba))
	// fmt.Println("Part Two:", PartTwo(ba))
}

func PartOne(input []byte) int {
	wf, p, _ := bytes.Cut(input, []byte{'\n', '\n'})
	workflows, err := buildWorkflows(wf)
	if err != nil {
		fmt.Println("Error building workflows", err.Error())
		os.Exit(1)
	}
	wfMap := make(map[string]Workflow)
	for _, w := range workflows {
		wfMap[w.Name] = w
	}
	parts, err := buildParts(p)
	if err != nil {
		fmt.Println("Error building parts", err.Error())
		os.Exit(1)
	}
	accepted := make([]Part, 0)
	for _, prt := range parts {
		curr := wfMap["in"]
		for curr.Rules != nil || len(curr.Rules) > 0 {
		Loop:
			for _, rule := range curr.Rules {
				if rule.TestPart(prt) {
					switch rule.Destination {
					case "A":
						accepted = append(accepted, prt)
						curr = Workflow{Rules: nil}
						break Loop
					case "R":
						curr = Workflow{Rules: nil}
						break Loop
					default:
						curr = wfMap[rule.Destination]
						break Loop
					}
				}
			}
		}
	}

	var sum int
	for _, a := range accepted {
		sum += (a.X + a.M + a.A + a.S)
	}
	return sum
}

func buildWorkflows(wfs []byte) ([]Workflow, error) {
	wfLines := bytes.Split(bytes.TrimSpace(wfs), []byte{'\n'})
	out := make([]Workflow, 0, len(wfLines))
	for _, line := range wfLines {
		var wf Workflow
		if err := wf.UnmarshalText(line); err != nil {
			return out, err
		}
		out = append(out, wf)
	}
	return out, nil
}

func buildParts(parts []byte) ([]Part, error) {
	partLines := bytes.Split(bytes.TrimSpace(parts), []byte{'\n'})
	out := make([]Part, 0, len(partLines))
	for _, line := range partLines {
		var p Part
		if err := p.UnmarshalText(line); err != nil {
			return out, err
		}
		out = append(out, p)
	}
	return out, nil
}
