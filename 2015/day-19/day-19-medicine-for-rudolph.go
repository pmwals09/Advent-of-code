package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strings"
)

func main() {
	f, _ := os.Open("./day-19-data.txt")
	s := bufio.NewScanner(f)
	rawInstructions := []string{}
	for s.Scan() {
		rawInstructions = append(rawInstructions, s.Text())
	}

	replacements, molecule := rawInstructions[:len(rawInstructions)-2], rawInstructions[len(rawInstructions)-1]

	replacementMap := make(map[string][]string)
	for _, r := range replacements {
		from, to, _ := strings.Cut(r, " => ")
		_, exists := replacementMap[from]
		if exists {
			replacementMap[from] = append(replacementMap[from], to)
		} else {
			replacementMap[from] = []string{to}
		}
	}

	moleculeSet := make(map[string]struct{})
	for k, v := range replacementMap {
		for i := 0; i < len(molecule)-(len(k)-1); i++ {
			sub := molecule[i : i+len(k)]
			if sub == k {
				for _, val := range v {
					replaced := molecule[:i] + val + molecule[i+len(k):]
					moleculeSet[replaced] = struct{}{}
				}
			}
		}
	}

	fmt.Println("Part one:", len(moleculeSet))

	r := regexp.MustCompile("[A-Z]")
	numberOfMolecules := len(r.Split(molecule, -1)) - 1
	r = regexp.MustCompile("Ar|Rn")
	exteriorCantReplace := len(r.Split(molecule, -1)) - 1
	r = regexp.MustCompile("Y")
	interiorCantReplace := len(r.Split(molecule, -1)) - 1

	fmt.Println("Part two:", numberOfMolecules-exteriorCantReplace-(2*interiorCantReplace)-1)
}
