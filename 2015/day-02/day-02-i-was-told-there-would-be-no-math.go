package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

func main() {
	// read each line in file
	f, _ := os.Open("./day-02-data.txt")
	fs := bufio.NewScanner(f)
	fs.Split(bufio.ScanLines)
	var total_paper_area int
	var total_ribbon_length int
	for fs.Scan() {
		var gift_dimensions []int
		for _, d := range strings.Split(fs.Text(), "x") {
			dimension, _ := strconv.Atoi(d)
			gift_dimensions = append(gift_dimensions, dimension)
		}

		sort.Slice(gift_dimensions, func(i, j int) bool {
			return gift_dimensions[i] < gift_dimensions[j]
		})

		total_paper_area += side_surface_area(gift_dimensions[0], gift_dimensions[1]) + total_surface_area(gift_dimensions[0], gift_dimensions[1], gift_dimensions[2])
		total_ribbon_length += side_perimeter(gift_dimensions[0], gift_dimensions[1]) + volume(gift_dimensions[0], gift_dimensions[1], gift_dimensions[2])
	}

	fmt.Println("Part one:", total_paper_area)
	fmt.Println("Part two:", total_ribbon_length)
}

func side_surface_area(w, h int) int {
	return w * h
}

func total_surface_area(w, h, l int) int {
	return 2*side_surface_area(w, h) + 2*side_surface_area(h, l) + 2*side_surface_area(w, l)
}

func side_perimeter(w, h int) int {
	return 2*w + 2*h
}

func volume(w, h, l int) int {
	return w * h * l
}
