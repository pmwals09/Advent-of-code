package main

import (
	"reflect"
	"testing"
)

func TestPartOne(t *testing.T) {
	tests := map[string]struct {
		input    string
		expected int
	}{
		"Simple": {
			input: `-L|F7
7S-7|
L|7||
-L-J|
L|-JF`,
			expected: 4,
		},
		// 		"Complex": {
		// 			input: `7-F7-
		// .FJ|7
		// SJLL7
		// |F--J
		// LJ.LJ`,
		// 			expected: 8,
		// 		},
	}

	for name, test := range tests {
		t.Run(name, func(t *testing.T) {
			output := PartOne(test.input)
			if output != test.expected {
				t.Errorf("Expected %d, but got %d", test.expected, output)
			}
		})
	}
}

func TestNode_GetValidNeighbors(t *testing.T) {
	sample := NewPipeMap(`-L|F7
7S-7|
L|7||
-L-J|
L|-JF`)
	tests := map[string]struct {
		receiver Node
		expected []*Node
	}{
		"Start": {
			receiver: NewNode('S', 1, 1),
			expected: []*Node{
				func() *Node {
					n := NewNode('-', 1, 2)
					return &n
				}(),
				func() *Node {
					n := NewNode('|', 2, 1)
					return &n
				}(),
			},
		},
		"-": {
			receiver: NewNode('-', 1, 2),
			expected: []*Node{
				func() *Node {
					n := NewNode('7', 1, 3)
					return &n
				}(),
				func() *Node {
					n := NewNode('S', 1, 1)
					return &n
				}(),
			},
		},
	}

	for name, test := range tests {
		t.Run(name, func(t *testing.T) {
			output := test.receiver.GetValidNeighbors(sample)
			if !reflect.DeepEqual(output, test.expected) {
				t.Errorf("Received %v, expected %v", output, test.expected)
			}
		})
	}
}
