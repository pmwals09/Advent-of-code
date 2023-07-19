package main

import (
	"fmt"
	"math"
	"strconv"
	"strings"
)

// "bufio"
// "fmt"
// "os"

type Node struct {
  name string
  edges []*Edge
}

func NewNode(name string) Node {
  return Node { name: name, edges: make([]*Edge, 0) }
}

func (n *Node) ToString() string {
  out := "Node: " + n.name + "\n"

  for _, e := range n.edges {
    if e == nil {
      out += "-> nil\n"
    } else {
      out += fmt.Sprintf("  -> %s / %v\n", e.destination.name, e.weight)
    }
  }
  return out
}

type Edge struct {
  destination *Node
  weight int
}

func NewEdge(destination *Node, weight int) Edge {
  return Edge { destination: destination, weight: weight }
}

type Graph struct {
  nodes []*Node
}

func NewGraph() Graph {
  return Graph { nodes: make([]*Node, 0) }
}

func (g *Graph) AddNode(n *Node) {
  g.nodes = append(g.nodes, n)
}

func (g *Graph) AddEdge(fromNode *Node, toNode *Node, weight int) {
  e := NewEdge(toNode, weight)
  fromNode.edges = append(fromNode.edges, &e)
}

func (g *Graph) ToString() string {
  var out string
  for _, n := range g.nodes {
    out += "----- N IN -----\n"
    if n == nil {
      out += " nil "
    } else {
      out += n.ToString()
    }
    out += "\n----- N OUT -----\n\n"
  }
  return out
}

func main() {
  instructions := []string {
    "London to Dublin = 464",
    "London to Belfast = 518",
    "Dublin to Belfast = 141",
  }

  g := buildGraph(instructions)

  shortest := int(math.Inf(1))

  for _, n := range g.nodes {
    // get the shortest path that starts from each node
    plen := shortestPath(n, g)
    if shortest > plen {
      shortest = plen
    }
  }

  fmt.Println(g.ToString())
}

func parseInstruction(s string) (string, string, int) {
  before, weight, _ := strings.Cut(s, " = ")
  from, to, _ := strings.Cut(before, " to ")
  w, _ := strconv.Atoi(weight)
  return from, to, w
}

func buildGraph(instructions []string) Graph {
  g := NewGraph()

  for i := 0; i < len(instructions); i++ {
    nodeName, destinationName, weight := parseInstruction(instructions[i])
    var fromNode *Node
    var toNode *Node
    for _, n := range g.nodes {
      if n != nil && n.name == nodeName {
        fromNode = n
      } else if n != nil && n.name == destinationName {
        toNode = n
      }
    }

    if fromNode == nil {
      n := NewNode(nodeName)
      fromNode = &n
      g.AddNode(fromNode)
    }
    if toNode == nil {
      n := NewNode(destinationName)
      toNode = &n
      g.AddNode(toNode)
    }

    g.AddEdge(fromNode, toNode, weight)
    g.AddEdge(toNode, fromNode, weight)
  }

  return g
}

func shortestPath(n *Node, g Graph) int {
  pathsQ := [][]*Node{{n}}

  for _, p := range pathsQ {
    remaining := []*Node{}
    for _, node := range g.nodes {
      if !includes(p, node) {
        remaining = append(remaining, node)
      }
    }

    if len(remaining) == 0 {
      fmt.Println("All done with this path")
    } else {
      for _, r := range remaining {
        newPath := make([]*Node, len(p) + 1)
        copy(newPath, p)
        newPath = append(newPath, r)
        pathsQ = append(pathsQ, newPath)
      }
    }
  }
  fmt.Println(pathsQ)
  return 0
}

func shortestHelp(n *Node, visited []*Node, g Graph) {
}

func includes(a []*Node, val *Node) bool {
  for _, v := range a {
    if v == val {
      return true
    }
  }
  return false
}
