package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Graph struct {
  nodes []*Node
  length int
}

func (g *Graph) AddNode(name string) *Node {
  for _, node := range g.nodes {
    if node.name == name {
      return node
    }
  }
  n := NewNode(name)
  g.nodes = append(g.nodes, &n)
  g.length++
  return &n
}

func NewGraph() Graph {
  return Graph { nodes: nil, length: 0 }
}

func (g Graph) ToString() string {
  var out string
  for i, n := range g.nodes {
    out += fmt.Sprintf("%d. %s:\n", i + 1, n.name)
    for j, e := range n.edges {
      out += fmt.Sprintf(" -->%d. %s / %d\n",j + 1, e.to.name, e.weight)
    }
    out += "\n"
  }
  return out
}

type Node struct {
  name string
  edges []*Edge
}

func NewNode(name string) Node {
  return Node { name: name, edges: nil }
}

func (n *Node) AddEdge(to *Node, weight int) {
  e := NewEdge(to, weight)
  n.edges = append(n.edges, &e)
}

type Edge struct {
  to *Node
  weight int
}

func NewEdge(to *Node, weight int) Edge {
  return Edge { to: to, weight: weight }
}

func main() {
  f, _ := os.Open("./day-13-data.txt")
  s := bufio.NewScanner(f)
  g := NewGraph()
  for s.Scan() {
    name, weight, toName := parseEdgeText(s.Text())
    n := g.AddNode(name)
    var toNode *Node
    for _, node := range g.nodes {
      if node.name == toName {
        toNode = node
      }
    }

    if toNode == nil {
      toNode = g.AddNode(toName)
    }
    n.AddEdge(toNode, weight)
  }

  var maxHappiness int
  for _, n := range g.nodes {
    happiness := getMaxHappiness(n, g)
    if happiness > maxHappiness {
      maxHappiness = happiness
    }
  }
  fmt.Println("Part one:", maxHappiness)

  meN := g.AddNode("Me")
  for _, n := range g.nodes {
    meN.AddEdge(n, 0)
    n.AddEdge(meN, 0)
  }

  maxHappiness = 0
  for _, n := range g.nodes {
    happiness := getMaxHappiness(n, g)
    if happiness > maxHappiness {
      maxHappiness = happiness
    }
  }
  fmt.Println("Part two:", maxHappiness)
}

func parseEdgeText(s string) (string, int, string) {
  name, after, _ := strings.Cut(s, " would ")
  deltaStr, toName, _ := strings.Cut(after, " happiness units by sitting next to ")
  deltaTok := strings.Fields(deltaStr)
  direction, amt := deltaTok[0], deltaTok[1]
  val, _ := strconv.Atoi(amt)
  if direction == "lose" {
    val *= -1
  }
  toName = strings.Trim(toName, ".")
  return name, val, toName
}

func getMaxHappiness(n *Node, g Graph) int {
  var maxHappiness int
  pathsQ := make([]([]*Node), 0)
  pathsQ = append(pathsQ, []*Node{n})
  for len(pathsQ) > 0 {
    var p []*Node
    p, pathsQ = pathsQ[0], pathsQ[1:]
    if len(p) == len(g.nodes) {
      pHappiness := getTotalHappiness(p)
      if pHappiness > maxHappiness {
        maxHappiness = pHappiness
      }
    } else {
      remaining := getRemaining(p, g)
      for _, r := range remaining {
        newPath := make([]*Node, len(p))
        copy(newPath, p)
        newPath = append(newPath, r)
        pathsQ = append(pathsQ, newPath)
      }
    }
  }
  return maxHappiness
}

func getRemaining(p []*Node, g Graph) []*Node {
  remaining := []*Node{}
  for _, n := range g.nodes {
    if !includes(p, n) {
      remaining = append(remaining, n)
    }
  }
  return remaining
}

func includes(slice []*Node, item *Node) bool {
  for _, v := range slice {
    if v == item {
      return true
    }
  }
  return false
}

func getTotalHappiness(p []*Node) int {
  var total int
  for i := 0; i < len(p) - 1; i++ {
    curr := p[i]
    next := p[i + 1]
    for _, e := range curr.edges {
      if e.to == next {
        total += e.weight
      }
    }
  }
  for i := len(p) - 1; i > 0; i-- {
    curr := p[i]
    next := p[i - 1]
    for _, e := range curr.edges {
      if e.to == next {
        total += e.weight
      }
    }
  }

  first, last := p[0], p[len(p) - 1]
  for _, e := range first.edges {
    if e.to == last {
      total += e.weight
    }
  }

  for _, e := range last.edges {
    if e.to == first {
      total += e.weight
    }
  }
  return total
}
