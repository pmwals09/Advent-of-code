package main

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

type T struct {
  res string;
  i int;
}

func main() {
	s, _ := os.ReadFile("./day-04-data.txt")
	seed := strings.TrimSpace(string(s))

	var res string
	var n int

	for ; !strings.HasPrefix(res, "00000"); n++ {
		test := seed + strconv.Itoa(n)
		res = getHash(test)
	}
	fmt.Println("Part one:", n-1)

	for ; !strings.HasPrefix(res, "000000"); n++ {
		test := seed + strconv.Itoa(n)
		res = getHash(test)
	}
	fmt.Println("Part two:", n-1)
}

func getHash(s string) string {
	h := md5.New()
	io.WriteString(h, s)
	return hex.EncodeToString(h.Sum(nil))
}
