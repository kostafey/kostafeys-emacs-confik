package main

import (
	"fmt"
	"strings"
	"io/ioutil"
	"os"
	"unicode/utf8"
	"strconv"
)

func main() {
	if len(os.Args) != 4 {
		fmt.Println("Usage: file start-pos search-string")
		os.Exit(-1)
	}
	fileName := os.Args[1]
	startPos, err := strconv.Atoi(os.Args[2])
	search := strings.Replace(os.Args[3], "\\n", "\n", -1)

	byt, err := ioutil.ReadFile(fileName)
	if err != nil {
		panic(err)
	}
	s := string(byt)

	if startPos > 0 {
		s = string([]rune(s)[startPos:])
	}
	i := strings.Index(s, search)

	fmt.Println(startPos + utf8.RuneCountInString(s[:i]))
}
