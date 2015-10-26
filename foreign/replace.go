package main

import (
	"fmt"
	"strings"
	"io/ioutil"
	"os"
)

func main() {
	if len(os.Args) != 4 {
		fmt.Println("Usage: file old-string new-string")
		os.Exit(-1)
	}

	fileName := os.Args[1]
	byt, err := ioutil.ReadFile(fileName)
	if err != nil {
		panic(err)
	}
	old := strings.Replace(os.Args[2], "\\n", "\n", -1)
	new := strings.Replace(os.Args[3], "\\n", "\n", -1)

	s := string(byt)
	s = strings.Replace(s, old, new, -1)

	ioutil.WriteFile(fileName, []byte(s), 0644);
}
