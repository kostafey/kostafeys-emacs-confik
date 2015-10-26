//Build with: go build ./jsonpp.go

package main

import "encoding/json"
import "fmt"
import "io/ioutil"
import "os"

func main() {
    if len(os.Args) != 2 {
		fmt.Println("One argument, the json file to pretty-print is required")
		os.Exit(-1)
    }

    fileName := os.Args[1]
    byt, err := ioutil.ReadFile(fileName)
    if err != nil {
		panic(err)
    }

    var dat map[string] interface{}

    if err := json.Unmarshal(byt, &dat); err != nil {
        panic(err)
    }
    b, err := json.MarshalIndent(dat, "", "  ")
    if err != nil {
		panic(err)
    }
    b2 := append(b, '\n')
	ioutil.WriteFile(fileName, b2, 0644);
}
