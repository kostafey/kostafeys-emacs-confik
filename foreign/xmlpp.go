//Build with: go build ./xmlpp.go

package main

import "fmt"
import "io/ioutil"
import "os"
import "github.com/yosssi/gohtml"

func main() {
    if len(os.Args) != 2 {
		fmt.Println("One argument, the xml/html file to pretty-print is required")
		os.Exit(-1)
    }

    fileName := os.Args[1]
    byt, err := ioutil.ReadFile(fileName)
    if err != nil {
		panic(err)
    }
    s := string(byt)
    html := gohtml.Format(s)

    f, err := os.Create(fileName)
    if err != nil {
        fmt.Println(err)
        return
    }
    f.WriteString(html)

    err = f.Close()
    if err != nil {
        fmt.Println(err)
        return
    }
}
