package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"github.com/yosssi/gohtml"
	"github.com/mopemope/emacs-module-go"
)

func xmlpp(ctx emacs.FunctionCallContext) (emacs.Value, error) {
	stdlib := ctx.Environment().StdLib()

	fileName, err := ctx.GoStringArg(0)
	if err != nil {
		return stdlib.Nil(), err
	}
    byt, err := ioutil.ReadFile(fileName)
    if err != nil {
		panic(err)
    }
    s := string(byt)
    html := gohtml.Format(s)

    f, err := os.Create(fileName)
    if err != nil {
        fmt.Println(err)
        return stdlib.Nil(), err
    }
    f.WriteString(html)

    err = f.Close()
    if err != nil {
        fmt.Println(err)
        return stdlib.Nil(), err
    }
	return stdlib.T(), nil
}
