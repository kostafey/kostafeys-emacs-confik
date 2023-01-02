package main

import (
	"encoding/json"
	"io/ioutil"
	"github.com/mopemope/emacs-module-go"
)

func jsonpp(ctx emacs.FunctionCallContext) (emacs.Value, error) {
	stdlib := ctx.Environment().StdLib()

	fileName, err := ctx.GoStringArg(0)
	if err != nil {
		return stdlib.Nil(), err
	}
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
	return stdlib.T(), nil
}
