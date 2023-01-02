package main

import (
	"strings"
	"io/ioutil"
	"github.com/mopemope/emacs-module-go"
)

func replace(ctx emacs.FunctionCallContext) (emacs.Value, error) {
	stdlib := ctx.Environment().StdLib()

	fileName, err := ctx.GoStringArg(0)
	if err != nil {
		return stdlib.Nil(), err
	}
	byt, err := ioutil.ReadFile(fileName)
	if err != nil {
		panic(err)
	}
	old, err := ctx.GoStringArg(1)
	if err != nil {
		return stdlib.Nil(), err
	}
	new, err := ctx.GoStringArg(2)
	if err != nil {
		return stdlib.Nil(), err
	}

	s := string(byt)
	s = strings.Replace(s, old, new, -1)

	ioutil.WriteFile(fileName, []byte(s), 0644);
	return stdlib.T(), nil
}
