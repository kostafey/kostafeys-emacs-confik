package main

import (
	"olympos.io/encoding/edn"
	"io/ioutil"
	"bytes"
	"github.com/mopemope/emacs-module-go"
)

func ednpp(ctx emacs.FunctionCallContext) (emacs.Value, error) {
	stdlib := ctx.Environment().StdLib()

	fileName, err := ctx.GoStringArg(0)
	if err != nil {
		return stdlib.Nil(), err
	}
	byt, err := ioutil.ReadFile(fileName)
	if err != nil {
		panic(err)
	}

	var opts = edn.PPrintOpts{RightMargin: 2, MiserWidth: 2}
	var result bytes.Buffer
	edn.PPrint(&result, byt, &opts)

	ioutil.WriteFile(fileName, result.Bytes(), 0644);
	return stdlib.T(), nil
}
