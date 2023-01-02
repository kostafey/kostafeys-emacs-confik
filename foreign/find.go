package main

import (
	"strings"
	"io/ioutil"
	"unicode/utf8"
	"strconv"
	"github.com/mopemope/emacs-module-go"
)

func find(ctx emacs.FunctionCallContext) (emacs.Value, error) {
	env := ctx.Environment()
	stdlib := ctx.Environment().StdLib()

	fileName, err := ctx.GoStringArg(0)
	if err != nil {
		return stdlib.Nil(), err
	}
	startPosStr, err := ctx.GoStringArg(1)
	if err != nil {
		return stdlib.Nil(), err
	}
	startPos, err := strconv.ParseInt(startPosStr, 10, 0)
	searchRaw, err := ctx.GoStringArg(2)
	if err != nil {
		return stdlib.Nil(), err
	}
	search := strings.Replace(searchRaw, "\\n", "\n", -1)

	byt, err := ioutil.ReadFile(fileName)
	if err != nil {
		panic(err)
	}
	s := string(byt)

	if startPos > 0 {
		s = string([]rune(s)[startPos:])
	}
	i := strings.Index(s, search)
	result := startPos + int64(utf8.RuneCountInString(s[:i]))

	return env.Int(result), nil
}
