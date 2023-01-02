GO ?= go
EMACS ?= emacs

foreign-fmt.so:
	$(GO) build -buildmode=c-shared -ldflags -s -o $@ $<

clean:
	rm -f foreign-fmt.so

all: clean foreign-fmt.so
