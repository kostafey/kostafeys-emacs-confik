;;; go-mode configuration

;;---------------------
;; Environment example:
;;
;; The `GOROOT' points to the directory in which
;; golang was installed:
;;
;; export GOROOT=/usr/local/go
;; export PATH=$PATH:$GOROOT/bin
;;
;; The `GOPATH' environment variable specifies
;; the location of your workspace:
;;
;; export GOPATH=$HOME/data/go
;; export PATH=$PATH:$GOPATH/bin

;; --------------------
;; Navigation & go-mode
;; go get github.com/rogpeppe/godef
(require 'go-mode)
;; Assume godoc & gofmt provided with golang SDK distribution.

;;---------------
;; Autocompletion
;; - linux
;; go get -u github.com/nsf/gocode
;; - windows
;; go get -u -ldflags -H=windowsgui github.com/nsf/gocode
(require 'go-autocomplete)
;; eldoc
(add-hook 'go-mode-hook 'go-eldoc-setup)

;; --------
;; Flycheck
(when (require 'flycheck nil)
  (add-hook 'go-mode-hook (lambda () (flycheck-mode t))))

;; ----
;; Lint
;; go get -u golang.org/x/lint/golint
(add-to-list 'load-path (concat (getenv "GOPATH")
                                "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

;; -----------
;; Compilation
(defvar go-compile-command
  "go generate && go build -ldflags \"-s\" -v && go test -v && go vet")

(defvar go-compile-ad-hoc t)

(defun my-go-mode-hook ()
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           go-compile-command)))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(defun go-compile ()
  (interactive)
  (compile (if go-compile-ad-hoc
               (concat "go build " (buffer-file-name))
             go-compile-command)))

(defun go-run ()
  (interactive)
  (compile
   (format "go run \"%s\""(buffer-file-name))))

(provide 'go-conf)
