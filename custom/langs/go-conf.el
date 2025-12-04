;;; go-conf.el --- go-mode configuration

;;---------------------
;; Environment example:
;;
;; The `GOROOT' points to the directory in which
;; golang was installed:
;;
;; Debian:
;;   export GOROOT=/usr/local/go
;; Fedora:
;;   export GOROOT=/usr/lib/golang
;;
;; export PATH="$PATH:$GOROOT/bin"
;;
;; The `GOPATH' environment variable specifies
;; the location of your workspace:
;;
;; export GOPATH=$HOME/go
;; export PATH=$PATH:$GOPATH/bin

;; --------------------
;; Navigation & go-mode
;; go install github.com/rogpeppe/godef@latest
;; go install golang.org/x/tools/cmd/godoc@latest
;; Assume gofmt provided with golang SDK distribution.
(straight-use-package
 '(go-mode :type git :host github
				   :repo "dominikh/go-mode.el" :branch "master"))

(add-hook 'go-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)))
;; eldoc
(add-hook 'go-mode-hook 'go-eldoc-setup)

;; --------
;; LSP
;; go install golang.org/x/tools/gopls@latest
(use-package lsp-mode
  :straight '(lsp-mode :type git :host github
			                 :repo "emacs-lsp/lsp-mode" :branch "master")
  :hook ((go-mode . lsp-deferred)
         (go-mode . my-common-coding-hook)))

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
