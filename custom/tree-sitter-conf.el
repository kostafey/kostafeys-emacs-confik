;;; -*- lexical-binding: t -*-
;;-------------------------------------------------------------------
;; MS Windows: C compiler for grammars
;;
;; `treesit-install-language-grammar' shells out to a C compiler, probing
;; exactly "cc", "gcc", "c99" (see `treesit--install-language-grammar-1').
;; MSYS2 provides all of them in `mingw64/bin', it's just not on PATH.
;;
;; libtree-sitter itself needs nothing: the official Emacs 31 build is
;; compiled against tree-sitter 0.25+, ships libtree-sitter-0.26.dll and
;; loads grammars of ABI 13 to 15 (`dynamic-library-alist' in w32-nt.el).
(when (eq system-type 'windows-nt)
  (let ((mingw-bin "C:/msys64/mingw64/bin"))
    (when (file-directory-p mingw-bin)
      (add-to-list 'exec-path mingw-bin)
      (setenv "PATH" (concat mingw-bin ";" (getenv "PATH"))))))
;;-------------------------------------------------------------------

(require 'treesit)
(require 'straight)

;; Check if Emacs is built with tree-sitter library
;; (treesit-available-p)

;; Grammar libs loaction: `~/.emacs.d/tree-sitter/'
;; Additional directories to look for tree-sitter language definitions:
;; `treesit-extra-load-path'

;; Install via M-x `treesit-install-language-grammar'
;; or
;; (mapc #'treesit-install-language-grammar
;;       (mapcar #'car treesit-language-source-alist))
(setq
 treesit-language-source-alist
 '((bash "https://github.com/tree-sitter/tree-sitter-bash")
   (cmake "https://github.com/uyha/tree-sitter-cmake")
   (css "https://github.com/tree-sitter/tree-sitter-css")
   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
   (go "https://github.com/tree-sitter/tree-sitter-go")
   (html "https://github.com/tree-sitter/tree-sitter-html")
   (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
   (json "https://github.com/tree-sitter/tree-sitter-json")
   (make "https://github.com/alemuller/tree-sitter-make")
   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
   (toml "https://github.com/tree-sitter/tree-sitter-toml")
   (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
   (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
   (yaml "https://github.com/ikatyang/tree-sitter-yaml")
   (scala "https://github.com/tree-sitter/tree-sitter-scala")))

;;-------------------------------------------------------------------
;; The asciidoc grammars (used by `asciidoc-mode') are registered separately,
;; and only where their local clone exists -- currently the MS Windows box.
;; Listing them unconditionally breaks grammar installation everywhere else:
;; `treesit' would `git clone' a path that is not there, which also takes down
;; the install-everything form above.  To enable them on another machine:
;;
;;   git clone --filter=blob:none --sparse \
;;     https://github.com/cathaysia/tree-sitter-asciidoc \
;;     ~/.emacs.d/tree-sitter-src/tree-sitter-asciidoc
;;   cd ~/.emacs.d/tree-sitter-src/tree-sitter-asciidoc
;;   git sparse-checkout set tree-sitter-asciidoc/src tree-sitter-asciidoc_inline/src
;;   git checkout 8d6d71e
;;
;; Why it looks like this:
;;
;; - the grammars need ABI 15, i.e. libtree-sitter 0.25 or later;
;;
;; - they are pinned to 8d6d71e, the last commit before the grammar renamed
;;   the `ltalic' node to `italic'.  asciidoc-mode still queries `(ltalic)',
;;   so at grammar HEAD every inline font-lock query dies with
;;   `treesit-query-error'.  The older v0.9.0 tag is no good either: it
;;   predates `typographic_quote', which asciidoc-mode also queries.  Drop the
;;   pin once asciidoc-mode catches up with the rename;
;;
;; - the pin is a bare SHA, and `treesit' passes a revision to `git clone -b'
;;   (tags/branches only) -- unless the "URL" is a local repository, in which
;;   case it does a plain `git checkout'.  Hence the local clone; blobless +
;;   sparse, so re-checking out this SHA works offline, other revisions need
;;   network;
;;
;; - beware: `M-x asciidoc-install-grammars' ignores this alist (it binds its
;;   own recipes, unpinned), but it skips languages that already load, so it
;;   is a no-op once they are installed.  Reinstall via
;;   `M-x treesit-install-language-grammar'.
(let ((asciidoc-src (expand-file-name "tree-sitter-src/tree-sitter-asciidoc"
                                      user-emacs-directory)))
  (when (file-directory-p asciidoc-src)
    (setq treesit-language-source-alist
          (append `((asciidoc ,asciidoc-src
                              "8d6d71e" "tree-sitter-asciidoc/src")
                    (asciidoc-inline ,asciidoc-src
                                     "8d6d71e" "tree-sitter-asciidoc_inline/src"))
                  treesit-language-source-alist))))
;;-------------------------------------------------------------------

;; Check: (treesit-language-available-p 'scala)
;; All TC modes: C-h a -ts-mode$

(when (eq system-type 'gnu/linux)

  (straight-use-package
   '(html-ts-mode :type git :host github
                  :repo "mickeynp/html-ts-mode" :branch "master"))

  (add-to-list 'auto-mode-alist '("\\.xml$" . html-ts-mode)))

;; Java and Python: `java-ts-mode' and `python-ts-mode' come with Emacs,
;; which pins the grammar recipe they are written for (so the list above
;; leaves these languages out) and offers to install it when the mode is
;; turned on: run M-x java-ts-mode or M-x python-ts-mode in such a buffer
;; once.  Until the grammar is there, `java-mode' or `python-mode' stays.
(when (treesit-language-available-p 'java)
  (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode)))
(when (treesit-language-available-p 'python)
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

;; Scala: until the grammar is installed (M-x treesit-install-language-grammar
;; RET scala, by the recipe above), `scala-mode' stays.
(straight-use-package
 `(scala-ts-mode :type git :host nil
                 :repo ,(pcase system-type
                          ('windows-nt
                           "https://github.com/KaranAhlawat/scala-ts-mode.git")
                          ('gnu/linux
                           "git@github.com:KaranAhlawat/scala-ts-mode.git"))
                 :branch "main"))
(when (treesit-language-available-p 'scala)
  (add-to-list 'major-mode-remap-alist '(scala-mode . scala-ts-mode)))

;; Decoration level to be used by tree-sitter fontifications.
(setq treesit-font-lock-level 4)

;; Enable/disable font-lock features:
;; (treesit-font-lock-recompute-features)

(provide 'tree-sitter-conf)
