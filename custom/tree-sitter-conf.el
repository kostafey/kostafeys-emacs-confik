;;-------------------------------------------------------------------
;; MS Windows: compiler & up-to-date libtree-sitter
;;
;; NOTHING here may come after `(require 'treesit)' below: loading treesit.el
;; already initializes the shared library, and the handle is cached from then
;; on, so a later `dynamic-library-alist' entry is silently ignored.  For the
;; same reason `init.el' requires this module before `text-modes-conf' (which
;; pulls in `asciidoc-mode', a tree-sitter mode).
;;
;; 1. `treesit-install-language-grammar' shells out to a C compiler, probing
;;    exactly "cc", "gcc", "c99" (see `treesit--install-language-grammar-1').
;;    MSYS2 provides all of them in `mingw64/bin', it's just not on PATH.
;;
;; 2. The libtree-sitter.dll bundled with the official Emacs 30.2 build
;;    supports grammar ABI 14 at most, while current grammars (asciidoc among
;;    them) are generated with ABI 15 => `version-mismatch'.  So load a
;;    locally built libtree-sitter 0.25.x instead:
;;
;;      git clone --depth 1 --branch v0.25.10 \
;;        https://github.com/tree-sitter/tree-sitter /tmp/ts25
;;      cc -O2 -std=c11 -Ilib/src -Ilib/include -shared lib/src/lib.c \
;;         -o ~/.emacs.d/lib/libtree-sitter.dll
;;
;;    Do NOT use 0.26.x (nor the MSYS2 `mingw-w64-x86_64-tree-sitter'
;;    package, currently 0.26.9): it dropped the `ts_language_version' export
;;    that Emacs 30.2 resolves on load, so tree-sitter dies completely.
(when (eq system-type 'windows-nt)
  (let ((mingw-bin "C:/msys64/mingw64/bin"))
    (when (file-directory-p mingw-bin)
      (add-to-list 'exec-path mingw-bin)
      (setenv "PATH" (concat mingw-bin ";" (getenv "PATH")))))
  ;; NB: must be a fully expanded path -- `dynamic-library-alist' names go
  ;; straight to LoadLibrary, which does not understand "~".
  (let ((libtreesit (expand-file-name "lib/libtree-sitter.dll"
                                      user-emacs-directory)))
    (when (file-exists-p libtreesit)
      (add-to-list 'dynamic-library-alist
                   ;; Fall back to the bundled one if this copy disappears.
                   (list 'tree-sitter libtreesit "libtree-sitter.dll")))))
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
 ;; NOTE on the asciidoc grammars (used by `asciidoc-mode'):
 ;;
 ;; - they need ABI 15, i.e. the libtree-sitter shim above;
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
 ;;   case it does a plain `git checkout'.  Hence the local clone below; it was
 ;;   made blobless + sparse (`--filter=blob:none', only the two src/ dirs), so
 ;;   re-checking out this SHA works offline, other revisions need network.
 ;;
 ;; - beware: `M-x asciidoc-install-grammars' ignores this alist (it binds its
 ;;   own recipes, unpinned), but it skips languages that already load, so it
 ;;   is a no-op now.  Reinstall via `M-x treesit-install-language-grammar'.
 '((asciidoc "~/.emacs.d/tree-sitter-src/tree-sitter-asciidoc"
             "8d6d71e" "tree-sitter-asciidoc/src")
   (asciidoc-inline "~/.emacs.d/tree-sitter-src/tree-sitter-asciidoc"
                    "8d6d71e" "tree-sitter-asciidoc_inline/src")
   (bash "https://github.com/tree-sitter/tree-sitter-bash")
   (cmake "https://github.com/uyha/tree-sitter-cmake")
   (css "https://github.com/tree-sitter/tree-sitter-css")
   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
   (go "https://github.com/tree-sitter/tree-sitter-go")
   (html "https://github.com/tree-sitter/tree-sitter-html")
   (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
   (json "https://github.com/tree-sitter/tree-sitter-json")
   (make "https://github.com/alemuller/tree-sitter-make")
   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
   (python "https://github.com/tree-sitter/tree-sitter-python")
   (toml "https://github.com/tree-sitter/tree-sitter-toml")
   (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
   (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
   (yaml "https://github.com/ikatyang/tree-sitter-yaml")
   (scala "https://github.com/tree-sitter/tree-sitter-scala")))

;; Check: (treesit-language-available-p 'scala)
;; All TC modes: C-h a -ts-mode$

(when (eq system-type 'gnu/linux)

  (straight-use-package
   '(scala-ts-mode :type git :host gitlab
                   :repo "kostafey/scala-ts-mode" :branch "dev"))
  (add-to-list 'auto-mode-alist '("\\.scala$" . scala-ts-mode))

  (straight-use-package
   '(html-ts-mode :type git :host github
                  :repo "mickeynp/html-ts-mode" :branch "master"))

  (add-to-list 'auto-mode-alist '("\\.xml$" . html-ts-mode)))

;; Decoration level to be used by tree-sitter fontifications.
(setq treesit-font-lock-level 4)

;; Enable/disable font-lock features:
;; (treesit-font-lock-recompute-features)

(provide 'tree-sitter-conf)
