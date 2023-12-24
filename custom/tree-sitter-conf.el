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
   (python "https://github.com/tree-sitter/tree-sitter-python")
   (toml "https://github.com/tree-sitter/tree-sitter-toml")
   (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
   (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
   (yaml "https://github.com/ikatyang/tree-sitter-yaml")
   (scala "https://github.com/tree-sitter/tree-sitter-scala")))

;; Check: (treesit-language-available-p 'scala)
;; All TC modes: C-h a -ts-mode$

(straight-use-package
 '(scala-ts-mode :type git :host github :repo "KaranAhlawat/scala-ts-mode"))

(add-to-list 'auto-mode-alist '("\\.scala$" . scala-ts-mode))

;; Decoration level to be used by tree-sitter fontifications.
(setq treesit-font-lock-level 4)

;; Enable/disable font-lock features:
;; (treesit-font-lock-recompute-features)

(provide 'tree-sitter-conf)
