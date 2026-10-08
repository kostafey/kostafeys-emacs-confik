;;; key-bindings.el --- Key bindings reference  -*- lexical-binding: t -*-

;; No code here: each line names a key, its command and the file that
;; binds it.  Bindings in a mode's keymap go under a subheading saying
;; so; `nil' there means the key is unbound to leave the global command.

;;===================================================================
;; Advanced text editing
;;
;; C-S-m       mc/edit-lines                 advanced-text-editing.el
;; C->         mc/mark-next-like-this        advanced-text-editing.el
;; C-<         mc/mark-previous-like-this    advanced-text-editing.el
;; C-M->       mc/mark-all-like-this         advanced-text-editing.el
;;
;;===================================================================

;;===================================================================
;; Advanced navigation
;;
;; C-<f3>      k/highlight-thing-at-point    advanced-navigation.el
;; S-<f3>      highlight-symbol-prev         advanced-navigation.el
;; M-<f3>      highlight-symbol-remove-all   advanced-navigation.el
;; C-M-<up>    highlight-symbol-prev         advanced-navigation.el
;; C-M-<down>  highlight-symbol-next         advanced-navigation.el
;; In isearch
;; C-<f3>      k/highlight-isearch-string    advanced-navigation.el
;; M-a         ace-jump-mode                 advanced-navigation.el
;; C-c M-a     ace-jump-mode-pop-mark        advanced-navigation.el
;; C-M-e       eframe-pop-emacs              advanced-navigation.el
;; C-w         eframe-kill-buffer            advanced-navigation.el
;; C-<next>    eframe-next-buffer            advanced-navigation.el
;; C-<prior>   eframe-previous-buffer        advanced-navigation.el
;; C-x C-c     temporary-persistent-switch-buffer advanced-navigation.el
;;
;;===================================================================

;;===================================================================
;; Point history
;;
;; C-x x       last-change-jump              basic-keys.el
;;
;;===================================================================

;;===================================================================
;; Search & replace
;;
;; C-c r       k/rg                          rg-conf.el
;; In the minibuffer
;; <escape>    abort-recursive-edit          basic-keys.el
;; In markdown-mode, unbound to leave the global commands
;; M-p         nil                           text-modes-conf.el
;; DEL         nil                           text-modes-conf.el
;;
;;===================================================================

;;===================================================================
;; Intellectual point jumps
;;
;; C-M-d       hop-at-point                  advanced-navigation.el
;; C-x d       hop-at-point-other-window     advanced-navigation.el
;; M-S-<left>  hop-backward                  advanced-navigation.el
;; M-S-<right> hop-forward                   advanced-navigation.el
;; <C-mouse-1> hop-by-mouse                  advanced-navigation.el
;; In ibuffer-mode
;; C-x d       ibuffer-visit-buffer-other-window basic-keys.el
;;
;;===================================================================

;;===================================================================
;; Frames & windows
;;
;; M-k f       make-frame                    basic-keys.el
;; M-<left>    meta-left                     basic-keys.el
;; M-<right>   meta-right                    basic-keys.el
;; s-<left>    shrink-window-horizontally    basic-keys.el
;; s-<right>   enlarge-window-horizontally   basic-keys.el
;; S-s-<left>  shrink-window-horizontally    basic-keys.el, by 20
;; S-s-<right> enlarge-window-horizontally   basic-keys.el, by 20
;; s-<down>    shrink-window                 basic-keys.el
;; s-<up>      enlarge-window                basic-keys.el
;;
;;===================================================================

;;===================================================================
;; Web browser
;;
;; C-c g       web-browse-google             basic-keys.el
;; C-c C-g     web-browse-google-query       basic-keys.el
;; C-M-w       web-browse-google-home        basic-keys.el
;; C-x u       web-browse-url                basic-keys.el
;;
;;===================================================================

;; C-SPC       completion-at-point           basic-keys.el

;;===================================================================
;; Function keys
;;
;; <f1>        consult-buffer                minibuffer-conf.el
;; C-M-n       k/project-find-file           project-conf.el
;; <f2>        consult-imenu                 minibuffer-conf.el
;; <f3>        consult-bookmark              minibuffer-conf.el

;; <f4>        k/shell                       shell-conf.el
;; <f5>        dired-open                    dired-conf.el
;; <f6>        k/toggle-tab-line-breadcrumb  appearance.el
;; <f7>        k/rg                          rg-conf.el
;; C-<f7>      k/rg-file                     rg-conf.el

;; <f8>        recode-buffer-rotate-ring     reencoding-file.el
;; C-<f8>      eol-buffer-rotate-ring        reencoding-file.el
;; M-<f8>      describe-coding-system        reencoding-file.el
;; <f9>        auto-fill-mode                basic-keys.el
;; <f10>       smerge-mode                   basic-keys.el
;; <f12>       flyspell-mode                 basic-keys.el
;; Russian words typed past the intended key
;; S-<f12>     k/ru-typo-mode                ru-typo-conf.el
;; C-<f12>     k/ru-typo-correct-word        ru-typo-conf.el
;; M-<f12>     k/ru-typo-accept-word         ru-typo-conf.el
;; yasnippet
;; C-y n       yas-new-snippet               yas-conf.el
;; C-y f       yas-describe-tables           yas-conf.el
;; C-y v       yas-visit-snippet-file        yas-conf.el
;; C-y r       yas-reload-all                yas-conf.el
;; C-<tab>     open-line-or-yas              yas-conf.el
;; C-S-<tab>   yas-prev-field                yas-conf.el
;;
;;===================================================================

;;=============================================================================
;; Mode keys & programming language specific keys.
;;

;;----------------------------------------------------------------------
;; emacs lisp
;; In emacs-lisp-mode
;; C-c p       k/el-insert-eval-last-sexp    emacs-lisp-conf.el
;; C-c C-p     k/el-pprint-eval-last-sexp    emacs-lisp-conf.el
;; C-n e b     k/el-eval-buffer              emacs-lisp-conf.el
;; Eval Emacs Lisp in any mode
;; C-c M-e     eval-last-sexp                emacs-lisp-conf.el
;; C-c M-E     k/el-insert-eval-last-sexp    emacs-lisp-conf.el

;;----------------------------------------------------------------------
;; CIDER - Nrepl.el
;; In clojure-mode
;; C-n j       cider-jack-in                 clojure-conf.el
;; C-n e b     my-cider-eval-buffer          clojure-conf.el
;; C-n q       cider-quit                    clojure-conf.el
;; C-x C-e     k/clojure-eval-last-sexp      clojure-conf.el
;; C-c RET     newline-and-indent            clojure-conf.el
;; M-n         k/clojure-switch-to-current-namespace clojure-conf.el
;; In cider-mode, unbound to leave the global and clojure-mode commands
;; C-c C-f     nil                           clojure-conf.el
;; C-c C-l     nil                           clojure-conf.el
;; C-c RET     nil                           clojure-conf.el
;; C-<f5>      initialize-cljs-repl          clojure-conf.el

;;----------------------------------------------------------------------
;; Scala
;; In scala-mode and scala-ts-mode
;; C-n j       k/scala-start-console-or-switch scala-conf.el
;; C-n c       k/scala-switch-console        scala-conf.el
;; M-e         k/scala-eval-region           scala-conf.el
;; C-n e b     k/scala-eval-buffer           scala-conf.el
;; C-x C-e     k/scala-eval-last-scala-expr  scala-conf.el
;; C-c C-e     k/scala-eval-line             scala-conf.el
;; C-n k       k/scala-compile               scala-conf.el
;; C-c RET     newline-and-indent            scala-conf.el
;; <tab>       k/scala-indent-region         scala-conf.el
;; C-c <tab>   yas-expand                    scala-conf.el

;;----------------------------------------------------------------------
;; Go
;; In go-mode
;; C-c C-c     go-compile                    go-conf.el
;; C-c C-e     go-run                        go-conf.el
;; C-x C-e     go-run                        go-conf.el

;; In rst-mode, unbound to leave the global C-M-a prefix
;; C-M-a       nil                           text-modes-conf.el
;; In tex-mode and latex-mode, unbound to leave the global command
;; C-j         nil                           text-modes-conf.el

;;----------------------------------------------------------------------
;; Version control
;; M-w         get-vc-status                 version-control.el
;; In magit-mode
;; C-w         kill-buffer                   version-control.el
;; S-M-w       magit-copy-buffer-revision    version-control.el
;; M-w         diffview-current              version-control.el
;; C-s-<down>  magit-section-forward         version-control.el
;; C-s-<up>    magit-section-backward        version-control.el
;; In magit-status-mode
;; C-x d       k/magit-diff-visit-worktree-file-other-window version-control.el
;; In pijul-commit-mode, read-only buffers such as *pijul-record-preview*
;; q           k/pijul-commit-quit           pijul-conf.el
;; c           k/pijul-record                pijul-conf.el
;; l           k/pijul-log                   pijul-conf.el
;; d           k/pijul-commit-show-context   pijul-conf.el
;; k           k/pijul-discard               pijul-conf.el
;; RET         k/pijul-commit-visit-file     pijul-conf.el
;; C-x d       k/pijul-commit-visit-file-other-window pijul-conf.el
;; In pijul-commit-mode, while pijul record waits on the buffer
;; C-c C-c     k/pijul-commit-finish         pijul-conf.el
;; C-c C-k     k/pijul-commit-cancel         pijul-conf.el
;; In diffview-mode, scrolling both sides
;; <next>      k/diffview-page-down          version-control.el
;; <prior>     k/diffview-page-up            version-control.el
;; C-<up>      k/diffview-scroll-down-line   version-control.el
;; C-<down>    k/diffview-scroll-up-line     version-control.el
;; <mouse-4>   k/diffview-scroll-down-line   version-control.el
;; <mouse-5>   k/diffview-scroll-up-line     version-control.el
;; In smerge-mode, the smerge-command-prefix
;; C-c s       smerge-basic-map              version-control.el
;;   cheat sheet, bound by smerge-mode itself under that prefix:
;; C-c s n     smerge-next                   next conflict
;; C-c s p     smerge-prev                   previous conflict
;; C-c s RET   smerge-keep-current           keep the side at point
;; C-c s u     smerge-keep-upper             keep <<<<<<< side, also C-c s m
;; C-c s l     smerge-keep-lower             keep >>>>>>> side, also C-c s o
;; C-c s b     smerge-keep-base              keep the base (diff3 style)
;; C-c s a     smerge-keep-all               keep all sides
;; C-c s r     smerge-resolve                resolve automatically
;; C-c s C     smerge-combine-with-next      join with the next conflict
;; C-c s E     smerge-ediff                  resolve in ediff
;; C-c s R     smerge-refine                 highlight changed words
;; C-c s = <   smerge-diff-base-upper        diff base with upper
;; C-c s = >   smerge-diff-base-lower        diff base with lower
;; C-c s = =   smerge-diff-upper-lower       diff upper with lower

;;----------------------------------------------------------------------
;; dired
;; In dired-mode
;; C-<down>    dired-find-file               dired-conf.el
;; C-<up>      dired-up-directory            dired-conf.el
;; M-p         copy-to-clipboard-dired-current-directory dired-conf.el
;; C-<home>    dired-home                    dired-conf.el
;; C-<end>     dired-end                     dired-conf.el

(provide 'key-bindings)

