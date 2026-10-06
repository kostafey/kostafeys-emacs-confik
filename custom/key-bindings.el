;;; key-bindings.el -- A collection of key bindings (default and custom).  -*- lexical-binding: t -*-

(use-package multiple-cursors
  :straight (multiple-cursors :type git :host github
                              :repo "magnars/multiple-cursors.el"
                              :branch "master")
  ;; Add a cursor to each line of an active region spanning multiple lines,
  ;; or to the next, previous or all occurrences of the region text.
  :bind (("C-S-m" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M->" . mc/mark-all-like-this)))
(use-package ace-jump-mode
  :straight `(ace-jump-mode
              :type git :host nil
              :repo ,(pcase system-type
                       ('windows-nt
                        "https://github.com/winterTTr/ace-jump-mode.git")
                       ('gnu/linux
                        "git@github.com:winterTTr/ace-jump-mode.git"))
              :branch "master")
  :bind (("M-a" . ace-jump-mode)
         ("C-c M-a" . ace-jump-mode-pop-mark))
  :custom
  (ace-jump-mode-scope 'window))

(require 'navigation-in-frame)
(require 'project-conf)

(use-package eframe-jack-in
  :straight (eframe-jack-in :type git :host github
                            :repo "kostafey/eframe-jack-in"
                            :branch "master")
  ;; It sets up hooks and advice on load, so load it right away.
  :demand t
  :bind (("C-M-e" . eframe-pop-emacs)
         ("C-w" . eframe-kill-buffer)
         ("C-<next>" . eframe-next-buffer)
         ("C-<prior>" . eframe-previous-buffer))
  :config
  (require 'eframe-windmove))

(use-package temporary-persistent
  :straight `(temporary-persistent
              :type git :host nil
              :repo ,(pcase system-type
                       ('windows-nt
                        "https://github.com/kostafey/temporary-persistent.git")
                       ('gnu/linux
                        "git@github.com:kostafey/temporary-persistent.git"))
              :branch "master")
  ;; Desktop restores the temp buffers at startup; loaded right away, it
  ;; saves them on exit and lists them in its `consult-buffer' source.
  :demand t
  :bind (("C-x C-c" . temporary-persistent-switch-buffer))
  :config
  (setq temporary-persistent-default-major-mode 'markdown-mode)
  ;; `temporary-persistent' does not pull in `consult' itself, and `consult'
  ;; is loaded lazily, so register the source once `consult' is there.
  (with-eval-after-load 'consult
    (add-to-list 'consult-buffer-sources 'temporary-persistent-consult-source t)
    (add-to-list 'consult-buffer-filter "\\`\\*temp\\(-[0-9]+\\)?\\*\\'")))

(require 'shell-conf)
(require 'dired-conf)
(require 'reencoding-file)
(require 'version-control)

(use-package highlight-symbol
  :straight (highlight-symbol :type git :host github
                              :repo "nschum/highlight-symbol.el"
                              :branch "master")
  :preface
  (defun k/highlight-thing-at-point ()
    "Toggle highlighting of the symbol at point, or of the active region."
    (interactive)
    (if (use-region-p)
        (highlight-symbol
         (regexp-quote (buffer-substring (region-beginning) (region-end))))
      (highlight-symbol)))

  (defun k/highlight-isearch-string ()
    "Toggle highlighting of the current search string."
    (interactive)
    (when (string-empty-p isearch-string)
      (user-error "Empty search string"))
    ;; `highlight-symbol' takes a regexp.
    (highlight-symbol (substring-no-properties
                       (if isearch-regexp
                           isearch-string
                         (regexp-quote isearch-string)))))
  :bind (("C-<f3>" . k/highlight-thing-at-point)
         ("S-<f3>" . highlight-symbol-prev)
         ("M-<f3>" . highlight-symbol-remove-all)
         ("C-M-<up>" . highlight-symbol-prev)
         ("C-M-<down>" . highlight-symbol-next)
         :map isearch-mode-map
         ("C-<f3>" . k/highlight-isearch-string)))
;;
;;===================================================================

;; C-SPC      completion-at-point           basic-keys.el

;;===================================================================
;; Function keys
;;
;; <f1>       consult-buffer                minibuffer-conf.el
;; C-M-n      k/project-find-file           project-conf.el
;; <f2>       consult-imenu                 minibuffer-conf.el
;; <f3>       consult-bookmark              minibuffer-conf.el

;; <f4>       k/shell                       shell-conf.el
;; <f5>       dired-open                    dired-conf.el
;; <f6>       k/toggle-tab-line-breadcrumb  appearance.el
;; <f7>       k/rg                          rg-conf.el
;; C-<f7>     k/rg-file                     rg-conf.el

;; <f8>       recode-buffer-rotate-ring     reencoding-file.el
;; C-<f8>     eol-buffer-rotate-ring        reencoding-file.el
;; M-<f8>     describe-coding-system        reencoding-file.el
;; <f9>       auto-fill-mode                basic-keys.el
;; <f10>      smerge-mode                   basic-keys.el
;; <f12>      flyspell-mode                 basic-keys.el
;; Russian words typed past the intended key
;; S-<f12>    k/ru-typo-mode                ru-typo-conf.el
;; C-<f12>    k/ru-typo-correct-word        ru-typo-conf.el
;; M-<f12>    k/ru-typo-accept-word         ru-typo-conf.el
;; yasnippet
;; C-y n      yas-new-snippet               yas-conf.el
;; C-y f      yas-describe-tables           yas-conf.el
;; C-y v      yas-visit-snippet-file        yas-conf.el
;; C-y r      yas-reload-all                yas-conf.el
;; S-<tab>    open-line-or-yas              yas-conf.el
;; C-S-<tab>  yas-prev-field                yas-conf.el
;;
;;===================================================================


;;===================================================================

(defun kostafey-lsp-signature-mode-map ()
  (define-key lsp-signature-mode-map (kbd "M-a") 'ace-jump-mode)
  (define-key lsp-signature-previous (kbd "M-p") 'copy-to-clipboard-buffer-file-path))
(add-hook 'lsp-signature-mode-map-hook 'kostafey-lsp-signature-mode-map)

;;=============================================================================
;; Mode keys & programming language specific keys.
;;
;;----------------------------------------------------------------------
;; Java
(defun kostafey-java-mode-hook ()
  (define-key java-mode-map (kbd "C-a") nil)
  (define-key java-mode-map (kbd "C-h j") 'javadoc-lookup)
  (define-key java-mode-map (kbd "C-<f1>") 'javadoc-lookup)
  (define-key java-mode-map (kbd "C-M-d") 'hop-at-point))
(add-hook 'java-mode-hook 'kostafey-java-mode-hook)

(global-set-key (kbd "C-<f10>") 'tomcat-toggle)
(global-set-key (kbd "C-<f9>") 'maven-tomcat-deploy)

;;----------------------------------------------------------------------
;; lisp
(defun kostafey-lisp-mode-hook ()
  (define-key lisp-mode-map (kbd "M-p") 'copy-to-clipboard-buffer-file-path)
  (define-key lisp-mode-map (kbd "C-c h") 'slime-hyperspec-lookup)
  (define-key slime-mode-map (kbd "M-p") 'copy-to-clipboard-buffer-file-path)
  (define-key slime-mode-map (kbd "C-c h") 'slime-hyperspec-lookup))
(add-hook 'lisp-mode-hook 'kostafey-lisp-mode-hook)
(add-hook 'slime-mode-hook 'kostafey-lisp-mode-hook)

;;----------------------------------------------------------------------
;; emacs lisp
(defun kostafey-elisp-mode-hook ()
  (define-key emacs-lisp-mode-map (kbd "C-c p")
    'k/el-insert-eval-last-sexp)
  (define-key emacs-lisp-mode-map (kbd "C-c C-p")
    'k/el-pprint-eval-last-sexp)
  (define-key emacs-lisp-mode-map (kbd "C-n e b")
    (lambda () (interactive)
      (eval-buffer)
      (message "Elisp buffer evaluated."))))
(add-hook 'emacs-lisp-mode-hook 'kostafey-elisp-mode-hook)

;; Eval Emacs Lisp in any mode
(global-set-key (kbd "C-c M-e") 'eval-last-sexp)
(global-set-key (kbd "C-c M-E") 'k/el-insert-eval-last-sexp)

;;----------------------------------------------------------------------
;; CIDER - Nrepl.el
;;
(require 'clojure-conf)
(global-unset-key (kbd "C-n"))
(defun kostafey-clojure-mode-hook ()
  (define-key clojure-mode-map (kbd "C-c C-p") 'cider-pprint-eval-last-sexp)
  (define-key clojure-mode-map (kbd "C-n j") 'cider-jack-in)
  (define-key clojure-mode-map (kbd "C-n e b") 'my-cider-eval-buffer)
  (define-key clojure-mode-map (kbd "C-x C-e") 'k/clojure-eval-last-sexp)
  (define-key clojure-mode-map (kbd "C-n q") 'cider-quit)
  (define-key clojure-mode-map (kbd "C-h j") 'javadoc-lookup)
  (define-key clojure-mode-map (kbd "C-M-d") 'hop-at-point)
  (define-key clojure-mode-map (kbd "C-c C-l") nil)
  (define-key clojure-mode-map (kbd "C-c C-f") nil)
  (define-key clojure-mode-map (kbd "C-c RET") 'newline-and-indent)
  (define-key clojure-mode-map (kbd "M-n") 'k/clojure-switch-to-current-namespace))
(add-hook 'clojure-mode-hook 'kostafey-clojure-mode-hook)
(global-set-key (kbd "C-<f5>") 'initialize-cljs-repl)

(defun kostafey-lua-mode-hook ()
  (define-key lua-mode-map (kbd "C-c C-c") 'lua-send-current-line)
  (define-key lua-mode-map (kbd "M-e") 'lua-send-region)
  (define-key lua-mode-map (kbd "C-x C-e") 'lua-eval-last-expr)
  (define-key lua-mode-map (kbd "C-M-<right>") 'lua-goto-forward)
  (define-key lua-mode-map (kbd "C-M-<left>") 'lua-goto-backward)
  (define-key lua-mode-map (kbd "C-M-S-<right>") 'lua-goto-forward-select)
  (define-key lua-mode-map (kbd "C-M-S-<left>") 'lua-goto-backward-select))
(add-hook 'lua-mode-hook 'kostafey-lua-mode-hook)

;;----------------------------------------------------------------------
;; Scala
;;
(defun kostafey-scala-mode-hook (mode-map)
  (define-key mode-map (kbd "C-n j")   'k/scala-start-console-or-switch)
  (define-key mode-map (kbd "C-n c")   'k/scala-switch-console)
  (define-key mode-map (kbd "M-e")     'k/scala-eval-region)
  (define-key mode-map (kbd "C-n e b") 'k/scala-eval-buffer)
  (define-key mode-map (kbd "C-x C-e") 'k/scala-eval-last-scala-expr)
  (define-key mode-map (kbd "C-c C-e") 'k/scala-eval-line)
  (define-key mode-map (kbd "C-n k")   'k/scala-compile)
  (define-key mode-map (kbd "C-c RET") 'newline-and-indent)
  (define-key mode-map (kbd "C-c ?")   'lsp-metals-toggle-show-inferred-type)
  (define-key mode-map (kbd "M-p")     'copy-to-clipboard-buffer-file-path)
  (define-key mode-map (kbd "<tab>")   'k/scala-indent-region)
  (define-key mode-map (kbd "C-c <tab>") 'yas-expand))
(add-hook 'scala-mode-hook #'(lambda () (kostafey-scala-mode-hook scala-mode-map)))
(add-hook 'scala-ts-mode-hook #'(lambda () (kostafey-scala-mode-hook scala-ts-mode-map)))

;;----------------------------------------------------------------------
;; Tcl
;;
(defun kostafey-tcl-mode-hook ()
  (define-key tcl-mode-map (kbd "M-e") 'tcl-eval-region)
  (define-key tcl-mode-map (kbd "C-c C-c")
    #'(lambda() (interactive)
       (save-excursion
         (let ((beg (point))
               (end (progn
                      (beginning-of-line)
                      (point))))
           (tcl-eval-region end beg))))))
(add-hook 'tcl-mode-hook 'kostafey-tcl-mode-hook)

;; (require 'go-conf)
;; (define-key go-mode-map (kbd "C-c C-c") 'go-compile)
;; (define-key go-mode-map (kbd "C-c C-e") 'go-run)
;; (define-key go-mode-map (kbd "C-x C-e") 'go-run)

(require 'rst)
(define-key rst-mode-map (kbd "C-M-a") nil)

(defun k/LaTeX-mode-hook ()
  (define-key LaTeX-mode-map  (kbd "C-j") 'join-next-line-space-n))
(add-hook 'LaTeX-mode-hook 'k/LaTeX-mode-hook)

;;----------------------------------------------------------------------
;; Version control
;;
(global-unset-key (kbd "M-w"))
(defun kostafey-magit-mode-hook ()
  (define-key magit-mode-map (kbd "C-w") 'kill-buffer)
  (define-key magit-mode-map (kbd "S-M-w") 'magit-copy-buffer-revision)
  (define-key magit-mode-map (kbd "M-w") 'diffview-current)
  (define-key magit-mode-map (kbd "C-s-<down>") 'magit-section-forward)
  (define-key magit-mode-map (kbd "C-s-<up>") 'magit-section-backward))
(add-hook 'magit-mode-hook 'kostafey-magit-mode-hook)

(defun k/magit-status-mode-hook ()
  (define-key magit-status-mode-map (kbd "C-x d")
              'k/magit-diff-visit-worktree-file-other-window))
(add-hook 'magit-status-mode-hook 'k/magit-status-mode-hook)

(global-set-key (kbd "M-w") 'get-vc-status)

(eval-after-load "diffview"
  '(progn
     (defun do-side-by-side (action)
       (funcall action)
       (other-window 1)
       (funcall action)
       (other-window 1))

     (defun kostafey-diffview-mode-hook ()
       (define-key diffview-mode-map [next]
         #'(lambda nil (interactive)
             (do-side-by-side #'(lambda nil (pager-page-down)))))
       (define-key diffview-mode-map [prior]
         #'(lambda nil (interactive)
             (do-side-by-side #'(lambda nil (pager-page-up)))))
       (define-key diffview-mode-map (kbd "C-<up>")
         #'(lambda nil (interactive)
             (do-side-by-side #'(lambda nil (scroll-down-line 1)))))
       (define-key diffview-mode-map (kbd "C-<down>")
         #'(lambda nil (interactive)
             (do-side-by-side #'(lambda nil (scroll-up-line 1)))))
       (define-key diffview-mode-map (kbd "<mouse-4>")
         #'(lambda nil (interactive)
             (do-side-by-side #'(lambda nil (scroll-down-line 1)))))
       (define-key diffview-mode-map (kbd "<mouse-5>")
         #'(lambda nil (interactive)
             (do-side-by-side #'(lambda nil (scroll-up-line 1))))))
     (add-hook 'diffview-mode-hook 'kostafey-diffview-mode-hook)))

(setq smerge-command-prefix (kbd "C-c s"))
(defun kostafey-smerge-mode-hook ()
  (define-key smerge-mode-map (kbd "C-c s n") 'smerge-next)
  (define-key smerge-mode-map (kbd "C-c s p") 'smerge-prev)
  (define-key smerge-mode-map (kbd "C-c s RET") 'smerge-keep-current)
  (define-key smerge-mode-map (kbd "C-c s u") 'smerge-keep-upper)
  (define-key smerge-mode-map (kbd "C-c s l") 'smerge-keep-lower))
(add-hook 'smerge-mode-hook 'kostafey-smerge-mode-hook)

;;----------------------------------------------------------------------
;; dired
;;
(defun kostafey-dired-mode-hook ()
  (define-key dired-mode-map [f1] nil)
  (define-key dired-mode-map (kbd "M-z") nil)
  (define-key dired-mode-map (kbd "M-p")
              'copy-to-clipboard-dired-current-directory)
  (define-key dired-mode-map (kbd "C-<home>") 'dired-home)
  (define-key dired-mode-map (kbd "C-<end>") 'dired-end)
  (define-key dired-mode-map (kbd "C-<up>") 'diredp-up-directory-reuse-dir-buffer)
  (define-key dired-mode-map (kbd "C-<down>") 'diredp-find-file-reuse-dir-buffer))
(add-hook 'dired-mode-hook 'kostafey-dired-mode-hook)

(provide 'key-bindings)

