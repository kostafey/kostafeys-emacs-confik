;;; key-bindings.el -- A collection of key bindings (default and custom).

(require 'elpa-conf)
(use-elpa 'session)
(use-elpa 'multiple-cursors)
(use-elpa 'flx)
(use-elpa 'flx-ido)
(use-elpa 'flx-isearch)
(require 'ack-conf)
(use-elpa 'highlight-symbol)
(use-elpa 'ace-jump-mode)
(use-elpa 'smex)
(require 'navigation-in-frame)
(require 'completition-conf)
(use-elpa 'popup-switcher)
(use-elpa 'projectile)

(add-to-list 'load-path "~/.emacs.d/eframe-jack-in")
(require 'eframe-jack-in)
(global-set-key (kbd "C-M-e") 'eframe-pop-emacs)
(use-elpa 'temporary-persistent)

(require 'shell-conf)
(require 'dired-conf)
(require 'reencoding-file)
(require 'version-control)
(require 'org)

;;-------------------------------------------------------------------
;; multiple-cursors
;;
(when (require 'multiple-cursors nil 'noerror)
  ;; When you have an active region that spans multiple lines, the
  ;; following will add a cursor to each line:
  (global-set-key (kbd "C-S-m") 'mc/edit-lines)
  ;; When you want to add multiple cursors not based on continuous
  ;; lines, but based on keywords in the buffer, use:
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

;;-------------------------------------------------------------------
;; goto-last-change
;;
(when (require 'session nil 'noerror)
  (setq session-jump-undo-threshold 80)  ; default was 240
  (global-set-key (kbd "C-x x") 'session-jump-to-last-change)
  (global-set-key (kbd "C-M-l") 'session-jump-to-last-change))

;;-------------------------------------------------------------------
;; Search & replace
;;
(global-set-key (kbd "C-s-f") 'flx-isearch-forward)
(global-set-key (kbd "C-s-r") 'flx-isearch-backward)
(global-set-key (kbd "C-c r") 'k/rg)

;; Esc - exit from search
(define-key minibuffer-local-map (kbd "<escape>") 'abort-recursive-edit)

(when (require 'highlight-symbol nil 'noerror)
  (eval-after-load "highlight-symbol"
    '(progn
       (defun k/highlight-region (start end)
         "Toggle highlighting of the region."
         (interactive "r")
         (if mark-active
             (highlight-symbol (buffer-substring start end))
           (highlight-symbol)))
       (global-set-key (kbd "C-<f3>") 'k/highlight-region)
       (global-set-key (kbd "S-<f3>") 'highlight-symbol-prev)
       (global-set-key (kbd "M-<f3>") 'highlight-symbol-remove-all)
       (global-set-key (kbd "C-M-<up>") 'highlight-symbol-prev)
       (global-set-key (kbd "C-M-<down>") 'highlight-symbol-next))))

(defun kostafey-markdown-mode-hook ()
  (define-key markdown-mode-map (kbd "C-M-<up>") 'highlight-symbol-prev)
  (define-key markdown-mode-map (kbd "C-M-<down>") 'highlight-symbol-next)
  (define-key markdown-mode-map (kbd "<backspace>") nil))
(add-hook 'markdown-mode-hook 'kostafey-markdown-mode-hook)

;; ace-jump-mode
(setq ace-jump-mode-scope 'window)
(global-unset-key (kbd "M-a"))
(when (require 'ace-jump-mode nil 'noerror)
  (define-key global-map (kbd "M-a") 'ace-jump-mode))
;;
;;===================================================================

;;===================================================================
;;                           Intellectual point jumps
;;
(require 'hopper)
;; goto definition
(global-set-key (kbd "C-M-d") 'hop-at-point)
(global-set-key (kbd "C-x d") 'hop-at-point-other-window)
(global-set-key (kbd "M-S-<left>") 'hop-backward)
(global-set-key (kbd "M-S-<right>") 'hop-forward)
(global-set-key (kbd "<C-mouse-1>") 'hop-by-mouse)
;;
;;===================================================================

;;===================================================================
;;                              Command executions
;;
;; (global-set-key (kbd "M-x") 'execute-extended-command)
;;
;;===================================================================

;;===================================================================
;;                        Text transformations
;;
(require 'graph-easy)
(global-set-key (kbd "C-c e") 'graph-easy-run)
;;
;;===================================================================

;;===================================================================
;; Buffers navigation
;;
(global-set-key (kbd "C-w") 'eframe-kill-buffer)
(global-set-key [(control next)] 'eframe-next-buffer)      ; C-Page Up
(global-set-key [(control prior)] 'eframe-previous-buffer) ; C-Page Down
(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-k f") 'make-frame)

(when (require 'temporary-persistent nil 'noerror)
  (global-set-key (kbd "C-x C-c") 'temporary-persistent-switch-buffer))

(global-set-key (kbd "C-c g") 'google)
(global-set-key (kbd "C-x g") 'goto-url)
(global-set-key (kbd "C-c C-g") #'(lambda () (interactive) (google -1)))
;;
;;===================================================================

(defun kostafey-markdown-mode-hook ()
  (define-key markdown-mode-map (kbd "M-<left>") nil)
  (define-key markdown-mode-map (kbd "M-<right>") nil))
(add-hook 'markdown-mode-hook 'kostafey-markdown-mode-hook)

(global-set-key (kbd "M-<left>") 'meta-left)
(global-set-key (kbd "M-<right>") 'meta-right)

(global-set-key (kbd "s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<down>") 'shrink-window)
(global-set-key (kbd "s-<up>") 'enlarge-window)
;;
;;===================================================================

;; to 'set-mark-command use M-s instead
(global-set-key (kbd "C-SPC") 'start-complete)
;;-------------------------------------------------------------------
;; auto-complete-mode
(define-key ac-complete-mode-map [next] 'ac-page-next)
(define-key ac-complete-mode-map [prior] 'ac-page-previous)
(define-key ac-complete-mode-map (kbd "C-f") 'ac-isearch)

;;-------------------------------------------------------------------
;; company-mode
(define-key company-search-map (kbd "<escape>") 'company-search-abort)
(define-key company-active-map (kbd "<escape>") 'company-abort)
(define-key company-active-map (kbd "<left>") #'(lambda() (interactive)
                                                  (company-abort)
                                                  (k/char-backward)))
(define-key company-active-map (kbd "<right>") #'(lambda() (interactive)
                                                   (company-abort)
                                                   (k/char-forward)))
(define-key company-active-map (kbd "C-<left>") #'(lambda() (interactive)
                                                    (company-abort)
                                                    (k/word-backward)))
(define-key company-active-map (kbd "C-<right>") #'(lambda() (interactive)
                                                     (company-abort)
                                                     (k/word-forward)))
(define-key company-active-map (kbd "<up>") 'k/company-select-previous)
(define-key company-active-map (kbd "<down>") 'k/company-select-next)
(define-key company-active-map [next] 'company-next-page)
(define-key company-active-map [prior] 'company-previous-page)
(define-key company-active-map (kbd "C-f") 'company-search-candidates)
(define-key company-active-map (kbd "<tab>") 'company-complete-selection)

;;===================================================================
;; Function keys
;;
(global-set-key [f1] 'psw-switch-buffer)
(global-set-key [M-f1] (lambda () (interactive) (psw-switch-buffer t)))
(global-set-key (kbd "C-M-n") 'projectile-find-file)
(global-set-key (kbd "M-n") 'psw-switch-projectile-projects)
(global-set-key (kbd "s-q") 'psw-navigate-files)
(global-set-key (kbd "s-a") 'psw-navigate-files)
(global-set-key [f2] 'psw-switch-function)

(global-set-key [f4] 'k/shell)
(global-set-key [f5] 'dired-open)
(global-set-key [f6] 'switch-completion-frontend)
(global-set-key [f7] 'k/rg)
(global-set-key [S-f7] 'k/ag)
(global-set-key [s-f7] 'ack)
(global-set-key [C-f7] 'ack-file)

(global-set-key [f8] 'recode-buffer-rotate-ring)
(global-set-key [f9] 'auto-fill-mode); enable/disable lines auto-fill
(global-set-key [f10] 'smerge-mode)
(global-set-key [f12] 'flyspell-mode)   ; enable/disable spell checking
;; yasnippet
(defvar yasnippet-prefix "\C-y")
(global-unset-key yasnippet-prefix)
(global-set-key (concat yasnippet-prefix "n") 'yas/new-snippet)
(global-set-key (concat yasnippet-prefix "f") 'yas/find-snippets)
(global-set-key (concat yasnippet-prefix "v") 'yas/visit-snippet-file)
(global-set-key (concat yasnippet-prefix "r") 'yas/reload-all)

(global-set-key (kbd "S-<tab>") 'open-line-or-yas)
(global-set-key (kbd "C-S-<tab>") 'yas-prev-field)
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
  (define-key emacs-lisp-mode-map (kbd "C-c C-p")
    'k/el-pprint-eval-last-sexp)
  (define-key emacs-lisp-mode-map (kbd "C-n e b")
    (lambda () (interactive)
      (eval-buffer)
      (message "Elisp buffer evaluated."))))
(add-hook 'emacs-lisp-mode-hook 'kostafey-elisp-mode-hook)

;; Eval Emacs Lisp in any mode
(global-set-key (kbd "C-c M-e") 'eval-last-sexp)
(global-set-key (kbd "C-c M-E") #'(lambda () (interactive) (eval-last-sexp "-")))

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
  (define-key clojure-mode-map (kbd "C-c C-f") 'ack-file)
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
  (define-key mode-map (kbd "C-n j")   'k/scala-start-console)
  (define-key mode-map (kbd "C-n c")   'k/scala-switch-console)
  (define-key mode-map (kbd "M-e")     'k/scala-eval-region)
  (define-key mode-map (kbd "C-n e b") 'k/scala-eval-buffer)
  (define-key mode-map (kbd "C-x C-e") 'k/scala-eval-last-scala-expr)
  (define-key mode-map (kbd "C-c C-e") 'k/scala-eval-line)
  (define-key mode-map (kbd "C-n k")   'k/scala-compile)
  (define-key mode-map (kbd "C-c RET") 'newline-and-indent)
  (define-key mode-map (kbd "C-c ?")   'lsp-metals-toggle-show-inferred-type)
  (define-key mode-map (kbd "C-c i")   'lsp-java-add-import)
  (define-key mode-map (kbd "M-p")     'copy-to-clipboard-buffer-file-path))
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

;;----------------------------------------------------------------------
;; SQL
;;
(when (require 'ejc-sql nil 'noerror)
  (eval-after-load "ejc-sql"
    '(progn
       (define-key ejc-sql-mode-keymap (kbd "C-S-s-<up>") #'(lambda() (interactive) (ejc-previous-sql t)))
       (define-key ejc-sql-mode-keymap (kbd "C-S-s-<down>") #'(lambda() (interactive) (ejc-next-sql t)))
       (define-key ejc-sql-mode-keymap (kbd "C-s-<up>") 'ejc-previous-sql)
       (define-key ejc-sql-mode-keymap (kbd "C-s-<down>") 'ejc-next-sql)
       (global-set-key (kbd "C-x <up>") 'ejc-show-last-result)
       (global-set-key (kbd "C-x C-s") 'ejc-get-temp-editor-buffer)
       (global-set-key (kbd "C-M-<next>") (lambda ()
                                     (interactive)
                                     (if (equal (buffer-name)
                                                ejc-results-buffer-name)
                                         (ejc-show-next-result))))
       (global-set-key (kbd "C-M-<prior>") (lambda ()
                                             (interactive)
                                             (if (equal (buffer-name)
                                                        ejc-results-buffer-name)
                                                 (ejc-show-prev-result)))))))
;;

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

(global-set-key (kbd "M-w") 'get-vc-status)
(global-set-key (kbd "C-M-w") 'k/multy-magit-status)

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

(when (require 'git-gutter nil 'noerror)
  (global-set-key (kbd "C-M-g <down>") 'git-gutter:next-hunk)
  (global-set-key (kbd "C-M-g <up>") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-M-g p") 'git-gutter:popup-hunk))

(setq smerge-command-prefix (kbd "C-c s"))
(define-key smerge-mode-map (kbd "C-c s n") 'smerge-next)
(define-key smerge-mode-map (kbd "C-c s p") 'smerge-prev)
(define-key smerge-mode-map (kbd "C-c s RET") 'smerge-keep-current)
(define-key smerge-mode-map (kbd "C-c s u") 'smerge-keep-upper)
(define-key smerge-mode-map (kbd "C-c s l") 'smerge-keep-lower)

;;===================================================================
;; Org-mode
;;
(define-key org-mode-map (kbd "C-x t") 'org-todo)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

(define-key dired-mode-map [f1] nil)
(define-key dired-mode-map (kbd "M-z") nil)
(define-key dired-mode-map (kbd "M-p")
  'copy-to-clipboard-dired-current-directory)
(define-key dired-mode-map (kbd "C-<home>") 'dired-home)
(define-key dired-mode-map (kbd "C-<end>") 'dired-end)
(define-key dired-mode-map (kbd "C-<up>") 'diredp-up-directory-reuse-dir-buffer)
(define-key dired-mode-map (kbd "C-<down>") 'diredp-find-file-reuse-dir-buffer)

(provide 'key-bindings)

