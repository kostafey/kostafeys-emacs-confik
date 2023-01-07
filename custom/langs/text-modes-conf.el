(require 'elpa-conf)
(use-elpa 'use-package)

;;-------------------------------------------------------------------
;; nxhtml
;; (load (concat site-lisp-path "nxhtml/autostart.el"))

;;-------------------------------------------------------------------
;; htmllize
(require 'htmlize)
(setq htmlize-output-type (quote css))

; This will complete the appropriate close tag after you type </
(setq nxml-slash-auto-complete-flag t)
(add-hook 'html-mode-hook #'(lambda nil (setq sgml-xml-mode t)))
(add-hook 'nxml-mode-hook
          (lambda () (rng-validate-mode 0) )
          t)
(fset 'html-mode 'nxml-mode)

(require 'hl-tags-mode)
(add-hook 'sgml-mode-hook (lambda () (hl-tags-mode 1)))
(add-hook 'nxml-mode-hook (lambda () (hl-tags-mode 1)))

;;-------------------------------------------------------------------
(use-elpa 'lorem-ipsum)

;;-------------------------------------------------------------------
; sh-mode
(add-to-list 'auto-mode-alist '("\\.xsessionrc$" . sh-mode))

;;-------------------------------------------------------------------
; dos-mode
(require 'dos)
(autoload 'dos-mode "dos" "Edit Dos scripts." t)
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))

;;-------------------------------------------------------------------
; log4j-mode
(autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))
(add-hook 'log4j-mode-hook (lambda () (toggle-truncate-lines nil)))

;;-------------------------------------------------------------------
;; PL/SQL
(add-to-list 'auto-mode-alist '("\\.pkh\\'" . sql-mode))
(add-to-list 'auto-mode-alist '("\\.pkb\\'" . sql-mode))

;;-------------------------------------------------------------------
;; reStructuredText settings
;; .. -*- mode: rst -*-
(add-hook 'rst-adjust-hook 'rst-toc-update)

(setq auto-mode-alist
      (append '(("\\.txt$" . rst-mode)
                ("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))

;;-------------------------------------------------------------------
;; Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(use-elpa 'markdown-toc)

;;-------------------------------------------------------------------
;; CSV
(use-elpa 'csv-mode)
;; M-x `csv-align-mode'

(defun k/csv-get-field-index ()
  "Extend `csv--field-index' fn - get field index & handle quotes in data.
Take into account case when CSV data can be quoted, e.g.:
Trades,Header,Currency,Symbol,Date/Time,Quantity
Trades,Data,USD,AAPL,\"2000-01-01, 09:00:00\",10
\"Trades,Data,USD,AAPL,\"\"2000-01-01, 09:00:00\"\",10"
  (let ((curr-idx (csv--field-index)))
    (if (= curr-idx 1)
        curr-idx
      (let ((bound-beg (line-beginning-position))
            (bound-end (line-end-position))
            (delta 0))
        (save-mark-and-excursion
          (while (re-search-backward "(\"|\"\").+,.+(\"|\"\")" bound-beg t)
            (setq delta (+ delta 1))))
        (when (and (save-mark-and-excursion
                     (re-search-backward "\".+,.*" bound-beg t)
                     (not (equal bound-beg (point))))
                   (save-mark-and-excursion
                     (re-search-forward "\"" bound-end t)))
          (setq delta (+ delta 1)))
        (- curr-idx delta)))))

;;-------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("PKGBUILD" . shell-script-mode))
;; .xresources
(add-to-list 'auto-mode-alist '("\\.xresources$" . conf-xdefaults-mode))

;;-------------------------------------------------------------------
;; Org-mode
;;
(setq org-hide-leading-stars t)
(setq org-log-done 'nil) ; Don't show datetime on switching to done.

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((perl . t)
   (ruby . t)
   (shell  . t)
   (python . t)
   (emacs-lisp . t)))

;; Inhibit confirmation before interactively evaluating SQL code
;; blocks in Org buffers.
(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("sql"))))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(setq org-fontify-quote-and-verse-blocks nil)

(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)

;;-------------------------------------------------------------------
;; graphviz-dot-mode
;;
(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

(use-package company-graphviz-dot
  :ensure nil)

(use-elpa 'yaml-mode)

(provide 'text-modes-conf)
