(require 'elpa-conf)

;;-----------------------------------------------------------------------------
;; shell
(setq w32-quote-process-args t)

;; Windows shell (cmd) correct encoding
(when (eq system-type 'windows-nt)
  (defadvice shell (after my-shell-advice)
    (set-buffer-process-coding-system 'cp1251 'cp1251))
  (ad-activate 'shell))

(setq stock-ticker-symbols
      '("^gspc" "DIA" "^ixic" "^tnx" "^tyx" "^nya"
        "XAUUSD=X" "GBPUSD=X" "EURUSD=X"
        "GOOGL" "AAPL" "IBM" "TSLA" "MSFT" "T" "ORCL"
        "SIE.DE" "F" "WMT" "MCD" "NVS" "PG" "BAYN.DE" "JPM"
        "BZU15.NYM" "RUB=x" "EURRUB=x"))

;;-----------------------------------------------------------------------------
;; nxhtml
;; (load (concat site-lisp-path "nxhtml/autostart.el"))

;;-----------------------------------------------------------------------------
;; html-изация
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

;;-----------------------------------------------------------------------------
(require 'lorem-ipsum)

;;-----------------------------------------------------------------------------
; sh-mode
(add-to-list 'auto-mode-alist '("\\.xsessionrc$" . sh-mode))

;;-----------------------------------------------------------------------------
; dos-mode
(autoload 'dos-mode "dos" "Edit Dos scripts." t)
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))

;;-----------------------------------------------------------------------------
; log4j-mode
(autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))
(add-hook 'log4j-mode-hook (lambda () (toggle-truncate-lines nil)))

;;-----------------------------------------------------------------------------
;; PL/SQL
(add-to-list 'auto-mode-alist '("\\.pkh\\'" . sql-mode))
(add-to-list 'auto-mode-alist '("\\.pkb\\'" . sql-mode))

;;-----------------------------------------------------------------------------
;; reStructuredText settings
;; .. -*- mode: rst -*-
(add-hook 'rst-adjust-hook 'rst-toc-update)

(setq auto-mode-alist
      (append '(("\\.txt$" . rst-mode)
                ("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))

;;-----------------------------------------------------------------------------
;; Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(require 'markdown-toc)

;;-----------------------------------------------------------------------------
;; CSV
(require 'csv-mode)
;; M-x `csv-align-mode'

(defun k/csv-get-field-index ()
  "Extend `csv--field-index' fn - get field index & handle quotes in data.
Take into account case when CSV data can be quoted, e.g.:
Trades,Header,Currency,Symbol,Date/Time,Quantity
Trades,Data,USD,AAPL,\"2000-01-01, 09:00:00\",10"
  (let ((curr-idx (csv--field-index))
        (bound-beg (line-beginning-position))
        (bound-end (line-end-position))
        (delta 0))
    (save-mark-and-excursion
      (while (re-search-backward "\".+,.+\"" bound-beg)
        (setq delta (+ delta 1))))
    (when (and (save-mark-and-excursion
                 (re-search-backward "\".+,.*" bound-beg))
               (save-mark-and-excursion
                 (re-search-forward "\"" bound-end)))
      (setq delta (+ delta 1)))
    (- curr-idx delta)))

;;-----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("PKGBUILD" . shell-script-mode))
;; .xresources
(add-to-list 'auto-mode-alist '("\\.xresources$" . conf-xdefaults-mode))

;;-----------------------------------------------------------------------------
;; Org-mode settings
;; (add-to-list 'load-path (concat site-lisp-path "org-7.6/lisp/"))
;; (require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)
;введенный пароль не будет кешироваться
(setq epa-file-cache-passphrase-for-symmetric-encryption nil)

(setq org-hide-leading-stars t)

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

;;--------------------------------------------------------------------
;;typing game
(autoload 'typing-of-emacs "The Typing Of Emacs, a game." t)

;;--------------------------------------------------------------------
;; elfeed
(setq elfeed-feeds
      '("http://nullprogram.com/feed/"
        "http://batsov.com/atom.xml"
        "http://www.masteringemacs.org/feed/"
        "http://planet.emacsen.org/atom.xml"
        "http://planet.emacsen.org/ru/atom.xml"))

(provide 'text-modes-conf)
