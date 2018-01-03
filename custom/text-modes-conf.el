(require 'elpa-conf)

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
; dos-mode
(autoload 'dos-mode "dos" "Edit Dos scripts." t)
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))

;;-----------------------------------------------------------------------------
; log4j-mode
(autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))

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
(add-to-list 'auto-mode-alist '("PKGBUILD" . shell-script-mode))

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
   (sh . t)
   (python . t)
   (emacs-lisp . t)))

(defun sql-ansi-mode ()
  (interactive)
  (sql-mode)
  (sql-set-product "ansi"))

(eval-after-load "sql"
  '(progn
     (sql-set-product "ansi")))

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
