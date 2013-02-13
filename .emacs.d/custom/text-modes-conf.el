(require 'elpa-conf)

;;-----------------------------------------------------------------------------
;; nxhtml
;; (load (concat site-lisp-path "nxhtml/autostart.el"))

;;-----------------------------------------------------------------------------
;; html-изация
(require 'htmlize)
(setq htmlize-output-type (quote css))

; This will complete the appropriate close tag after you type </
(setq nxml-slash-auto-complete-flag t)

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

;;-----------------------------------------------------------------------------
;; reStructuredText settings
;; .. -*- mode: rst -*-
(add-hook 'rst-adjust-hook 'rst-toc-update)

(setq auto-mode-alist
      (append '(("\\.txt$" . rst-mode)
                ("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))

;;-----------------------------------------------------------------------------
;; Org-mode settings
;; (add-to-list 'load-path (concat site-lisp-path "org-7.6/lisp/"))
;; (require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;введенный пароль не будет кешироваться
(setq epa-file-cache-passphrase-for-symmetric-encryption nil)

(setq org-hide-leading-stars t)

(defun sql-ansi-mode ()
  (interactive)
  (sql-mode)
  (sql-set-product "ansi"))

(eval-after-load "sql"
  '(progn
     (sql-set-product "ansi")))

;;typing game
(autoload 'typing-of-emacs "The Typing Of Emacs, a game." t)

(provide 'text-modes-conf)
