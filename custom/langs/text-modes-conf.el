;;; text-modes-conf.el

(straight-use-package 'org)

;;-------------------------------------------------------------------
;; Wrap text with punctation or tag
(when (require 'wrap-region nil 'noerror)
  (wrap-region-global-mode t)
  (wrap-region-add-wrapper "*" "*")
  (wrap-region-add-wrapper "`" "`"))

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

;; If non-nil, make sure that the skeleton inserted ends with a newline.
;; Used in commands like `html-span'.
(setq skeleton-end-newline nil)

(define-skeleton html-Red
  "HTML Red tag."
  nil
  "<Red>" _ "</Red>")

(define-skeleton html-Teal
  "HTML Teal tag."
  nil
  "<Teal>" _ "</Teal>")

(define-skeleton html-li
  "HTML li tag."
  nil
  "<li>" _ "</li>")

(define-skeleton html-span
  "HTML li tag."
  nil
  "<span>" _ "</span>")

(define-skeleton html-Box
  "HTML Box tag."
  nil
  "<Box>" _ "</Box>")

;;-------------------------------------------------------------------
(use-package lorem-ipsum
  :straight '(lorem-ipsum :type git :host github
			                    :repo "jschaf/emacs-lorem-ipsum" :branch "master"))

;;-------------------------------------------------------------------
;; sh-mode
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

(use-package markdown-toc
  :straight '(markdown-toc :type git :host github
			                     :repo "ardumont/markdown-toc" :branch "master"))

;;-------------------------------------------------------------------
;; CSV
(straight-use-package 'csv-mode)
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
(use-package verb
  :straight '(verb :type git :host github
			             :repo "federicotdn/verb" :branch "main"))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config (progn
            (define-key org-mode-map (kbd "C-c C-r") verb-command-map)

            (defun k/verb-send ()
              (interactive)
              (verb-kill-all-response-buffers 1)
              (verb-send-request-on-point-other-window-stay))

            (define-key org-mode-map (kbd "C-c C-c") 'k/verb-send)
            (setq org-hide-leading-stars t)
            (setq org-log-done 'nil) ; Don't show datetime on switching to done.
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((perl . t)
               (ruby . t)
               (shell  . t)
               (python . t)
               (emacs-lisp . t)))))

;; Inhibit confirmation before interactively evaluating SQL code
;; blocks in Org buffers.
(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("sql"))))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(setq org-fontify-quote-and-verse-blocks nil)

(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)

;; Allow Emacs to handle the passphrase input in the minibuffer
;; instead of a separate dialog.
(setq epg-pinentry-mode 'loopback)

;;-------------------------------------------------------------------
;; graphviz-dot-mode
;;
(use-package graphviz-dot-mode
  :straight '(graphviz-dot-mode :type git :host github
			                          :repo "ppareit/graphviz-dot-mode"
                                :branch "master")
  :config
  (progn
    (setq graphviz-dot-indent-width 4)
    (add-hook 'graphviz-dot-mode-hook 'company-mode)))

(use-package yaml-mode
  :straight '(yaml-mode :type git :host github
			                          :repo "yoshiki/yaml-mode"
                                :branch "master"))

(use-package dockerfile-mode
  :straight '(dockerfile-mode :type git :host github
			                        :repo "spotify/dockerfile-mode"
                              :branch "master"))

(use-package nginx-mode
  :straight '(nginx-mode :type git :host github
			                   :repo "ajc/nginx-mode"
                         :branch "master")
  :config
  (progn
    (setq nginx-indent-level 2)))

(provide 'text-modes-conf)
