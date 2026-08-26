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

;; `markdown-syntax-propertize-comments' gives up scanning the whole region as
;; soon as it meets a `<!--' that sits inside inline code or a code block, so
;; every real comment after e.g. a literal `` `<!-- x -->` `` in the text loses
;; its comment syntax.  Skip such a match and keep scanning instead.
(defun k/markdown-syntax-propertize-comments (start end)
  "Match HTML comments from the START to END."
  (let (finish)
    (goto-char start)
    (while (not finish)
      (let* ((in-comment (nth 4 (syntax-ppss)))
             (comment-begin (nth 8 (syntax-ppss))))
        (cond
         ;; Comment start
         ((and (not in-comment)
               (re-search-forward markdown-regex-comment-start end t))
          (if (or (markdown-inline-code-at-point-p)
                  (markdown-code-block-at-point-p))
              ;; Not a comment after all, look for the next one.
              (goto-char (min (match-end 0) end (point-max)))
            (let ((open-beg (match-beginning 0)))
              (put-text-property open-beg (1+ open-beg)
                                 'syntax-table (string-to-syntax "<"))
              (goto-char (min (1+ (match-end 0)) end (point-max))))))
         ;; Comment end
         ((and in-comment comment-begin
               (re-search-forward markdown-regex-comment-end end t))
          (let ((comment-end (match-end 0)))
            (put-text-property (1- comment-end) comment-end
                               'syntax-table (string-to-syntax ">"))
            ;; Remove any other text properties inside the comment
            (remove-text-properties comment-begin comment-end
                                    markdown--syntax-properties)
            (put-text-property comment-begin comment-end
                               'markdown-comment (list comment-begin comment-end))
            (goto-char (min comment-end end (point-max)))))
         ;; Nothing found
         (t (setq finish t)))))
    nil))

(with-eval-after-load 'markdown-mode
  (advice-add 'markdown-syntax-propertize-comments
              :override #'k/markdown-syntax-propertize-comments))

;; HTML comments are fontified with `markdown-comment-face' via
;; `font-lock-syntactic-face-function', but keyword fontification wins over it
;; on heading lines, so `# Title <!-- note -->' loses the comment face.
;; Re-apply the face on top of the regions markdown-mode has propertized as
;; comments.
(defun k/markdown-match-comment (last)
  "Match the next `<!-- ... -->' comment region up to LAST."
  (markdown-match-propertized-text 'markdown-comment last))

(defun k/markdown-comment-highlight-initialize ()
  (font-lock-add-keywords
   nil '((k/markdown-match-comment . (0 'markdown-comment-face t)))
   'append))

(add-hook 'markdown-mode-hook 'k/markdown-comment-highlight-initialize)

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

;; After installing the package, run:
;; M-x `asciidoc-install-grammars'
;;
;; Keep this deferred (the autoloads already claim `.adoc'/`.asciidoc').
;; Loading asciidoc-mode eagerly pulls in `flymake' while `load-path' still
;; only has the built-in one -- straight's `flymake' (an eglot dependency)
;; joins `load-path' later, in `scala-conf'.  `eglot' then greets every LSP
;; buffer with `require-with-check' => "Feature `flymake' is now provided by a
;; different file".  Deferring means the require happens after init, when
;; straight's copy is the one that gets loaded.
(use-package asciidoc-mode
  :defer t
  :straight (asciidoc-mode
             :type git :host github
             :repo "bbatsov/asciidoc-mode"
             :branch "main"))

(provide 'text-modes-conf)
