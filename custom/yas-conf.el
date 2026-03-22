;;; yas-conf.el -- Yet Another Snippet extension configuration

(straight-use-package
 '(yasnippet :type git :host github
				     :repo "joaotavora/yasnippet" :branch "master"))
(straight-use-package
 '(yasnippet-snippets :type git :host github
				              :repo "AndreaCrotti/yasnippet-snippets" :branch "master"))

(yas-global-mode 1)
;; personal snippets
(setq yas-snippet-dirs
      (append yas-snippet-dirs
              (list "~/.emacs.d/custom/mysnippets")))

(defun yas/next-field-or-maybe-expand-1 ()
  (interactive)
  (let ((yas/fallback-behavior 'return-nil))
    (unless (yas/expand)
      (yas/next-field))))

(defun open-line-or-yas ()
  (interactive)
  (cond ((and (looking-back " ") (looking-at "[\s\n}]+"))
     (insert "\n\n")
     (indent-according-to-mode)
     (previous-line)
     (indent-according-to-mode))
    ((expand-abbrev))
    (t
     (setq *yas-invokation-point* (point))
     (yas/next-field-or-maybe-expand-1))))

(provide 'yas-conf)
