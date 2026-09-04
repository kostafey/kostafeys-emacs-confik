;;; yas-conf.el -- Yet Another Snippet extension configuration

(use-package yasnippet
  :vc (:url "https://github.com/joaotavora/yasnippet.git"
       :branch "master")
  :defer t)
(use-package yasnippet-snippets
  :vc (:url "https://github.com/AndreaCrotti/yasnippet-snippets.git"
       :branch "master")
  :defer t)

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
