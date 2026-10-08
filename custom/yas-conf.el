;;; yas-conf.el -- Yet Another Snippet extension configuration  -*- lexical-binding: t -*-

(use-package yasnippet
  :straight (yasnippet :type git :host github
                       :repo "joaotavora/yasnippet" :branch "master")
  :demand t
  :init
  ;; Declared after yasnippet, so as not to register it with another
  ;; recipe, and before loading it: the collection adds its directory
  ;; once yasnippet is loaded, ahead of the personal snippets below.
  (straight-use-package
   '(yasnippet-snippets :type git :host github
                        :repo "AndreaCrotti/yasnippet-snippets"
                        :branch "master"))
  ;; C-y is the prefix for the snippet commands; cua-mode yanks with C-v.
  (global-unset-key (kbd "C-y"))
  :bind (("C-y n" . yas-new-snippet)
         ("C-y f" . yas-describe-tables)
         ("C-y v" . yas-visit-snippet-file)
         ("C-y r" . yas-reload-all)
         ("C-<tab>" . open-line-or-yas)
         ("C-S-<tab>" . yas-prev-field))
  :config
  ;; personal snippets
  (setq yas-snippet-dirs
        (append yas-snippet-dirs
                (list "~/.emacs.d/custom/mysnippets")))
  (yas-global-mode 1))

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
