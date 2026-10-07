;;; advanced-text-editing.el --- Third-party editing  -*- lexical-binding: t -*-

;; The counterpart of `basic-text-editing', built on third-party packages.

;;-------------------------------------------------------------------
;; multiple-cursors
;;
(use-package multiple-cursors
  :straight (multiple-cursors :type git :host github
                              :repo "magnars/multiple-cursors.el"
                              :branch "master")
  ;; Add a cursor to each line of an active region spanning multiple lines,
  ;; or to the next, previous or all occurrences of the region text.
  :bind (("C-S-m" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M->" . mc/mark-all-like-this)))

;;-------------------------------------------------------------------
;; highlight-symbol
;;
(use-package highlight-symbol
  :straight (highlight-symbol :type git :host github
                              :repo "nschum/highlight-symbol.el"
                              :branch "master")
  :preface
  (defun k/highlight-thing-at-point ()
    "Toggle highlighting of the symbol at point, or of the active region."
    (interactive)
    (if (use-region-p)
        (highlight-symbol
         (regexp-quote (buffer-substring (region-beginning) (region-end))))
      (highlight-symbol)))

  (defun k/highlight-isearch-string ()
    "Toggle highlighting of the current search string."
    (interactive)
    (when (string-empty-p isearch-string)
      (user-error "Empty search string"))
    ;; `highlight-symbol' takes a regexp.
    (highlight-symbol (substring-no-properties
                       (if isearch-regexp
                           isearch-string
                         (regexp-quote isearch-string)))))
  :bind (("C-<f3>" . k/highlight-thing-at-point)
         ("S-<f3>" . highlight-symbol-prev)
         ("M-<f3>" . highlight-symbol-remove-all)
         ("C-M-<up>" . highlight-symbol-prev)
         ("C-M-<down>" . highlight-symbol-next)
         :map isearch-mode-map
         ("C-<f3>" . k/highlight-isearch-string)))

(provide 'advanced-text-editing)
