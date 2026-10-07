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

(provide 'advanced-text-editing)
