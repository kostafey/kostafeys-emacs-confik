;;; vterm-conf.el --- vterm terminal configuration.  -*- lexical-binding: t -*-

(straight-use-package
 '(emacs-libvterm :type git :host github
			            :repo "akermu/emacs-libvterm" :branch "master"))

(use-package vterm
  :straight `(vterm
              :type git :host nil
              :repo "https://github.com/akermu/emacs-libvterm"
              :branch "master")
  :bind (:map vterm-mode-map
         ("C-<insert>" . vterm-yank)
         ;; Navigate among windows and frames the same way as in any other
         ;; mode, do not pass M-<arrow> through to the terminal.
         ("M-<left>"   . meta-left)
         ("M-<right>"  . meta-right)
         ("M-<up>"     . windmove-up)
         ("M-<down>"   . windmove-down)))

(provide 'vterm-conf)

;;; vterm-conf.el ends here
