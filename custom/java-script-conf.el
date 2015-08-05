;;-----------------------------------------------------------------------------
;; JavaScript IDE
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))

;;------------------------------------------------------------
;; skewer-mode
;;
(add-hook 'js-mode-hook 'skewer-mode)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

(defun skewer-eval-region (start end)
  "Evaluate the region as JavaScript code."
  (interactive "r")
  (skewer-eval (buffer-substring-no-properties start end)
               #'skewer-post-minibuffer))
;;
;;------------------------------------------------------------

(provide 'java-script-conf)
