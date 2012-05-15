;;-----------------------------------------------------------------------------
;; JavaScript IDE
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; in Emacs, M-x byte-compile-file RE js2.el RET
;; (byte-compile-file ".emacs.d/js2.el")

(provide 'java-script-conf)
