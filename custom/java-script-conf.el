;;-----------------------------------------------------------------------------
;; JavaScript IDE
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; ;; in Emacs, M-x byte-compile-file RE js2.el RET
;; ;; (byte-compile-file ".emacs.d/js2.el")

;; (require 'js-comint)
;; ;; Use node as our repl
;; ;; (setq inferior-js-program-command "C:/Progra~1/nodejs/node")
;; (setq inferior-js-program-command "nodejs")
 
;; (setq inferior-js-mode-hook
;;       (lambda ()
;;         ;; We like nice colors
;;         (ansi-color-for-comint-mode-on)
;;         ;; Deal with some prompt nonsense
;;         (add-to-list 'comint-preoutput-filter-functions
;;                      (lambda (output)
;;                        (replace-regexp-in-string 
;;                         ".*1G\.\.\..*5G" "..."
;;                         (replace-regexp-in-string ".*1G.*3G" "&gt;" output))))))

(provide 'java-script-conf)
