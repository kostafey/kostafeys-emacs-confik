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

;;------------------------------------------------------------
;;        Steps to communicate to the "external" site
;; For example, there is an application running in localhost:8080.
;;
;; 1. (setq httpd-port 8081) in `.emacs'
;; 2. Install `greasemonkey' for Firefox or `tampermonkey' for Chrome.
;; 3. Add `skewer-everything.user.js' script.
;; 4. Modify this script: var host = 'http://localhost:8081';
;; 5. M-x `run-skewer' (get new tab for localhost:8081)
;; 6. M-x `skewer-repl'
;; 7. Reload target "external" localhost:8080 tab in browser

;; At this point, you'll get 2 clients in skewer-repl.
;; So you have to close localhost:8081 tab.
;;------------------------------------------------------------

;; Before M-x `run-skewer' check port 8080 is free.
;; Use (setq httpd-port 8081) otherwise.
(setq httpd-port 8081)

(defun skewer-eval-region (start end)
  "Evaluate the region as JavaScript code."
  (interactive "r")
  (skewer-eval (buffer-substring-no-properties start end)
               #'skewer-post-minibuffer))

;;------------------------------------------------------------
;; Customize js-comint.el for `rhino' and `node.js'
;; M-x `run-js'
;;
(defun rhino-repl ()
  (interactive)
  (setq inferior-js-program-command "java")
  (setq inferior-js-program-arguments
        (list
         "-jar"
         (file-truename "~/.m2/repository/org/mozilla/rhino/1.7.7/rhino-1.7.7.jar")))
  (run-js inferior-js-program-command))

(defun node-repl ()
  (interactive)
  (setq inferior-js-program-command "node")
  (setq inferior-js-program-arguments '("--interactive"))
  (run-js inferior-js-program-command))

;;------------------------------------------------------------
;; tern
;;
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

(provide 'java-script-conf)
