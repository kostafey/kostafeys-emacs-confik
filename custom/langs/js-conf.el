(require 'elpa-conf)
(use-elpa 'skewer-mode)
(use-elpa 'rjsx-mode)
(use-elpa 'npm-mode)
(use-elpa 'web-mode)
(require 'typescript-mode nil 'noerror)

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
;; 8. At this point, you'll get 2 clients in skewer-repl.
;;    So you have to close localhost:8081 tab.
;;------------------------------------------------------------

;; Before M-x `run-skewer' check port 8080 is free.
;; Use (setq httpd-port 8081) otherwise.
(setq httpd-port 8081)

(defun skewer-eval-region (start end)
  "Evaluate the region as JavaScript code."
  (interactive "r")
  (skewer-eval (buffer-substring-no-properties start end)
               #'skewer-post-minibuffer))

(when (require 'skewer-mode nil 'noerror)
  (define-key skewer-mode-map
    (kbd "C-c C-p") 'skewer-pprint-eval-last-expression))

;;------------------------------------------------------------
;; npm-mode
;;
(when (require 'npm-mode nil 'noerror)
  (npm-global-mode)

  (defun k/npm-mode-build ()
    (interactive)
    (npm-mode--exec-process "npm run build --prefer-offline --no-audit"))

  (let ((mode-maps (list js-mode-map js2-mode-map js-json-mode-map
                         js-jsx-mode-map web-mode-map)))
    (when (boundp 'typescript-mode-map)
      (add-to-list mode-maps typescript-mode-map))
    (mapcar
     (lambda (mm)
       (define-key mm
         (kbd "C-c C-c") 'k/npm-mode-build))
     mode-maps)))

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

;;------------------------------------------------------------
;; web-beautify
;; npm -g install js-beautify
;; M-x `web-beautify-js'

(add-hook 'js-mode-hook #'(lambda () (setq js2-basic-offset 2)))
(add-hook 'rjsx-mode #'(lambda () (setq js2-basic-offset 2)))

;;------------------------------------------------------------
;; JavaScript IDE
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)) ; js-ts-mode
(add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode)) ; js-ts-mode
;; (wrap-region-add-wrapper "{/*" "*/}" "/" 'rjsx-mode)
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))

;; `lsp-server'
;; npm install -g eslint
(setq flycheck-checker-error-threshold 10000)

;;------------------------------------------------------------
;; web-mode
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ftl$" . web-mode))
(setq web-mode-enable-current-element-highlight t)
(setq-default web-mode-markup-indent-offset 4)

;;------------------------------------------------------------
;; rjsx-mode
(require 'basic)

(defun k/rjsx-forward (&optional select)
  (interactive)
  (if (eq ?< (char-after))
      (progn
        (k/char-forward select)
        (rjsx-jump-closing-tag)
        (k/sexp-forward select))
    (k/sexp-forward select)))

(defun k/rjsx-backward (&optional select)
  (interactive)
  (if (eq ?> (char-before))
      (progn
        (k/char-backward select)
        (rjsx-jump-opening-tag)
        (k/sexp-backward select))
    (k/sexp-backward select)))

(define-key rjsx-mode-map (kbd "C-M-<right>") 'k/rjsx-forward)
(define-key rjsx-mode-map (kbd "C-M-<left>") 'k/rjsx-backward)
(define-key rjsx-mode-map (kbd "C-S-M-<right>")
  #'(lambda () (interactive) (k/rjsx-forward t)))
(define-key rjsx-mode-map (kbd "C-S-M-<left>")
  #'(lambda () (interactive) (k/rjsx-backward t)))

(provide 'js-conf)
