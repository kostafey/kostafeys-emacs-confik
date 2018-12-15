(add-to-list 'load-path (concat site-lisp-path "python-mode-6.0/"))

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(require 'python-mode)

(add-to-list 'load-path (concat site-lisp-path "pymacs/"))
(require 'pymacs)

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)  
(autoload 'pymacs-exec "pymacs" nil t)  
(autoload 'pymacs-load "pymacs" nil t)  

(setenv "PYMACS_PYTHON" "python")
(pymacs-load "ropemacs" "rope-")

(setq ropemacs-enable-autoimport t)

(setenv "PYTHONPATH" (concat "I:\\WorkDir\\workspace\\PyLib\\src;"
                (getenv "PYTHONPATH")))

(defun python-shell ()
  "Creates python shell in the same frame"
  (interactive)
  (progn
	(py-shell)
	(delete-frame)
	(split-window-vertically)
	(next-multiframe-window)
	(switch-to-buffer "*Python*")
	(shrink-window 7)
	(previous-multiframe-window)))
(global-set-key (kbd "C-c C-p") 'run-python-shell)

;; (defun python-run-module ()
;;   (interactive)
;;   (switch-to-buffer "*Python*")
;;   (pymacs-exec "import qwe")
;;   )

;; py-execute-import-or-reload

(provide 'python-conf)

