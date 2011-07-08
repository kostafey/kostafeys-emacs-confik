(add-to-list 'load-path (concat site-lisp-path "python-mode/"))
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

(setenv "PYTHONPATH" (concat "I:\\WorkDir\\workspace\\PyLib\\src;"
                (getenv "PYTHONPATH")))

(provide 'python-conf)
