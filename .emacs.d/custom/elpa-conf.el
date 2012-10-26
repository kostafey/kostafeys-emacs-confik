(require 'cl)
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar package-is-refreshed nil
  "Holds the flag if the `package-refresh-contents' already done.")

(when (not package-archive-contents)
  (progn
    (package-refresh-contents)
    (setq package-is-refreshed t)))

(defun reqired-packages-installed-p (reqired-packages)
  (loop for p in reqired-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun prompt-package-install (package)  
  (if (y-or-n-p (format "Package %s is missing. Install it? " package))
      (package-install package)))

(defun install-reqired-packages (reqired-packages)
  (unless (reqired-packages-installed-p reqired-packages)
    
    ;; check for new packages (package versions)    
    (if (not package-is-refreshed)
        (progn
          (message "%s" "Emacs is now refreshing its package database...")
          (package-refresh-contents)
          (setq package-is-refreshed t)
          (message "%s" " done.")))
    
    ;; install the missing packages
    (dolist (package reqired-packages)
      (unless (package-installed-p package)
        (prompt-package-install package)))))

(provide 'elpa-conf)

