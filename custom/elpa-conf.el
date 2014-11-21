(require 'cl)
(require 'package)

(if (equal system-name "kgsedykh-pc")
    (setq url-proxy-services
          (list (let ((proxy (split-string (getenv "http_proxy") "://")))
                  (cons (car proxy) (cadr proxy))))))

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(defvar package-is-refreshed nil
  "Holds the flag if the `package-refresh-contents' already done.")

(when (not package-archive-contents)
  (progn
    (package-refresh-contents)
    (setq package-is-refreshed t)))

(defun required-packages-installed-p (required-packages)
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun prompt-package-install (package)
  (if (y-or-n-p (format "Package %s is missing. Install it? " package))
      (package-install package)))

(defun install-required-packages (required-packages)
  (unless (required-packages-installed-p required-packages)

    ;; check for new packages (package versions)
    (if (not package-is-refreshed)
        (progn
          (message "%s" "Emacs is now refreshing its package database...")
          (package-refresh-contents)
          (setq package-is-refreshed t)
          (message "%s" " done.")))

    ;; install the missing packages
    (dolist (package required-packages)
      (unless (package-installed-p package)
        (prompt-package-install package)))))


(defvar text-modes-required-packages
  (list 'org
        'org-plus-contrib
        'log4j-mode
        'lorem-ipsum
        'markdown-mode
        'markdown-toc
        'typing
        'sphinx-frontend
        ;; news reader
        'elfeed
        ;; View Large Files
        'vlf)
  "Required packages for `text-modes-conf'.")

(defvar bte-required-packages
  (list 'browse-kill-ring
        'wrap-region)
  "Required packages for `basic-text-editing'.")

(defvar clojure-packages '(clojure-mode
                           clojure-mode-extra-font-locking
                           cider
                           ac-nrepl
                           clojure-snippets
                           typed-clojure-mode)
  "Required packages for clojure coding.")

(defvar scala-packages '(ensime
                         sbt-mode)
  "Required packages for scala coding.")

(defvar ac-required-packages
  (list 'popup
        'auto-complete
        'ac-etags
        'yasnippet
        'icomplete+)
  "Required packages for autocompletition.")

(defvar laf-required-packages
  (list 'fill-column-indicator
        'highlight-parentheses
        'idle-highlight-mode
        ;; 'popwin
        'tabbar
        'rainbow-mode
        'highlight-symbol
        'minimap
        ;; 'powerline
        'popup-switcher
        'sr-speedbar
        'rainbow-delimiters
        'organic-green-theme
        'cl-lib-highlight
        )
  "Required packages for `look-and-feel'.")

(defvar nav-keys-required-packages
  (list 'goto-last-change
        'multiple-cursors
        'dired+
        'flx
        'flx-ido
        'smex
        'ace-jump-mode
        'flx-isearch)
  "Required packages for `navigation-and-simplify-keys', `key-bindings'
and `buffer-navigation'.")

(defvar vc-packages
  (list 'magit
        'ahg
        )
  "Required packages for `version-control'.")

(defvar prog-modes-packages
  (list 'auctex
        'quack
        'ecb
        'projectile
        'javadoc-lookup
        'web-mode
        'diff-hl
        'paredit
        'lua-mode
        'erlang)
  "Packages, requred by misc programming modes.")

(defvar js-packages
  (list 'simple-httpd
        'js2-mode
        'skewer-mode)
  "Required packages for `java-script-conf'.")


(defvar misc-packages
  (list 'noflet
        'jabber)
  "Packages, not requred by configuration files.")

(install-required-packages (append
                            bte-required-packages
                            text-modes-required-packages
                            clojure-packages
                            scala-packages
                            ac-required-packages
                            laf-required-packages
                            nav-keys-required-packages
                            prog-modes-packages
                            vc-packages
                            misc-packages
                            js-packages))

(provide 'elpa-conf)
