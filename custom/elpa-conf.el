(require 'cl)
(require 'package)

(if (equal system-name "kgsedykh-pc")
    (setq url-proxy-services
          (list (let ((proxy (split-string (getenv "http_proxy") "://")))
                  (cons (car proxy) (cadr proxy))))))

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("m-stable" . "http://stable.melpa.org/packages/")
                         ;;("org" . "http://orgmode.org/elpa/")
						 ))

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

(defun install-required-packages (required-packages)
  (unless (required-packages-installed-p required-packages)
    ;; check for new packages (package versions)
    (if (not package-is-refreshed)
        (progn
          (message "%s" "Emacs is now refreshing its package database...")
          (package-refresh-contents)
          (setq package-is-refreshed t)
          (message "%s" " done.")))
    (let ((uninsalled-packages '()))
      (dolist (package required-packages)
        (unless (package-installed-p package)
          (setq uninsalled-packages (cons package uninsalled-packages))))
      (if (y-or-n-p (format "Packages %s are missing. Install them?"
                            uninsalled-packages))
          ;; install the missing packages
          (dolist (package uninsalled-packages)
            (package-install package))))))

(defvar text-modes-required-packages
  (list 'org
        ;'org-plus-contrib
        'log4j-mode
        'lorem-ipsum
        'markdown-mode
        'markdown-toc
        'typing
        'sphinx-frontend
        ;; news reader
        'elfeed
        ;; View Large Files
        'vlf
        'yaml-mode
        'restclient
        'google-translate)
  "Required packages for `text-modes-conf'.")

(defvar clojure-packages '(clojure-mode
                           clojure-mode-extra-font-locking
                           cider
                           ac-cider
                           ;; clojure-snippets
                           ;; typed-clojure-mode
                           flycheck-clojure)
  "Required packages for clojure coding.")

(defvar common-lisp-packages '(slime
                               elisp-slime-nav
                               ac-slime)
  "Required packages for common-lisp coding.")

(defvar go-packages '(go-mode
                      go-autocomplete
                      go-eldoc)
  "Required packages for go (golang) coding.")

(defvar rust-packages '(rust-mode
                        racer
                        cargo
                        flycheck-rust)
  "Required packages for rust coding.")

(defvar scala-packages '(ensime
                         sbt-mode)
  "Required packages for scala coding.")

(defvar ac-required-packages
  (list ;'popup
        'auto-complete
        'ac-etags
        'yasnippet)
  "Required packages for autocompletition.")

(defvar laf-required-packages
  (list 'fill-column-indicator
        'highlight-parentheses
        'idle-highlight-mode
        ;; 'popwin
        'tabbar
        'rainbow-mode
        'highlight-symbol
        ;; 'minimap
        ;; 'powerline
        'popup-switcher
        'sr-speedbar
        'rainbow-delimiters
        'organic-green-theme
        'cl-lib-highlight
        'flycheck-tip
        'doom-themes
        'nord-theme)
  "Required packages for `look-and-feel'.")

(defvar nav-keys-required-packages
  (list 'goto-last-change
        'multiple-cursors
        'temporary-persistent
        'flx
        'flx-ido
        'smex
        'ace-jump-mode
        'flx-isearch
        'rg)
  "Required packages for `navigation-in-buffer', `key-bindings'
and `navigation-in-frame'.")

(defvar vc-packages
  (list 'magit
        'ahg
        'diffview
        'git-gutter
        'git-gutter-fringe
        'darcsum)
  "Required packages for `version-control'.")

(defvar prog-modes-packages
  (list 'auctex
        ;;'quack
        'ecb
        'projectile
        'flycheck
        'javadoc-lookup
        ;; 'jdee
        'groovy-mode
        'web-mode
        'diff-hl
        'paredit
        'paredit-everywhere
        'lua-mode
        'erlang
        'php-mode
        'ejc-sql
        'powershell)
  "Packages, requred by misc programming modes.")

(defvar js-packages
  (list 'simple-httpd
        'js2-mode
        'skewer-mode
        'js-comint
        'tern
        'tern-auto-complete
        'web-beautify
        'rjsx-mode
        'npm-mode
        'typescript-mode)
  "Required packages for `java-script-conf'.")


(defvar misc-packages
  (list 'noflet
        'jabber
        'circe
        'ag
        'names
        'undercover)
  "Packages, not requred by configuration files.")

(defvar misc-requred-packages
  (list 's
        'java-snippets)
  "Packages, requred by configuration files.")

(install-required-packages (append
                            text-modes-required-packages
                            clojure-packages
                            scala-packages
                            common-lisp-packages
                            go-packages
                            rust-packages
                            ac-required-packages
                            laf-required-packages
                            nav-keys-required-packages
                            prog-modes-packages
                            vc-packages
                            misc-packages
                            misc-requred-packages
                            js-packages))

(provide 'elpa-conf)
