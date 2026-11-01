;;; clojure-conf.el  -*- lexical-binding: t -*-

(straight-use-package
 '(cider :type git :host github
         :repo "clojure-emacs/cider" :branch "master"))
(straight-use-package
 '(dash :type git :host github
				:repo "magnars/dash.el" :branch "master"))
(straight-use-package
 '(emacs-libvterm :type git :host github
			            :repo "akermu/emacs-libvterm" :branch "master"))

;;----------------------------------------------------------------------
;; clojure
;;
;; eglot runs clojure-lsp, its default entry in `eglot-server-programs'.
;; `clojurescript-mode' and `clojurec-mode' derive from `clojure-mode'
;; and run its hook too.
(add-hook 'clojure-mode-hook #'eglot-ensure)

(add-hook 'cider-mode-hook 'eldoc-mode)

;; Stop the error buffer from popping up while working in the REPL buffer
(setq cider-popup-stacktraces t)

;; Enable error buffer popping also in the REPL
(setq cider-repl-popup-stacktraces t)

;; Make C-c C-z switch to the *nrepl* buffer in the current window
(add-to-list 'same-window-buffer-names "*nrepl*")

;; It will attempt to connect via ssh to remote hosts when unable to connect
;; directly.
(setq nrepl-use-ssh-fallback-for-remote-hosts t)

(defun k/cider-context-buffer-drop-uniquify (params)
  "Forget the `uniquify-managed' the context buffer of PARAMS copied.
`cider--update-project-dir' copies every local variable of the current
buffer into a hidden buffer it never kills, the permanent local
`uniquify-managed' included.  Once the original buffer is killed, the
copy holds a dead buffer, and every later visit of a file of the same
name fails in `uniquify-rationalize-file-buffer-names' with \"Selecting
deleted buffer\" -- a broken `consult' file preview, for one."
  (when-let* ((buffer (plist-get params :--context-buffer)))
    (with-current-buffer buffer
      (kill-local-variable 'uniquify-managed)))
  params)

(advice-add 'cider--update-project-dir :filter-return
            #'k/cider-context-buffer-drop-uniquify)

;; Enabling CamelCase support for editing commands(like forward-word,
;; backward-word, etc) in nREPL is quite useful since we often have to deal with
;; Java class and method names.
(add-hook 'cider-mode-hook 'subword-mode)

(defun my-cider-in-ns ()
  "Switch current namespace to current buffer namespace."
  (interactive)
  (cider-interactive-eval
   (concat "(in-ns '" (cider-current-ns) ")") nil))

(defun my-cider-eval-buffer (&optional buffer)
  "Load (eval) BUFFER's file in nREPL and switch current namespace."
  (interactive)
  (cider-eval-buffer buffer)
  (my-cider-in-ns))

;; C-M-x, C-c C-c
;; `cider-eval-defun-at-point'

(setq cider-default-repl-command "lein")

;;----------------------------------------------------------------------
;; clojure-lsp
;;
;; eglot runs the clojure-lsp it finds on `exec-path' and never installs
;; it: `k/clojure-lsp-install' downloads the latest release there.

(defvar k/clojure-lsp-install-dir
  (file-name-as-directory (expand-file-name "~/.local/bin"))
  "Directory `k/clojure-lsp-install' unpacks clojure-lsp into.")

;; On PATH on Linux, seldom on Windows.
(add-to-list 'exec-path k/clojure-lsp-install-dir t)

(defun k/clojure-lsp-asset ()
  "Return the name of the clojure-lsp release archive for this machine."
  (let ((arch (car (split-string system-configuration "-"))))
    (format "clojure-lsp-native-%s-%s.zip"
            (pcase system-type
              ('windows-nt "windows")
              ('darwin "macos")
              (_ "linux"))
            (if (equal arch "x86_64") "amd64" arch))))

(defun k/clojure-lsp-install ()
  "Download the latest clojure-lsp into `k/clojure-lsp-install-dir'.
curl runs in the background, then the archive is unpacked.  On Windows
shut eglot down before updating: a running clojure-lsp.exe cannot be
overwritten."
  (interactive)
  (let* ((asset (k/clojure-lsp-asset))
         (url (concat "https://github.com/clojure-lsp/clojure-lsp"
                      "/releases/latest/download/" asset))
         (zip (make-temp-file "clojure-lsp-" nil ".zip"))
         (dir k/clojure-lsp-install-dir)
         (unzip (if (eq system-type 'windows-nt)
                    ;; The bsdtar shipped with Windows: a GNU tar of Git
                    ;; or MSYS2 found first on PATH does not read zip.
                    (list (expand-file-name "System32/tar.exe"
                                            (getenv "SystemRoot"))
                          "-xf" zip "-C" dir)
                  (list "unzip" "-oq" zip "-d" dir)))
         (log (get-buffer-create "*clojure-lsp-install*")))
    (make-directory dir t)
    (message "Downloading %s..." url)
    (make-process
     :name "clojure-lsp-install"
     :buffer log
     :command (list "curl" "-fsSL" "-o" zip url)
     :sentinel
     (lambda (process _event)
       (unless (process-live-p process)
         (let ((default-directory dir))
           (if (and (zerop (process-exit-status process))
                    (zerop (apply #'call-process (car unzip) nil log nil
                                  (cdr unzip))))
               (message "Installed %s"
                        (car (process-lines "clojure-lsp" "--version")))
             (message "clojure-lsp install failed, see %s"
                      (buffer-name log))))
         (delete-file zip))))))

;;----------------------------------------------------------------------
;; clojure-script
;;
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojurescript-mode))

(defun initialize-cljs-repl ()
  (interactive)
  (insert "(require 'cljs.repl.browser)")
  (cider-repl-return)
  (insert "(cemerick.piggieback/cljs-repl
  :repl-env (cljs.repl.browser/repl-env :port 9000))")
  (cider-repl-return))

;;----------------------------------------------------------------------
;; eval in vterm
;;

(defun k/vterm-buffer ()
  "Set buffer `vterm-buffer-name' or return nil if buffer does not exist."
  (let ((current-vterm-buffer
         (-find (lambda (b) (equal (buffer-name b) vterm-buffer-name))
                (buffer-list))))
    (when current-vterm-buffer
      (set-buffer current-vterm-buffer))))

(defun k/clojure-send-vterm (data)
  "Send DATA as string to *vterm* buffer."
  (save-excursion
    (when (k/vterm-buffer)
      (vterm-send-string data)
      (vterm-send-return))))

(defun k/clojure-eval-last-sexp-in-vterm ()
  (interactive)
  (k/clojure-send-vterm (prin1-to-string (sexp-at-point))))

(defun k/cider-connected-p ()
  "Return non-nil if CIDER has a live REPL for the current buffer."
  (and (fboundp 'cider-connected-p)
       (cider-connected-p)))

(defun k/clojure-eval-last-sexp ()
  "Evaluate the expression preceding point in CIDER (if exists) or vterm."
  (interactive)
  (if (k/cider-connected-p)
      (cider-eval-last-sexp)
    (k/clojure-eval-last-sexp-in-vterm)))

(defun k/clojure-get-current-namespace ()
  "Return current clojure file namespace as string."
  (save-excursion
    (goto-char (point-min))
    (let* ((beg (progn
                  (re-search-forward "(ns")
                  (point)))
           (end (progn
                  (re-search-forward "$")
                  (point))))
      (string-trim (buffer-substring beg end)))))

(defun k/clojure-switch-to-current-namespace ()
  "Pass `in-ns' expression to vterm."
  (interactive)
  (let ((ns (k/clojure-get-current-namespace)))
    (k/clojure-send-vterm (format "(in-ns '%s)" ns))
    (message ns)))

;;----------------------------------------------------------------------
;; Keys
;;
;; Not use-package :bind: it would autoload these commands from the
;; package's own file, while they come from CIDER or this one.
(with-eval-after-load 'clojure-mode
  (define-key clojure-mode-map (kbd "C-n j") 'cider-jack-in)
  (define-key clojure-mode-map (kbd "C-n e b") 'my-cider-eval-buffer)
  (define-key clojure-mode-map (kbd "C-n q") 'cider-quit)
  (define-key clojure-mode-map (kbd "C-x C-e") 'k/clojure-eval-last-sexp)
  (define-key clojure-mode-map (kbd "C-c RET") 'newline-and-indent)
  (define-key clojure-mode-map (kbd "M-n")
              'k/clojure-switch-to-current-namespace))

;; Once connected, `cider-mode' takes over C-c C-f, C-c C-l and C-c RET;
;; leave C-c C-f to the global `consult-find' and C-c RET to the
;; `clojure-mode-map' binding above.
(with-eval-after-load 'cider-mode
  (define-key cider-mode-map (kbd "C-c C-f") nil)
  (define-key cider-mode-map (kbd "C-c C-l") nil)
  (define-key cider-mode-map (kbd "C-c RET") nil))

(global-set-key (kbd "C-<f5>") 'initialize-cljs-repl)

(provide 'clojure-conf)
