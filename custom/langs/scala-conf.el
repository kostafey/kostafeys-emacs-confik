;;; scala-conf.el --- Scala configuration for lsp

;;; Commentary:

;; # Make sure to use coursier v1.1.0-M9 or newer.
;; `Linux':
;; curl -L -o coursier https://git.io/coursier
;; chmod +x coursier
;; ./coursier bootstrap \
;;   --java-opt -Xss4m \
;;   --java-opt -Xms100m \
;;   --java-opt -Dmetals.client=emacs \
;;   org.scalameta:metals_2.12:0.9.4 \
;;   -r bintray:scalacenter/releases \
;;   -r sonatype:snapshots \
;;   -o /home/kostafey/data/soft/bin/metals-emacs -f
;;
;; `Windows':
;; set BIN_PATH=C:\bin\
;; curl -L -o "%BIN_PATH%coursier" https://git.io/coursier-cli
;; curl -L -o "%BIN_PATH%coursier.bat" https://git.io/coursier-bat
;; coursier bootstrap ^
;;   --java-opt -Xss4m ^
;;   --java-opt -Xms100m ^
;;   --java-opt -Dmetals.client=emacs ^
;;   org.scalameta:metals_2.12:0.9.4 ^
;;   -r bintray:scalacenter/releases ^
;;   -r sonatype:snapshots ^
;;   -o %BIN_PATH%metals-emacs -f

;; Run for new projects:
;; M-x `lsp-metals-build-import'

;; C-n j (`k/scala-start-console')

;;; Code:

(use-elpa 'use-package)
(require 'use-package)

;; Enable defer and ensure by default for use-package
;; Keep auto-save/backup files separate from source code:  https://github.com/scalameta/metals/issues/1027
(setq use-package-always-defer t
      use-package-always-ensure t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Enable scala-mode and sbt-mode
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :config (setq scala-indent:step 2
                scala-indent:indent-value-expression t
                scala-indent:align-parameters t
                scala-indent:align-forms t
                scala-indent:default-run-on-strategy scala-indent:reluctant-strategy))

(defun k/scala-toggle-indent:step (arg)
  "Toggle Scala indent step. When ARG is defined, set it as a step value."
  (interactive "P")
  (if arg
      (setq scala-indent:step arg)
    (if (equal scala-indent:step 2)
        (setq scala-indent:step 4)
      (setq scala-indent:step 2)))
  (message (format "set scala-indent:step %s"
                   (propertize (number-to-string scala-indent:step)
                               'face 'font-lock-keyword-face))))

(use-elpa 'restclient)
(add-to-list 'auto-mode-alist '("\\routes$" . restclient-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (setq sbt:program-options
        '(;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
          "-Dsbt.supershell=false"
          ;; sbt console on windows: https://github.com/hvesalai/emacs-sbt-mode/issues/44
          "-Djline.terminal=jline.UnsupportedTerminal")))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck)

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook (scala-mode . lsp)
  :config (progn
            (setq lsp-ui-doc-show-with-mouse nil)
            (setq lsp-prefer-flymake nil)
            (setq lsp-before-save-edits nil)
            (setq lsp-ui-sideline-diagnostic-max-lines 8)))

;; Add metals backend for lsp-mode
(use-package lsp-metals)

(use-package lsp-ui)

(defun k/scala-indent-region (beg end)
  "Indent region or current line in Scala file."
  (interactive "r")
  (if (not mark-active)
      (scala-indent:indent-line)
    (save-excursion
      (let ((beg (min beg end))
            (end (max beg end)))
        (-map (lambda (line)
                (goto-line (- (+ (line-number-at-pos beg) line) 1))
                (scala-indent:indent-line))
              (number-sequence 1 (count-lines beg end))))))
  (setq deactivate-mark t))

(defun k/scala-add-font-lock ()
  (font-lock-add-keywords
   'scala-mode '(("`\".*\"'" 0 'font-lock-string-face t)
                 ("`.*'" 0 'font-lock-function-name-face t))))

(defun k/scala-mode-hook ()
  (my-coding-hook)
  (k/scala-add-font-lock)
  (auto-complete-mode -1)
  (flycheck-mode)
  (define-key scala-mode-map (kbd "<tab>") 'k/scala-indent-region))

(add-hook 'scala-mode-hook 'k/scala-mode-hook)

(defun k/scala-flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun k/scala-skip-sexp (val)
  (ignore-errors
    (while (or
            (equal (string (preceding-char)) val)
            (equal (format "%s" (preceding-sexp)) val))
      (backward-sexp))))

(defun k/scala-skip-line (val)
  (ignore-errors
    (while (or
            (equal (string (preceding-char)) val)
            (equal (format "%s" (preceding-sexp)) val))
      (backward-sexp)
      (beginning-of-line))))

(defun k/scala-check-package (pname)
  (when (equal (format "%s" (preceding-sexp)) "package")
    (k/scala-flash-region (line-beginning-position)
                           (line-end-position))
    (sbt-send-region (concat "import " pname "._"))
    t))

(defun k/scala-eval-string (s)
  (with-current-buffer (sbt:buffer-name)
    (goto-char (point-max))
    (if (string-match "\n" s)
        (if (eq system-type 'windows-nt)
            ;; Windows
            (progn
              (comint-send-string nil s)
              (let ((comint-input-sender 'ignore)
                    (comint-input-filter-functions nil))
                (comint-send-input t t))
              (comint-send-input)
              (goto-char (process-mark (get-buffer-process (current-buffer))))
              (line-move -1)
              (right-char (length "scala> "))
              (when (and (>= (point-max) (+ (point) 3))
                         (not (equal
                               (buffer-substring (point) (+ (point) 3))
                               "res")))
                (kill-line 1))
              (comint-kill-input))
          ;; Linux
          (progn
            (comint-send-string nil ":paste\n")
            (comint-send-string nil s)
            (comint-send-string nil "\n")
            (comint-send-string nil sbt:quit-paste-command)
            (sit-for 1)))
      (progn
        (comint-send-string nil s)
        (comint-send-string nil "\n")))))

(defun k/scala-eval-region (start end)
  "Send current region to Scala interpreter."
  (interactive "r")
  (let* ((reg (trim-string
               (buffer-substring-no-properties start end)))
         (package-pos (string-match "package" reg))
         ;; remove package ... line
         (reg (if (equal package-pos 0)
                  (let* ((package-name-end-pos
                          (or
                           (string-match "\n" reg package-pos)
                           (length reg)))
                         (package-name
                          (trim-string
                           (substring reg
                                      (+ package-pos (length "package"))
                                      package-name-end-pos))))
                    (concat
                     (format "import %s._ %s" package-name
                             (if (string-match "\n" reg ) "\n" ""))
                     (substring reg package-name-end-pos)))
                reg)))
    (k/scala-eval-string reg)))

(defun k/scala-get-last-scala-expr ()
  (let* ((prev-str (string (preceding-char)))
         (start (point))
         (end
          (save-excursion
            (backward-sexp 1)
            (cond ((equal "}" prev-str)
                   (ignore-errors (backward-sexp 1))
                   (beginning-of-line))
                  ((or
                    (equal ")" prev-str)
                    (equal "]" prev-str))
                   (progn
                     (if (not (= (current-column) 0))
                         (ignore-errors (backward-sexp 1)))
                     (k/scala-skip-sexp ".")
                     (k/scala-skip-sexp "new")
                     (k/scala-skip-line "=")
                     (k/scala-skip-line "case")
                     (k/scala-skip-line "class")))
                  (t
                   (progn
                     (k/scala-skip-sexp ".")
                     (k/scala-skip-sexp "import")
                     (when (k/scala-check-package
                            (buffer-substring start (point)))
                       (return-from k/scala-get-last-scala-expr)))))
            (point))))
    (list start end)))

(defun k/scala-eval-last-scala-expr ()
  (interactive)
  (cl-multiple-value-bind
      (start end)
      (k/scala-get-last-scala-expr)
    (k/scala-flash-region start end)
    (k/scala-eval-region start end)))

(defun k/scala-find-root (orig-fun &rest args)
  (setq-local sbt:buffer-project-root
              (projectile-project-root)))

(advice-add 'sbt:find-root
            :around
            #'k/scala-find-root)

(defun k/scala-start-console ()
  (interactive)
  ;; Use `test:console' instead of `console'
  ;; to access test resources.
  (sbt-command "test:console"))

(defun k/scala-switch-console ()
  (interactive)
  (switch-to-buffer-other-window (sbt:buffer-name)))

(defun k/scala-eval-buffer ()
  (interactive)
  (save-excursion
    (k/scala-flash-region (point-max) (point-min))
    (k/scala-eval-region (point-max) (point-min))))

(defun k/scala-eval-line ()
  (interactive)
  (k/scala-flash-region
      (line-beginning-position)
      (point))
     (k/scala-eval-region
      (line-beginning-position)
      (point)))

(defun k/scala-compile ()
  (interactive)
  (sbt-command "compile"))

(provide 'scala-conf)

;;; scala-conf.el ends here
