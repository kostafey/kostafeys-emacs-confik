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
;;   org.scalameta:metals_2.12:0.7.6 \
;;   -r bintray:scalacenter/releases \
;;   -r sonatype:snapshots \
;;   -o /usr/local/bin/metals-emacs -f
;;
;; `Windows':
;; set BIN_PATH=C:\bin\
;; curl -L -o "%BIN_PATH%coursier" https://git.io/coursier-cli
;; curl -L -o "%BIN_PATH%coursier.bat" https://git.io/coursier-bat
;; coursier bootstrap ^
;;   --java-opt -Xss4m ^
;;   --java-opt -Xms100m ^
;;   --java-opt -Dmetals.client=emacs ^
;;   org.scalameta:metals_2.12:0.7.6 ^
;;   -r bintray:scalacenter/releases ^
;;   -r sonatype:snapshots ^
;;   -o %BIN_PATH%metals-emacs -f

;; Run for new projects:
;; M-x `lsp-metals-build-import'

;;; Code:

(require 'use-package)

;; Enable defer and ensure by default for use-package
;; Keep auto-save/backup files separate from source code:  https://github.com/scalameta/metals/issues/1027
(setq use-package-always-defer t
      use-package-always-ensure t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Enable scala-mode and sbt-mode
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck)

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook (scala-mode . lsp)
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-ui)

(defun k/scala-mode-hook ()
  (my-coding-hook)
  (auto-complete-mode -1))

(add-hook 'scala-mode-hook 'k/scala-mode-hook)

(setq scala-indent:step 4)

;; Add company-lsp backend for metals
(use-package company-lsp)

(provide 'scala-conf)

;;; scala-conf.el ends here
