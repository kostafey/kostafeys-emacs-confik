;;; shell-conf.el --- Shell related configuration: the common parts.  -*- lexical-binding: t -*-

(use-package exec-path-from-shell
  :straight '(exec-path-from-shell :type git :host github
			                             :repo "purcell/exec-path-from-shell"
                                   :branch "master")
  :config (when (eq system-type 'gnu/linux)
              (exec-path-from-shell-initialize)))

(straight-use-package
 '(exec-path-from-shell :type git :host github
			                  :repo "purcell/exec-path-from-shell" :branch "master"))

(defcustom k/default-shell 'eshell
  "The shell `k/shell' opens."
  :type '(choice (const eshell) (const shell))
  :group 'eshell)

(defvar eshell-foreground-command)
(defvar eshell-last-output-end)
(declare-function eshell-quote-argument "esh-arg" (string))
(declare-function eshell-send-input "esh-mode"
                  (&optional use-region queue-p no-newline))
(declare-function comint-send-input "comint" (&optional no-newline artificial))
(declare-function eframe-pop-buffer "eframe-nav" (mode))

(defun k/shell-cd (dir)
  "Change the directory of the shell in the current buffer to DIR.
In eshell the text typed at the prompt is put back after the cd."
  (goto-char (point-max))
  (if (derived-mode-p 'eshell-mode)
      (let ((input (buffer-substring-no-properties
                    eshell-last-output-end (point-max))))
        (delete-region eshell-last-output-end (point-max))
        (insert "cd " (eshell-quote-argument dir))
        (eshell-send-input)
        (insert input))
    (delete-region (process-mark (get-buffer-process (current-buffer)))
                   (point-max))
    (insert "cd " (shell-quote-argument (file-local-name dir)))
    (comint-send-input)))

(defun k/shell (&optional arg)
  "Pop to a `k/default-shell' and cd it to the directory of this buffer.
Without ARG reuse a shell that exists, with a number ARG the session of
that number, with \\[universal-argument] a new one.  A shell opens in
another window.  A shell running a command keeps its directory."
  (interactive "P")
  (let ((dir default-directory)
        (mode (if (eq k/default-shell 'eshell) 'eshell-mode 'shell-mode)))
    (if (derived-mode-p mode)
        (message "Already in %s" k/default-shell)
      (with-current-buffer
          (or (and (not arg) (eframe-pop-buffer mode))
              (let ((display-buffer-overriding-action
                     '(nil (inhibit-same-window . t))))
                (if (eq k/default-shell 'eshell)
                    (eshell arg)
                  (shell (if (numberp arg)
                             (format "*shell*<%d>" arg)
                           (and arg (generate-new-buffer-name "*shell*")))))))
        (goto-char (point-max))
        (unless (or (file-equal-p default-directory dir)
                    (bound-and-true-p eshell-foreground-command))
          (k/shell-cd dir))))))

(global-set-key (kbd "<f4>") 'k/shell)

;;------------------------------------------------------------
;; shell

(if (eq system-type 'windows-nt)
    ;; C-x C-f C-f /<user>@<host>:<path>
    (setq tramp-default-method "plink"))

(setq w32-quote-process-args t)

;; Windows shell (cmd) correct encoding
(defun k/shell-cp1251 (buffer)
  "Make the process of the shell BUFFER talk cp1251; return BUFFER."
  (set-process-coding-system (get-buffer-process buffer) 'cp1251 'cp1251)
  buffer)

(when (eq system-type 'windows-nt)
  (advice-add 'shell :filter-return #'k/shell-cp1251))

;;------------------------------------------------------------
;; Shells and terminals

(require 'eshell-conf)
(require 'vterm-conf)
(require 'ghostel-conf)

(provide 'shell-conf)

;;; shell-conf.el ends here
