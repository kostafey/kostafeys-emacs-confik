;;; shell-conf.el --- Eshell & shell related configuration.

(defcustom k/default-shell 'eshell
  "Set default shell type. Possible values are one of:
'eshell
'shell."
  :type 'symbol)

(defun k/shell (&optional num)
  (interactive "P")
  (let* ((current-dir (if buffer-file-name
                          (file-name-directory (buffer-file-name))))
         (shell-buffer-name (if num
                                (format "*%s %s*"
                                        (symbol-name k/default-shell) num)
                              (format "*%s*"
                                      (symbol-name k/default-shell))))
         (shell-bufer-exists-p (get-buffer shell-buffer-name)))
    (with-current-buffer
        (pcase k/default-shell
          ('eshell (flet ((pop-to-buffer-same-window
                           (b) (switch-to-buffer-other-window b)))
                     (eshell num)))
          ('shell (shell shell-buffer-name)))
      (goto-char (point-max))
      (when (and shell-bufer-exists-p
                 (not (equal default-directory current-dir)))
        (insert "cd ")
        (insert current-dir)
        (pcase k/default-shell
          ('eshell (eshell-send-input))
          ('shell (comint-send-input)))))))

;;------------------------------------------------------------
;; shell

(if (eq system-type 'windows-nt)
    ;; C-x C-f C-f /<user>@<host>:<path>
    (setq tramp-default-method "plink"))

;;------------------------------------------------------------
;; eshell

(require 'eshell-prompt-extras)

(defun k/git-branch ()
  "Return your git branch name."
  (let ((branch (car (vc-git-branches))))
    (cond
     ((null branch) nil)
     ((string-match "^(HEAD detached at \\(.+\\))$" branch)
      (concat epe-git-detached-HEAD-char (match-string 1 branch)))
     (t branch))))

(setq eshell-prompt-regexp "^λ "
      eshell-prompt-function
      (lambda ()
        (concat
         (propertize (eshell/pwd) 'face `(:foreground "#3063EA"))
         (if-let ((branch (k/git-branch)))
             (propertize (concat "\n" branch) 'face `(:foreground "#009292")))
         (propertize "\nλ" 'face `(:foreground "#5544EE" :weight bold))
         (propertize " " 'face `(:foreground "#326B6B" :weight normal)))))

(defun eshell-maybe-bol ()
  "Goto end of prompt or beginning of line."
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))

(defalias 'eshell/ff 'find-file)
(defalias 'eshell/fw 'find-file-other-window)

(defun eshell/cls ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/cd.. ()
  (eshell/cd ".."))

(defun k/eshell-handle-cd-args (args)
  "If `cd' argument is filepath, use it's directory path."
  (list (let ((path (car args)))
          (if (file-directory-p path)
              path
            (if (file-exists-p path)
                (file-name-directory path)
              path)))))

(advice-add 'eshell/cd :filter-args #'k/eshell-handle-cd-args)

(defun k/eshell-parse-backslash ()
  "Replace a single backslash (\\) character with slash (/).
Handle space before backslash ( \\) for multiline commands case."
  (when (eq (char-after) ?\\)
    (when (eshell-looking-at-backslash-return (point))
      (if (equal " " (string (char-before)))
	      (throw 'eshell-incomplete ?\\)
        ""))
    (forward-char 1)
    "/"))

(defun k/eshell-load-hook ()
  (add-hook 'eshell-parse-argument-hook 'k/eshell-parse-backslash))

(add-hook 'eshell-prompt-load-hook 'k/eshell-load-hook)

(defun k/eshell-mode-hook ()
  (define-key eshell-mode-map (kbd "<up>") 'k/line-previous)
  (define-key eshell-mode-map (kbd "<down>") 'k/line-next)
  (define-key eshell-mode-map (kbd "<home>") 'eshell-maybe-bol))

(add-hook 'eshell-mode-hook 'k/eshell-mode-hook)

(provide 'shell-conf)

;;; shell-conf.el ends here
