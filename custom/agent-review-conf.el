;;; agent-review-conf.el --- Agent diff review -*- lexical-binding: t -*-

;; ;  comment the hunk or region at point, in magit and in pijul changes
;; @  send the repository's comments to its running agent

(require 'agent-review)

(defvar pijul-commit-repository)
(defvar pijul-commit-mode-map)
(defvar magit-mode-map)
(declare-function k/pijul-commit--hunk-body "pijul-conf" ())
(declare-function k/pijul-commit--hunk-location "pijul-conf" ())

(add-to-list 'agent-review-agents
             '("Claude" k/claude-code-ide-running-p
               claude-code-ide-send-prompt))

;;; Pijul changes, as `pijul-conf' parses them

(defun k/agent-review-pijul-root ()
  "Return the repository of a `pijul-commit-mode' buffer."
  (or pijul-commit-repository default-directory))

(defun k/agent-review-pijul-hunk (pos)
  "Return (START . END) of the pijul change at POS, or nil."
  (save-excursion
    (goto-char pos)
    (k/pijul-commit--hunk-body)))

(defun k/agent-review-pijul-location (pos)
  "Return (FILE LINE) of the pijul change line at POS.
Some changes, such as a file deletion, name no file line to locate."
  (save-excursion
    (goto-char pos)
    (seq-take (ignore-errors (k/pijul-commit--hunk-location)) 2)))

(add-to-list 'agent-review-backends
             '(pijul-commit-mode :root k/agent-review-pijul-root
                                 :hunk k/agent-review-pijul-hunk
                                 :location k/agent-review-pijul-location))

;; `pijul-commit-mode' starts before `pijul-record-preview' sets the
;; repository of its buffer.
(add-hook 'pijul-commit-mode-hook #'agent-review-show-comments-later)

;;; Key bindings

(with-eval-after-load 'magit-mode
  (define-key magit-mode-map (kbd ";") #'agent-review-comment)
  (define-key magit-mode-map (kbd "@") #'agent-review-send))

;; As the other keys of `pijul-commit-mode' in `pijul-conf': only in
;; read-only buffers, so that they self-insert while `pijul record'
;; edits a change text.
(with-eval-after-load 'pijul-commit
  (define-key pijul-commit-mode-map (kbd ";")
    '(menu-item "" agent-review-comment
                :filter (lambda (cmd) (and buffer-read-only cmd))))
  (define-key pijul-commit-mode-map (kbd "@")
    '(menu-item "" agent-review-send
                :filter (lambda (cmd) (and buffer-read-only cmd)))))

(provide 'agent-review-conf)
;;; agent-review-conf.el ends here
