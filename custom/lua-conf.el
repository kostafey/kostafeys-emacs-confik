(require 'lua-mode)

(defvar lua-keywords
  '("and" "break" "do" "else" "elseif" "end"  "for" "function"
    "goto" "if" "in" "local" "not" "or" "repeat" "return"
    "then" "until" "while"))

(defun lua-is-keyword-here ()
  (member (symbol-name (symbol-at-point)) lua-keywords))

(defun lua-mark-and-move (mover mark)
  (interactive)
  (if (and mark (not mark-active))
      (cua-set-mark))
  (funcall mover))



(lua-mark-and-move (lambda () (left-char 10)) t)

(left-char 10)

;; (set-mark 3)

;; (progn
;;   (let ((target-pos (save-excursion
;;                       (lua-goto-matching-block)
;;                       (point))))
;;     (cua-set-mark)
;;     (goto-char target-pos)))

;; (progn
;;   (cua-set-mark)
;;   (goto-char 30))


(defun lua-goto-matching (direction &optional mark)
  (let* ((next-str (string (following-char)))
         (prev-str (string (preceding-char)))
         (possible-goto-point (save-excursion
                                (lua-goto-matching-block)
                                (point)))
         (accept-goto (or (and (equal direction :forward)
                               (> possible-goto-point (point)))
                          (and (equal direction :backward)
                               (< possible-goto-point (point)))))
         (mover (cond ((equal direction :forward)
                       (if (member next-str '("(" "{" "["))
                           (lambda () (lua-forward-sexp))
                         (if (and accept-goto (lua-is-keyword-here))
                             (lambda () (lua-goto-matching-block))
                           (lambda () (lua-forward-sexp)))))
                      ((equal direction :backward)
                       (if (member prev-str '(")" "}" "]"))
                           (lambda () (backward-sexp))
                         (if (and (lua-is-keyword-here)
                                  (not (member next-str '(" " "	" "\n"))))
                             (lambda () (lua-goto-matching-block))
                           (if (save-excursion
                                 (left-char 1)
                                 (lua-is-keyword-here))
                               (lambda ()
                                 (if (and mark (not mark-active))
                                     (cua-set-mark))
                                 (left-char 1)
                                 (lua-goto-matching-block))
                             (lambda () (backward-sexp)))))))))
    (lua-mark-and-move mover mark)))

(defun lua-goto-forward ()
  (interactive)
  (lua-goto-matching :forward))

(defun lua-goto-backward ()
  (interactive)
  (lua-goto-matching :backward))

(defun lua-goto-forward-select ()
  (interactive)
  (lua-goto-matching :forward :mark))

(defun lua-goto-backward-select ()
  (interactive)
  (lua-goto-matching :backward :mark))

(put 'lua-goto-forward-select 'CUA 'move)
(put 'lua-goto-backward-select 'CUA 'move)

(provide 'lua-conf)
