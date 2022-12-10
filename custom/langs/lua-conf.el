(require 'elpa-conf)
(use-elpa 'lua-mode)

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

(defun lua-goto-matching (direction &optional mark)
  (let* ((next-str (string (following-char)))
         (prev-str (string (preceding-char)))
         (possible-goto-point (save-excursion
                                (condition-case nil
                                    (if (lua-is-keyword-here)
                                        (progn
                                          (lua-goto-matching-block)
                                          (point))
                                      nil)
                                  (error nil))))
         (possible-goto-point-mov (save-excursion
                                    (if (lua-is-keyword-here)
                                        (or (save-excursion
                                              (right-char 1)
                                              (condition-case nil
                                                  (progn
                                                    (lua-goto-matching-block)
                                                    (point))
                                                (error nil)))
                                            (save-excursion
                                              (left-char 1)
                                              (condition-case nil
                                                  (progn
                                                    (lua-goto-matching-block)
                                                    (point))
                                                (error nil))))
                                      nil)))
         (accept-goto (and possible-goto-point
                           (or (and (equal direction :forward)
                                    (> possible-goto-point (point)))
                               (and (equal direction :backward)
                                    (< possible-goto-point (point))))))
         (accept-goto-mov (and possible-goto-point-mov
                               (or (and (equal direction :forward)
                                        (> possible-goto-point-mov (point)))
                                   (and (equal direction :backward)
                                        (< possible-goto-point-mov (point))))))
         (mover (cond ((equal direction :forward)
                       (cond ((member next-str '("(" "{" "["))
                              (lambda () (lua-forward-sexp)))
                             (accept-goto
                              (lambda () (goto-char possible-goto-point)))
                             (accept-goto-mov
                              (lambda () (goto-char possible-goto-point-mov)))
                             (t (lambda () (forward-sexp)))))
                      ((equal direction :backward)
                       (cond ((member prev-str '(")" "}" "]"))
                              (lambda () (backward-sexp)))
                             ((and accept-goto
                                   (not (member prev-str '(" " "	" "\n"))))
                              (lambda () (goto-char possible-goto-point)))
                             (accept-goto-mov
                              (lambda () (goto-char possible-goto-point-mov)))
                             (t (lambda () (backward-sexp))))))))
    (if mover (lua-mark-and-move mover mark))))

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

(defun lua-eval-last-expr ()
  (interactive)
  (save-excursion
    (let ((start (point))
          (end (progn
                 (lua-goto-backward)
                 (point))))
      (lua-send-region start end))))

(put 'lua-goto-forward-select 'CUA 'move)
(put 'lua-goto-backward-select 'CUA 'move)

(provide 'lua-conf)
