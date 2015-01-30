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
         ;; (_ (message "1"))
         (possible-goto-point-mov (save-excursion
                                    (condition-case nil
                                        (progn
                                          (if (equal direction :forward)
                                              (right-char 1)
                                            (left-char 1))
                                          (if (lua-is-keyword-here)
                                              (progn (lua-goto-matching-block)
                                                     (point))
                                            nil))
                                      (error nil))))
         ;; (_ (message "2"))
         (accept-goto (and possible-goto-point
                           (or (and (equal direction :forward)
                                    (> possible-goto-point (point)))
                               (and (equal direction :backward)
                                    (< possible-goto-point (point))))))
         ;; (_ (message (format "accept-goto: %d" 12)))
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
                              (lambda () (goto-char possible-goto-point-mov)))))
                      ((equal direction :backward)
                       (cond ((member next-str '(")" "}" "]"))
                              (lambda () (backward-sexp)))
                             ((and accept-goto
                                   (not (member next-str '(" " "	" "\n"))))
                              (lambda () (goto-char possible-goto-point)))
                             (accept-goto-mov
                              (lambda () (goto-char possible-goto-point-mov))))))))
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

(put 'lua-goto-forward-select 'CUA 'move)
(put 'lua-goto-backward-select 'CUA 'move)

(provide 'lua-conf)
