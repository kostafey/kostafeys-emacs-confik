;;; Usage:
;;
;; (global-set-key (kbd "C-M-d") 'hop-at-point)
;; (global-set-key (kbd "<C-down-mouse-1>") 'hop-by-mouse)

(defun hop-default-tag ()
  (let ((default (funcall (or find-tag-default-function
                              (get major-mode 'find-tag-default-function)
                              'find-tag-default))))    
    (if default
        (condition-case nil
            (find-tag default)
          (error (find-tag (car (last (split-string default "/")))))))))

(defun hop-at-point (point)
  "Jump to the entity definition."
  (interactive "d")
  (push-mark)
  (let ((mode (buffer-mode (current-buffer))))
    (cond
     ;; emacs-lisp-mode
     ((equal 'emacs-lisp-mode mode)
      (let ((symb (read (strip-text-properties 
                         (thing-at-point 'symbol)))))
        (when symb
          (cond
           ((or (functionp symb) (fboundp symb)) (find-function symb))
           (t (find-variable symb))))))
     ;; other modes
     (t
      (if (semantic-active-p)
          (condition-case nil
              (semantic-ia-fast-jump point)
            (error (hop-default-tag)))
        (hop-default-tag))))))

(defun hop-by-mouse (start-event)
  "Jump to the entity definition by mouse click."
  (interactive "e")
  (mouse-drag-region start-event)
  (hop-at-point (point)))

(provide 'hopper)
