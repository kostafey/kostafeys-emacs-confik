;;; hopper.el --- simplify code navigation.

;;; Commentary:

;;
;; (global-set-key (kbd "C-M-d") 'hop-at-point)
;; (global-set-key (kbd "<C-down-mouse-1>") 'hop-by-mouse)

;;; Code:

(require 's)
(require 'ejc-sql)
(require 'clojure-conf)

(defun hop-buffer-mode (buffer-or-string)
  "Return the major mode associated with a buffer BUFFER-OR-STRING."
  (with-current-buffer buffer-or-string
    (if (and (equal 'sql-mode major-mode)
             (member 'ejc-sql-mode minor-mode-list))
        'ejc-sql-mode
      major-mode)))

(defun hop-strip-text-properties (txt)
  (set-text-properties 0 (length txt) nil txt)
      txt)

(defun hop-default-tag ()
  (let ((default (funcall (or find-tag-default-function
                              (get major-mode 'find-tag-default-function)
                              'find-tag-default))))
    (if default
        (condition-case nil
            (xref-find-definitions default)
          (error (xref-find-definitions (car (last (split-string default "/")))))))))

(defvar hop-positions (list))
(defvar hop-positions-max-length 20)
(defvar hop-current-pos 0)

(defun hop-clear ()
  (interactive)
  (setq hop-positions (list))
  (setq hop-current-pos 0))

(defun hop-update-positions (buffer point reason)
  (let ((new-pos (list buffer point reason)))
    (when (eq reason :hop)
      (setq hop-current-pos 0)
      (setq hop-positions (cons new-pos hop-positions)))
    (when (eq reason :back)
      (if (eq (nth 2 (nth hop-current-pos hop-positions)) :back)
          (setcar (nthcdr hop-current-pos hop-positions) new-pos)
        (if (eq hop-current-pos 0)
         (setq hop-positions (cons new-pos hop-positions)))))
    (if (> (length hop-positions) hop-positions-max-length)
        (delete (car (last hop-positions)) hop-positions))))

(defun hop-in-list ()
  (switch-to-buffer (car (nth hop-current-pos hop-positions)))
  (goto-char (cadr (nth hop-current-pos hop-positions))))

(defun hop-backward ()
  (interactive)
  (hop-update-positions (current-buffer) (point) :back)
  (cl-incf hop-current-pos)
  (hop-in-list))

(defun hop-forward ()
  (interactive)
  (when (> hop-current-pos 0)
    (cl-decf hop-current-pos)
    (hop-in-list)))

(defvar hop-url-regexp
  (concat "\\s-*\\(\\w\\|\\+\\|-\\)+://\\(\\w\\|\\-\\)+\\(\\.\\w+\\)?"
          "\\(\\/\\(%[0-9a-fA-F]\\{2\\}\\|[~\\.A-Za-z_+-]*\\)*\\)*"))

(require 'cider nil 'noerror)

(defun hop--goto-file-location (arg)
  (let ((splitted-path (s-split ":" arg)))
    (cl-multiple-value-bind (path line)
        (if (equal (length (car splitted-path)) 1)
            (list (concat (car splitted-path) ":" (cadr splitted-path))
                  (string-to-number (nth 2 splitted-path)))
          (list (car splitted-path) (string-to-number (nth 1 splitted-path))))
      (find-file path)
      (forward-line line))))

(defun hop-goto-file-location (arg)
  (interactive "p")
  (hop--goto-file-location
   (if mark-active
       (buffer-substring (region-beginning) (region-end))
     (let* ((str-under-point (s-trim
                              (save-excursion
                                (buffer-substring
                                 (search-backward " " (line-beginning-position))
                                 (line-end-position)))))
            (entered-str (if (not (equal arg -1))
                             str-under-point
                           (read-string "Go to file location: "
                                        str-under-point))))
       entered-str))))

(defun hop-url-in-markdown (pos)
  "Since markdown URLs looks like: [link](http://link.com)"
  (let* ((beg (1+ (save-excursion
                    (search-backward "(" (line-beginning-position)))))
         (end (1- (save-excursion
                    (search-forward ")" (line-end-position)))))
         (browse-url (buffer-substring beg end)))
    (if (string-match hop-url-regexp browse-url)
        (browse-url browse-url))))

(defun hop-at-point (point)
  "Jump to the entity definition at POINT position."
  (interactive "d")
  (if mark-active
      (browse-url (buffer-substring (region-beginning) (region-end)))
    (progn
      (let ((mode (hop-buffer-mode (current-buffer)))
            (string-at-point (hop-strip-text-properties
                              (thing-at-point 'symbol)))
            (url (hop-strip-text-properties
                  (thing-at-point 'url))))
        (if (and url (string-match hop-url-regexp url))
            (browse-url url)
          (progn
            (push-mark)
            (hop-update-positions (current-buffer) (point) :hop)
            (cond
             ;; emacs-lisp-mode
             ((equal 'emacs-lisp-mode mode)
              (let ((symb (read string-at-point)))
                (when symb
                  (cond
                   ((or (functionp symb) (fboundp symb)) (find-function symb))
                   (t (find-variable symb))))))
             ;; lisp-mode
             ((and (equal 'lisp-mode mode)
                   (functionp 'slime-edit-definition))
              (let ((symb string-at-point))
                (slime-edit-definition symb)))
             ;; clojure-mode
             ((member mode (list 'clojure-mode 'clojurescript-mode))
              (if (k/nrepl-current-session)
                  (let ((xref-prompt-for-identifier nil))
                    (call-interactively 'xref-find-definitions))
                (if (member 'lsp-mode minor-mode-list)
                    (lsp-find-definition)
                  (message "Launch LSP or nREPL."))))
             ;; scala-mode
             ;; java-mode
             ((or (equal 'scala-mode mode)
                  (equal 'java-mode mode))
              (progn
                (lsp-find-definition)
                (recenter-top-bottom 5)))
             ;; go-mode
             ((equal 'go-mode mode)
              (if (member 'lsp-mode minor-mode-list)
                  (lsp-find-definition)
                (when (functionp 'godef-jump)
                  (godef-jump point))))
             ;; rust-mode
             ((and (equal 'rust-mode mode)
                   (functionp 'racer-find-definition))
              (racer-find-definition))
             ;; shell mode assume line looks like [ERROR] ~/project/MyClass.java:123:
             ((equal 'shell-mode mode) (hop-goto-file-location point))
             ;; markdown-mode
             ((equal 'markdown-mode mode) (hop-url-in-markdown point))
             ;; ejc-sql-mode
             ((equal 'ejc-sql-mode mode)
              (apply 'ejc-describe-entity
                     (ejc-get-prompt-symbol-under-point "Describe entity")))
             ;; other modes
             (t
              (if (and (functionp 'semantic-ia-fast-jump)
                       (functionp 'semantic-active-p)
                       (semantic-active-p))
                  (condition-case nil
                      (semantic-ia-fast-jump point)
                    (error (hop-default-tag)))
                (hop-default-tag))))))))))

(defun hop-by-mouse (start-event)
  "Jump to the entity definition by mouse click."
  (interactive "e")
  ;; (mouse-drag-region start-event)
  (hop-at-point (point)))

(provide 'hopper)

;;; hopper.el ends here
