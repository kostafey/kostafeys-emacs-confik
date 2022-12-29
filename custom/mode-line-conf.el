(require 'elpa-conf)
(use-elpa 'use-package)

;;=============================================================================
;; mode-line (emacs status bar) config
;;
(use-package minions
  :ensure t
  :config (minions-mode 1))

(setq-default
 projectile-mode-line-prefix " "
 mode-line-format
 (list ""
       ;; ------------------------------------------------------------
       ;; file encoding
       'mode-line-mule-info
       ;; '(:eval (propertize
       ;;          (format "%s " buffer-file-coding-system)
       ;;          'help-echo (format"%s" buffer-file-coding-system)))
       ;; ------------------------------------------------------------
       ;; the `buffer-name'; the file name as a tool tip
       '(:eval (propertize (buffer-name)
                           'face '(:weight bold)
                           'mouse-face 'mode-line-highlight
                           'help-echo (buffer-file-name)
                           'local-map (let ((map (make-sparse-keymap)))
                                        (define-key map [mode-line mouse-1]
                                          (lambda (e)
                                            (interactive "e")
                                            (mode-line-previous-buffer e)))
                                        (define-key map [mode-line mouse-3]
                                          (lambda (e)
                                            (interactive "e")
                                            (mode-line-next-buffer e)))
                                        map)))
       ;; ------------------------------------------------------------
       ;; line and column
       " (" ;; '%02' to set to 2 chars at least; prevents flickering
       (propertize "%02l" 'face 'font-lock-string-face) ","
       (propertize "%02c" 'face 'font-lock-string-face)
       ")"
       ;; ------------------------------------------------------------
       ;; csv field index for `csv-mode'
       '(:eval (when (and (eq major-mode 'csv-mode) csv-field-index-mode)
                 (propertize
                  (format " %s" (k/csv-get-field-index))
                  'face 'escape-glyph)))
       ;; ------------------------------------------------------------
       ;; the current `major-mode' for the buffer and list of minor modes.
       '(:eval (propertize " %m"
                           ;; 'face '((t :foreground "black"))
                           'help-echo (mapconcat
                                       'identity
                                       (-sort
                                        'string-lessp
                                        (-map 'symbol-name
                                              (--filter
                                               (and (boundp it)
                                                    (symbol-value it))
                                               minor-mode-list)))
                                       "\n")
                           'mouse-face 'mode-line-highlight
                           'local-map (let ((map (make-sparse-keymap)))
                                        (define-key map [mode-line mouse-1]
                                          'describe-mode)
                                        (define-key map [mode-line mouse-3]
                                          'minions-minor-modes-menu)
                                        map)))
       ;; ------------------------------------------------------------
       ;; version control data
       '(:eval (propertize (if (vc-mode-line buffer-file-name)
                               vc-mode
                             "")
                           'face 'font-lock-constant-face))
       ;; ------------------------------------------------------------
       ;; `projectile'
       '(:eval (propertize (if (projectile-project-root)
                               (projectile-default-mode-line)
                             "")
                           'mouse-face 'mode-line-highlight
                           'local-map (make-mode-line-mouse-map
                                       'mouse-1 'projectile-mode-menu)))
       ;; ------------------------------------------------------------
       ;; read only, insert/overwrite, edited signs
       " (" ;; insert vs overwrite mode, input-method in a tooltip
       '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                           'face 'font-lock-preprocessor-face
                           'help-echo (concat "Buffer is in "
                                              (if overwrite-mode
                                                  "overwrite"
                                                "insert") " mode")))
       ;; was this buffer modified since the last save?
       '(:eval (when (buffer-modified-p)
                 (concat ","  (propertize "*"
                                          'face 'font-lock-string-face
                                          'help-echo "Buffer has been modified"))))
       ;; is this buffer read-only?
       '(:eval (when buffer-read-only
                 (concat ","  (propertize "RO"
                                          'face 'font-lock-string-face
                                          'help-echo "Buffer is read-only"))))
       ")"
       ))

(provide 'mode-line-conf)
