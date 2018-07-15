;;=============================================================================
;; mode-line (emacs status bar) config
;;
(column-number-mode t)
(setq display-time-format "%H:%M")
(display-time-mode t)

(when (require 'battery nil 'noerror)
  (progn
    (setq battery−mode−line−format " [%L %p%% %dC]")
    (when (and battery-status-function
       (not (string-match-p "N/A" 
                (battery-format "%B"
                        (funcall battery-status-function)))))
      (display-battery-mode 1))))

(setq-default mode-line-format 
  (list ""
        ;; file encoding
        'mode-line-mule-info
        '(:eval (propertize
                 (format"%s " buffer-file-coding-system)
                 ;; 'face 'mode-line-default-face                 
                 'help-echo (format"%s" buffer-file-coding-system)))        
                
        ;; the buffer name; the file name as a tool tip
        '(:eval (propertize (format-mode-line mode-line-buffer-identification)
                            ;; 'face 'mode-line-header
                            'help-echo (buffer-file-name)))

        ;; line and column
        " (" ;; '%02' to set to 2 chars at least; prevents flickering
        (propertize "%02l" 'face 'font-lock-string-face) ","
        (propertize "%02c" 'face 'font-lock-string-face) 
        ") "

        ;; relative position, size of file
        "("
        (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
        "/"
        (propertize "%I" 'face 'font-lock-constant-face) ;; size
        ")"
        
        '(vc-mode vc-mode)
        ;; 'mode-line-process
        " "
        ;; the current major mode for the buffer and list of minor modes.
        'mode-line-modes       

        "(" ;; insert vs overwrite mode, input-method in a tooltip
        '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                            'face 'font-lock-preprocessor-face
                            'help-echo (concat "Buffer is in "
                                               (if overwrite-mode "overwrite" "insert") " mode")))

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
        ") "

        ;; add the time, with the date and the emacs uptime in the tooltip
        ;; and system load average
        '(:eval (propertize (format-mode-line global-mode-string)
                            ;; 'help-echo (concat (format-time-string "%c; ")
                            ;;                    (emacs-uptime "Uptime:%hh"))
                            ))
        ))

(provide 'mode-line-conf)

