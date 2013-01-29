;; The approach to copy/paste text to/from emacs depending on environment.
;; Resources:
;; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
;; https://gist.github.com/3842934

;; This works unstable with klipper (KDE)
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(defvar copy-paste-engine nil
  "Possibile values:
`nil' - default
`xsel' - xsel
`kde' - kde
`gnome' - GTK-based, like gnome or cinnamon.")

(when (not copy-paste-engine)
  (if (eq 'gnu/linux system-type)
      (progn
        ;; In case of Linux
        (if (not window-system)
            ;; If emacs is run in a terminal, the clipboard- functions have no
            ;; effect. Instead, we use of xsel, see
            ;; http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line
            ;; program for getting and setting the contents of the X selection"            
            (setq copy-paste-engine "xsel")
          ;; for window-system
          (if (string-equal "kde" (getenv "DESKTOP_SESSION"))
              (setq copy-paste-engine "kde")
            (setq copy-paste-engine "gnome"))))
    ;;  In case of Windows do nothing.
    ))

(when (executable-find "xsel") ;; xsel program is found
  ;; Callback for when user cuts
  (defun xsel-cut-function (text &optional push)
    ;; Insert text to temp-buffer, and "send" content to xsel stdin
    (with-temp-buffer
      (insert text)
      ;; I prefer using the "clipboard" selection (the one the
      ;; typically is used by c-c/c-v) before the primary selection
      ;; (that uses mouse-select/middle-button-click)
      (call-process-region (point-min) (point-max) 
                           "xsel" nil 0 nil "--clipboard" "--input")))
  ;; Call back for when user pastes
  (defun xsel-paste-function()
    ;; Find out what is current selection by xsel. If it is different
    ;; from the top of the kill-ring (car kill-ring), then return
    ;; it. Else, nil is returned, so whatever is in the top of the
    ;; kill-ring will be used.
    (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
      (unless (string= (car kill-ring) xsel-output)
        xsel-output ))))

(when (eq 'gnu/linux system-type)  
  (defun copy-from-kde()
    (let ((output (shell-command-to-string 
                   "qdbus org.kde.klipper /klipper getClipboardContents")))
      (unless (string= (car kill-ring) output)
        output )))
  (defun paste-to-kde (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process 
                   "proc" "*Messages*" "qdbus" "org.kde.klipper" 
                   "/klipper" "setClipboardContents" text)))))))

(cond ((equal copy-paste-engine "xsel")
       (progn
         (setq x-select-enable-clipboard t)
         (setq interprogram-cut-function 'xsel-cut-function)
         (setq interprogram-paste-function 'xsel-paste-function)))
      ((equal copy-paste-engine "kde")
       (progn
         (setq x-select-enable-clipboard t)
         (setq interprogram-cut-function 'paste-to-kde)
         ;; (setq interprogram-paste-function 'copy-from-kde)
         (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)))
      ((equal copy-paste-engine "gnome")
       (progn
         (setq x-select-enable-clipboard t)
         (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))))

(provide 'copy-paste-clipboard-linux)

