;;-----------------------------------------------------------------------------
;; popup-switcher
(defun psw-org-mode-hook ()
  (if (equal major-mode 'org-mode)
      (outline-show-all)))
(add-hook 'psw-before-menu-hook 'psw-org-mode-hook)
(setq psw-use-flx t)

;;-----------------------------------------------------------------------------
;; ibuffer sorting
(setq-default ibuffer-default-sorting-mode 'major-mode)

;;-----------------------------------------------------------------------------
;; Here's a handy function that kills the current buffer and removes
;; the file it is connected to.
(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;;-----------------------------------------------------------------------------
;Убить буфер
(defun prh:kill-current-buffer ()
    (interactive)
    (kill-buffer (current-buffer)))

;;=============================================================================
;; Переключения буферов
;; Buffers changing
;;
(defvar my-force-switch nil)

(defun my-nrepl-messages-buffer-p ()
  (or (equal (buffer-name) "*touch*")
      (and (>= (length (buffer-name)) 15)
           (equal "*nrepl-messages"
                  (substring-no-properties (buffer-name) 0 15)))))

(defun my-next-buffer ()
  (interactive)
  (setq my-force-switch t)
  (next-buffer)
  (setq my-force-switch nil)
  (if (my-nrepl-messages-buffer-p)
      (next-buffer)))

(defun my-previous-buffer ()
  (interactive)
  (setq my-force-switch t)
  (previous-buffer)
  (setq my-force-switch nil)
  (if (my-nrepl-messages-buffer-p)
      (previous-buffer)))

;;-----------------------------------------------------------------------------
;; Tabbar
(require 'tabbar)

;(set-face-foreground 'tabbar-default "LightSteelBlue")
;(set-face-background 'tabbar-default "DarkSlateGray")
;(set-face-foreground 'tabbar-selected "pale green")

(set-face-bold 'tabbar-selected t)
(set-face-attribute 'tabbar-button nil :box '(:line-width 1 :color "gray72"))

(setq tabbar-buffer-groups-function
      '(lambda ()
         (list
          (cond
           ((find (aref (buffer-name (current-buffer)) 0) " *") "*")
           (t "All Buffers")))))

(tabbar-mode t)

;;=============================================================================

;;; save minibuffer history between sessions
(when (> emacs-major-version 21) (savehist-mode t))
;;=============================================================================

;;----------------------------------------------------------------------
;; flx configuration - fuzzy matching files and paths via ido
;;
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
;;
;;----------------------------------------------------------------------

(defun copy-to-clipboard-buffer-file-path ()
  (interactive)
  "Copy current file path to the clipboard."
  (let ((result (kill-new (buffer-file-name))))
    (message result)
    result))

(defun copy-to-clipboard-buffer-file-name ()
  (interactive)
  "Copy current file name to the clipboard."
  (let ((result (kill-new (file-name-nondirectory (buffer-file-name)))))
    (message result)
    result))

(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))

(defun find-browser-executable ()
  (cond ((executable-find "palemoon") "palemoon")
        ((executable-find "google-chrome-stable") "google-chrome-stable")
        ((executable-find "chromium") "chromium")
        ((executable-find "chromium-browser") "chromium-browser")
        ((executable-find "firefox") "firefox")
        (t (message "Cant find any browser in the PATH."))))

(if (eq system-type 'windows-nt)
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program
          "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program (find-browser-executable)))

(defvar ipv6
  (and
   (featurep 'make-network-process '(:family ipv6))
   (not (equal
         (car (split-string
               (shell-command-to-string "ping6 ipv6.google.com") "\n"))
         "connect: Network is unreachable"))))

(defun google (&optional arg)
  "Google the selected region if any, display a query prompt otherwise."
  (interactive "p")
  (let ((e1 (equal arg 1)))
    (browse-url
     (concat
      (format "http://%s.google.com/search?ie=utf-8&oe=utf-8&q="
              (if ipv6 "ipv6" "www"))
      (url-hexify-string
       (if mark-active
           (buffer-substring (region-beginning) (region-end))
         (read-string "Google: "
                      (if (and e1 (symbol-at-point))
                          (symbol-name (symbol-at-point))))))))))

(defun goto-url (&optional arg)
  "Go to selected region as URL if any, display a query prompt otherwise."
  (interactive "p")
  (browse-url
   (if mark-active
       (buffer-substring (region-beginning) (region-end))
     (let ((entered-str
            (read-string "Go to URL: "
                         (if (not (equal arg 1))
                             (symbol-name (symbol-at-point))))))
       (if (not (equal "http" (substring entered-str 0 4)))
           (concat "http://" entered-str)
         entered-str)))))

(defun find-file-from-clipboard ()
  "Open file or directory path from clipboard (kill ring) if path exists."
  (interactive)
  (let ((file-path (current-kill 0)))
    (if (file-exists-p file-path)
        (find-file file-path)
      (message "Can't find file '%s'" file-path))))

;;----------------------------------------------------------------------
;; windmove
;; Handle 2 monitors case
;;
(defun k/windmove-do-window-select (orig-fun &rest args)
  (let ((other-window (apply 'windmove-find-other-window args)))
    (if (and (null other-window)
             (> (length (frame-list)) 1))
        (other-frame 1)
      (apply orig-fun args))))

(advice-add 'windmove-do-window-select
            :around #'k/windmove-do-window-select)

(provide 'buffer-navigation)
