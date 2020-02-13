;;; navigation-in-frame.el --- Simplify navigation among buffers and windows.

;;-----------------------------------------------------------------------------
;; popup-switcher
(defun psw-org-mode-hook ()
  (if (equal major-mode 'org-mode)
      (outline-show-all)))
(add-hook 'psw-before-menu-hook 'psw-org-mode-hook)
(setq psw-use-flx t)
(setq psw-popup-position 'fill-column)

;;-----------------------------------------------------------------------------
;; dired
(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")
              (seq bol "." eol))))

(add-hook 'dired-mode-hook 'dired-omit-mode)

;; dired+
(when (require 'dired+ nil 'noerror)
  (toggle-diredp-find-file-reuse-dir t)

  (defun mydired-sort ()
    "Sort dired listings with directories first."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
      (set-buffer-modified-p nil)))

  (defadvice dired-readin
      (after dired-after-updating-hook first () activate)
    "Sort dired listings with directories first before adding marks."
    (mydired-sort)))

;;-----------------------------------------------------------------------------
;; ibuffer sorting
(setq-default ibuffer-default-sorting-mode 'major-mode)

;;-----------------------------------------------------------------------------
;; Kill current buffer
(defun prh:kill-current-buffer ()
    (interactive)
    (kill-buffer (current-buffer)))

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

;; Hide `tabbar' for buffers displayed in windows located
;; not in the top of the frame.
(defadvice select-window (after
                          k/select-window
                          activate)
  (if tabbar-mode
      (if (or (not (> (cadr (window-edges)) 0))
              (> (length (get-buffer-window-list)) 1))
          (tabbar-local-mode -1)
        (tabbar-local-mode 1))))

;;----------------------------------------------------------------------
;; flx configuration - fuzzy matching files and paths via ido
;;
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;;----------------------------------------------------------------------

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
   (not (eq system-type 'windows-nt))
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

(if (eq system-type 'windows-nt)
    ;; C-x C-f C-f /<user>@<host>:<path>
    (setq tramp-default-method "plink"))

(defun k/shell (&optional num)
  (interactive "P")
  (let* ((current-dir (if buffer-file-name
                          (file-name-directory (buffer-file-name))))
         (shell-buffer-name (if num (format "*shell %s*" num) "*shell*"))
         (shell-bufer-exists-p (get-buffer shell-buffer-name)))
    (with-current-buffer (shell shell-buffer-name)
      (goto-char (point-max))
      (when (and shell-bufer-exists-p
                 (not (equal default-directory current-dir)))
        (insert "cd ")
        (insert current-dir)
        (comint-send-input)))))

(provide 'navigation-in-frame)

;;; navigation-in-frame.el ends here
