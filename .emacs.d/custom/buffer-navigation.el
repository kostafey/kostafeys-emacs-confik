;;=============================================================================

(defvar change-buffer-prefix "\C-c\C-b")

(global-set-key (concat selected-area-prefix "\C-e") 
                '(lambda () (interactive) (find-file "~/.emacs")))
(global-set-key (concat change-buffer-prefix "e") 
                '(lambda () (interactive) (find-file "~/.emacs")))

(global-set-key "\C-x\C-c" 
                '(lambda () (interactive) 
                   (progn
                     (switch-to-buffer "temp") (linum-mode t)
                     (flyspell-russian)
                     (auto-fill-mode t)
                     (setq auto-complete-mode t))))
(global-set-key (concat change-buffer-prefix "t") 
                '(lambda () (interactive) (switch-to-buffer "temp") (linum-mode t)))

(global-set-key (concat change-buffer-prefix "p") 
                '(lambda () (interactive) (find-file "~/.org.gpg")))
(global-set-key (concat change-buffer-prefix "k") 
                '(lambda () (interactive) (find-file "~/.keys.org")))

(global-set-key (concat change-buffer-prefix "b") 'switch-to-buffer)

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
(global-set-key (kbd "C-c k") 'delete-this-buffer-and-file)

;;=============================================================================
;; Переключения буферов
;; Buffers changing
;;=============================================================================
;;-----------------------------------------------------------------------------
;; Tabbar
(require 'tabbar)

(global-set-key [S-tab] 'tabbar-forward-tab)
(global-set-key [C-S-tab] 'tabbar-backward-tab)

(global-set-key [(meta shift left)] 'tabbar-backward-tab)
(global-set-key [(meta shift right)] 'tabbar-forward-tab)

;(global-set-key [(meta ctrl left)] 'tabbar-backward-tab)
;(global-set-key [(meta ctrl right)] 'tabbar-forward-tab)

;(set-face-foreground 'tabbar-default "LightSteelBlue")
;(set-face-background 'tabbar-default "DarkSlateGray")
;(set-face-foreground 'tabbar-selected "pale green")

(set-face-bold-p 'tabbar-selected t)
(set-face-attribute 'tabbar-button nil :box '(:line-width 1 :color "gray72"))

(setq tabbar-buffer-groups-function
      (lambda () 
        (list
         (cond
          ((find (aref (buffer-name (current-buffer)) 0) " *") "*")
          (t "All Buffers"))
         )))

(tabbar-mode t)
;;-----------------------------------------------------------------------------
;; ido
(require 'ido)
(ido-mode t)
(global-set-key "\C-x\C-f" 'ido-find-file)
(global-set-key "\C-x\b" 'ido-switch-buffer)
;;-----------------------------------------------------------------------------
;; ibuffer - еще один способ переключения между буферами
(global-set-key "\C-x\C-b" 'ibuffer)
;;-----------------------------------------------------------------------------
(require 'bs)
(global-set-key "\C-x\C-n" 'bs-show)
;;-----------------------------------------------------------------------------
;; lusty-explorer
(when (require 'lusty-explorer nil 'noerror)
  (global-set-key (kbd "C-c C-f") 'lusty-file-explorer)
  (global-set-key (kbd "C-c C-x")   'lusty-buffer-explorer))
;;-----------------------------------------------------------------------------
;Убить буфер
(defun prh:kill-current-buffer ()
    (interactive)
    (kill-buffer (current-buffer)))
(global-set-key "\C-w" 'prh:kill-current-buffer)
(global-set-key (kbd "C-x w") 'kill-buffer)
;;=============================================================================

;;; save minibuffer history between sessions
(when (> emacs-major-version 21) (savehist-mode t))
;;=============================================================================

(provide 'buffer-navigation)

