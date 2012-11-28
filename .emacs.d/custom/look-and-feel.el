(require 'elpa-conf)

;;-----------------------------------------------------------------------------
;; Font
(set-face-attribute 'default nil :family "Consolas" :height 110)

;;=============================================================================
;; Change font size
;;=============================================================================
(defun djcb-zoom (n)
  "with positive N, increase the font size, otherwise decrease it"
  (set-face-attribute 'default (selected-frame) :height 
    (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 10))))

;;=============================================================================
;; Font decorations
;;=============================================================================
(setq font-lock-maximum-decoration t)
(global-font-lock-mode 1)

; Maximum size of a buffer for buffer fontification.
(if (< emacs-major-version 24)
    (setq font-lock-maximum-size 2560000))

(require 'font-lock)
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode t))
(make-face 'trailing-spaces-face "Face to display trailing spaces in.")
(add-hook 'font-lock-mode-hook     ; Show trailing spaces and make fixme tags standout
          (lambda ()
            (font-lock-add-keywords nil
             '(("[ \t]+$" 0 'trailing-spaces-face t)
               ("AEK:?\\|FIXME:\\|TODO:\\|BUG:" 0 'font-lock-warning-face t)))))

;;=============================================================================
;; bell
;;=============================================================================
;; Emacs does not beep when you hit `C-g' in the minibuffer or during
;; an `isearch' (http://www.emacswiki.org/cgi-bin/wiki.pl?AlarmBell)
;; (setq ring-bell-function 
;;       (lambda ()
;; 	(unless (memq this-command
;; 		      '(isearch-abort abort-recursive-edit find-file
;; 				      exit-minibuffer keyboard-quit))
;; 	  (ding))))

;;;turn off the bell http://www.emacswiki.org/cgi-bin/wiki?AlarmBell
(setq ring-bell-function 'ignore)
;; (setq ring-bell-function (lambda nil)) ; No any bell (visual or beep)
;;=============================================================================

;; показ имени файла вместе с директорией в заголовке
(setq frame-title-format "%S: %f")

;;-----------------------------------------------------------------------------
(global-linum-mode) ; Нумерация строк

(require 'minimap)

;; (require 'rainbow-mode)
;;-----------------------------------------------------------------------------
;; Emacs custom color themes path
(add-to-list 'custom-theme-load-path "~/.emacs.d/custom")

(load-theme 'organic-green t)
;;-----------------------------------------------------------------------------

;;=============================================================================
;; StatusBar config
;;
(require 'mode-line-conf)
;;
;;=============================================================================

;;=============================================================================
;; Cursor config
;;
(global-hl-line-mode 1) ; highlight the line about point in the current window

(blink-cursor-mode -1)
;;
;;=============================================================================

(require 'window-number)
(window-number-mode)

;;=============================================================================
;; full screen toggle using command+[RET]
(defvar my-fullscreen-p t "Check if fullscreen is on or off")

(defun toggle-fullscreen () 
  (interactive) 
  (if (eq system-type 'windows-nt)
      ;; Максимизировать окно - Windows
      (progn            
        (defun my-non-fullscreen ()
          (interactive)
          (if (fboundp 'w32-send-sys-command)
              ;; WM_SYSCOMMAND restore #xf120
              (w32-send-sys-command 61728)
            (progn (set-frame-parameter nil 'width 82)
                   (set-frame-parameter nil 'fullscreen 'fullheight))))
        (defun my-fullscreen ()
          "Ajusts current frame to display properties"
          (interactive)
          (if (fboundp 'w32-send-sys-command)
              ;; WM_SYSCOMMAND maximaze #xf030
              (w32-send-sys-command 61488)
            (set-frame-parameter nil 'fullscreen 'fullboth)))
        (defun my-toggle-fullscreen ()
          (interactive)
          (setq my-fullscreen-p (not my-fullscreen-p))
          (if my-fullscreen-p
              (my-non-fullscreen)
            (my-fullscreen)))
        (my-toggle-fullscreen))
    ;; Максимизировать окно - Linux 
    (set-frame-parameter nil 'fullscreen 
                         (if (frame-parameter nil 'fullscreen) 
                             nil 
                           'fullboth))))

;; (run-with-idle-timer 0.1 nil 'toggle-fullscreen)
;; (setq term-setup-hook 'toggle-fullscreen)
(setq window-setup-hook 
      '(lambda() (interactive) 
         (progn (toggle-fullscreen) 
                (previous-buffer))))

(defun toggle-full-screen-win32 ()
 "Toggles full-screen mode for Emacs window on Win32."
 (interactive)
 (shell-command "emacs_fullscreen.exe"))
 
(global-set-key (kbd "C-M-<return>") 'toggle-full-screen-win32)

;;=============================================================================
;; Continuation lines
;;
;; Non-nil means do not display continuation lines.
;; Instead, give each line of text just one screen line.
(setq truncate-lines nil)
;; A value of nil means to respect the value of `truncate-lines'.
(setq truncate-partial-width-windows nil)

;;-----------------------------------------------------------------------------
;; Non-nil means no need to redraw entire frame after suspending.
(setq no-redraw-on-reenter nil)
;; update isn't paused when input is detected
(setq redisplay-dont-pause t)

(show-paren-mode 1)              ;; Выделение парных скобок
(setq inhibit-startup-message t) ;; не показывать сообщение при старте
(fset 'yes-or-no-p 'y-or-n-p)    ;; не печать yes целиком
(setq default-tab-width 4)       ;; количество пробелов в табуляции
(setq-default indent-tabs-mode nil)

(tool-bar-mode -1)
(scroll-bar-mode -1)
;; (scroll-bar-mode t)

;;=============================================================================
;; fringes
(setq fringe-mode t) ;Show fields
(setq-default indicate-buffer-boundaries 
      '((top . left) (bottom . left) (t . right)))
(setq-default indicate-empty-lines t)

;; (require 'yascroll)
;; (global-yascroll-bar-mode 1)


;;=============================================================================
;; fill-column-indicator
(require 'fill-column-indicator)
(setq-default fill-column 80)
(setq-default fci-rule-column fill-column)
(setq fci-rule-width 1)
(setq fci-rule-color "gray80")
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

;; del `lines' highlight
(setq whitespace-style 
      '(face tabs spaces trailing  space-before-tab 
             newline indentation empty space-after-tab 
             space-mark tab-mark newline-mark))

;;=============================================================================
;; Enables narrow possibility (`narrow-to-page' function).
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
;;=============================================================================

;;=============================================================================
;; Magic lisp parentheses rainbow
;;=============================================================================
;; Usage: Just enable highlight-parentheses-mode.
(require 'highlight-parentheses)

;; (require 'hi-list)
;; (set-face-background 'hi-list-face "#E3F2A1")
;; (add-hook 'emacs-lisp-mode-hook 'hi-list-mode)

(require 'highlight-parentheses)
(add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
(add-hook 'python-mode-hook 'highlight-parentheses-mode)
(add-hook 'comint-mode-hook 'highlight-parentheses-mode)
(add-hook 'js-mode-hook 'highlight-parentheses-mode)
(add-hook 'java-mode-hook 'highlight-parentheses-mode)
(add-hook 'clojure-mode-hook 'highlight-parentheses-mode)
(add-hook 'sql-mode-hook 'highlight-parentheses-mode)

(setq hl-paren-colors '("#326B6B"))
(setq hl-paren-background-colors '(
      "#00FF99" "#CCFF99" "#FFCC99" "#FF9999" "#FF99CC" 
      "#CC99FF" "#9999FF" "#99CCFF" "#99FFCC" "#7FFF00"))

(setq query-replace-highlight t)

;;-----------------------------------------------------------------------------
;; Заменяет lambda на λ.
(font-lock-add-keywords 
 'emacs-lisp-mode
 '(("(\\(lambda\\)\\>" (0 (prog1 ()
                       (compose-region (match-beginning 1)
                                       (match-end 1)
                                       ?λ))))))

;;-----------------------------------------------------------------------------

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(global-set-key (kbd "C-x p") popwin:keymap)

(push "*Kill Ring*" popwin:special-display-config)

(provide 'look-and-feel)

