(require 'elpa-conf)

;;-----------------------------------------------------------------------------
;; Font
(when (eq system-type 'windows-nt)
  ;; (set-face-attribute 'default nil :family "Lucida Sans Typewriter" :height 120)
  (set-face-font 'default "Consolas-14.0:antialias=subpixel"))

(when (eq 'gnu/linux system-type)
  ;; (set-face-attribute 'default nil :family "Consolas" :height 130)
  ;; (set-face-font 'default "Consolas-13.5:antialias=subpixel")
  (set-face-font 'default "FiraMono-12.0:antialias=subpixel"))

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
(setq font-lock-maximum-decoration
      '((java-mode . (if (eq system-type 'windows-nt) 1 t))
        (t . t)))
(global-font-lock-mode t)

; Maximum size of a buffer for buffer fontification.
(if (< emacs-major-version 24)
    (setq font-lock-maximum-size 2560000))

;;=============================================================================
;; bell
;;=============================================================================
;; Emacs does not beep when you hit `C-g' in the minibuffer or during
;; an `isearch' (http://www.emacswiki.org/cgi-bin/wiki.pl?AlarmBell)
;; (setq ring-bell-function
;;       (lambda ()
;;  (unless (memq this-command
;;            '(isearch-abort abort-recursive-edit find-file
;;                    exit-minibuffer keyboard-quit))
;;    (ding))))

;;;turn off the bell http://www.emacswiki.org/cgi-bin/wiki?AlarmBell
(setq ring-bell-function 'ignore)
;; (setq ring-bell-function (lambda nil)) ; No any bell (visual or beep)
;;=============================================================================

;; показ имени файла вместе с директорией в заголовке
(setq frame-title-format "%S: %f")

;;-----------------------------------------------------------------------------
(global-linum-mode) ; Нумерация строк

;;-----------------------------------------------------------------------------
;; Emacs custom color themes path
(load-theme 'organic-green t)
;;-----------------------------------------------------------------------------

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;;=============================================================================
;; StatusBar config
;;
(require 'mode-line-conf)
;; (require 'powerline)
;; (powerline-default-theme)
;;
;;=============================================================================

;;=============================================================================
;; Cursor config
;;
(global-hl-line-mode 1) ; highlight the line about point in the current window

(blink-cursor-mode -1)
;;
;;=============================================================================

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

;(setq window-setup-hook 'toggle-fullscreen)

(defun fullscreen-linux (&optional f)
       (interactive)
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

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
;; (scroll-bar-mode -1)
(scroll-bar-mode t)
(set-scroll-bar-mode 'right) ; replace 'right with 'left to place it to the left

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

;; (require 'hi-list)
;; (set-face-background 'hi-list-face "#E3F2A1")
;; (add-hook 'emacs-lisp-mode-hook 'hi-list-mode)

(defun font-lock-warn-todo ()
  "Make fixme tags standout."
  (font-lock-add-keywords nil
                          '(("AEK:?\\|FIXME:\\|TODO:\\|BUG:"
                             0 'font-lock-warning-face t))))

(defface tcl-substitution-char-face '((t :inherit default))
  "Face used for substitution ($) char in Tcl mode."
  :group 'tcl)

(defvar tcl-substitution-char-face 'tcl-substitution-char-face
  "Face used for substitution ($) char in Tcl mode.")

(font-lock-add-keywords
 'tcl-mode
 '(("\\$" . tcl-substitution-char-face)))

;; cl-lib-highlight init
(cl-lib-highlight-initialize)

;; Font lock of dash functions in emacs lisp buffers
(eval-after-load "dash" '(dash-enable-font-lock))

;;=============================================================================
;; Magic lisp parentheses rainbow
;;=============================================================================
;; Usage: Just enable highlight-parentheses-mode.
;; (require 'highlight-parentheses)

(defun my-common-coding-hook ()
  (rainbow-delimiters-mode t)
  (idle-highlight-mode t)
  (font-lock-warn-todo))

(defun my-coding-hook ()
  (my-common-coding-hook)
  (paredit-everywhere-mode)
  (electric-pair-mode))

(defun my-lisp-coding-hook ()
  (my-common-coding-hook)
  (enable-paredit-mode))

(defun web-mode-hook ()
  "Hooks for Web mode."
  (idle-highlight-mode t))

(require 'paredit)

(add-to-list 'auto-mode-alist '("\\.iss$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.cnf$" . conf-mode))

(add-hook 'emacs-lisp-mode-hook 'my-lisp-coding-hook)
(add-hook 'scheme-mode-hook     'my-lisp-coding-hook)
(add-hook 'clojure-mode-hook    'my-lisp-coding-hook)
(add-hook 'cider-mode-hook      'my-lisp-coding-hook)
(add-hook 'scala-mode-hook    'my-coding-hook)
(add-hook 'markdown-mode-hook 'my-coding-hook)
(add-hook 'mql-mode-hook      'my-coding-hook)
(add-hook 'tex-mode-hook      'my-coding-hook)
(add-hook 'lua-mode-hook      'my-coding-hook)
(add-hook 'tcl-mode-hook      'my-coding-hook)
(add-hook 'java-mode-hook     'my-coding-hook)
(add-hook 'python-mode-hook   'my-coding-hook)
(add-hook 'comint-mode-hook   'my-coding-hook)
(add-hook 'js-mode-hook       'my-coding-hook)
(add-hook 'sql-mode-hook      'my-coding-hook)
(add-hook 'mql-mode-hook      'my-coding-hook)
(add-hook 'go-mode-hook       'my-coding-hook)
(add-hook 'web-mode-hook      'web-mode-hook)

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

;; (require 'popwin)
;; ;; Since popwin-el is conflicing with ECB.
;; ;; (setq display-buffer-function 'popwin:display-buffer)
;; (global-set-key (kbd "C-x p") popwin:keymap)
;; (push "*Kill Ring*" popwin:special-display-config)

;; Disable bidirectional text support
(setq-default bidi-display-reordering nil)

;; Windows shell (cmd) correct encoding
(when (eq system-type 'windows-nt)
  (defadvice shell (after my-shell-advice)
    (set-buffer-process-coding-system 'cp1251 'cp1251))
  (ad-activate 'shell))

;;=============================================================================
;; Speedbar
;;=============================================================================
(require 'sr-speedbar)
(setq sr-speedbar-right-side t)
(setq speedbar-use-images nil)
(setq speedbar-show-unknown-files t)
(setq speedbar-tag-split-minimum-length 200)
(setq speedbar-use-imenu-flag nil)

(provide 'look-and-feel)
