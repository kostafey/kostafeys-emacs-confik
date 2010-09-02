;Font
;; (set-face-attribute 'default nil :family "Lucida Sans Typewriter" :height 100)
(set-face-attribute 'default nil :family "Lucida Sans Typewriter" :height 110)

;(set-face-attribute 'default nil :family "Liberation Mono" :height 100)

(setq font-lock-maximum-decoration t)
(global-font-lock-mode 1)
;Maximum size of a buffer for buffer fontification.
(setq font-lock-maximum-size 2560000)

;; показ имени файла вместе с директорией в заголовке
(setq frame-title-format "%S: %f")

;;-----------------------------------------------------------------------------
;; Модуль нумерации строк
(require 'linum)
(global-linum-mode) ; Нумерация строк

(require 'minimap)
;;-----------------------------------------------------------------------------
;; Боевая раскраска
;;указываем где будут лежать файлы расширений
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/")
(require 'color-theme) ;;подгружаем "модуль раскраски"
(color-theme-initialize) ;;подгрузить библиотеку цветовых схем
;(load-file "~/.emacs.d/color-theme-6.6.0/themes/color-theme-organic-green.el")
(color-theme-organic-green);;выбрать конкретную схему
;(color-theme-aliceblue-mod) 
;;-----------------------------------------------------------------------------

;;=============================================================================
;; StatusBar config
;;
(column-number-mode t)
(display-time-mode t)
;;
;;=============================================================================

;;=============================================================================
;; Cursor config
;;
(global-hl-line-mode 1) ; highlight the line about point in the current window
(set-face-background 'hl-line "#afa")
(blink-cursor-mode -1)
;;
;;=============================================================================

(require 'window-number)
(window-number-mode)

;; full screen toggle using command+[RET]
(defun toggle-fullscreen () 
  (interactive) 
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) 
                                           nil 
										 'fullboth)))
(global-set-key [(meta return)] 'toggle-fullscreen) 

;;Максимизировать окно - Windows
(defun prh:ajust-frame ()
  "Ajusts current frame to display properties"
  (interactive)
  (w32-send-sys-command 61488))
(prh:ajust-frame)

;Длинные строки всегда разбивать при отображении
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

(setq no-redraw-on-reenter nil)

;; Usage: Just enable highlight-parentheses-mode.
(require 'highlight-parentheses)

(show-paren-mode 1)              ;;Выделение парных скобок
(setq inhibit-startup-message t) ;;не показывать сообщение при старте
(fset 'yes-or-no-p 'y-or-n-p)    ;;не печать yes целиком
(setq default-tab-width 4)       ;;количество пробелов в табуляции
(setq-default indent-tabs-mode nil)

(tool-bar-mode -1)
(scroll-bar-mode -1)

;;=============================================================================
(setq fringe-mode t) ;Show fields

;;Folding
(global-set-key [(control meta tab)] 'fold-dwim-toggle-selective-display)
(global-set-key "\C-cf" 'semantic-tag-folding-fold-block)
(global-set-key "\C-cs" 'semantic-tag-folding-show-block)
;;=============================================================================

(setq default-indicate-buffer-boundaries '((top . left) (bottom . left) (t . right)))
(setq default-indicate-empty-lines t)

(provide 'look-and-feel)


;; (defvar my-fullscreen-p t "Check if fullscreen is on or off")

;; (defun my-non-fullscreen ()
;;   (interactive)
;;   (if (fboundp 'w32-send-sys-command)
;; 	  ;; WM_SYSCOMMAND restore #xf120
;; 	  (w32-send-sys-command 61728)
;; 	(progn (set-frame-parameter nil 'width 82)
;; 		   (set-frame-parameter nil 'fullscreen 'fullheight))))

;; (defun my-fullscreen ()
;;   (interactive)
;;   (if (fboundp 'w32-send-sys-command)
;; 	  ;; WM_SYSCOMMAND maximaze #xf030
;; 	  (w32-send-sys-command 61488)
;; 	(set-frame-parameter nil 'fullscreen 'fullboth)))

;; (defun my-toggle-fullscreen ()
;;   (interactive)
;;   (setq my-fullscreen-p (not my-fullscreen-p))
;;   (if my-fullscreen-p
;; 	  (my-non-fullscreen)
;; 	(my-fullscreen)))

; при запуске - разворачиваем на весь экран - Linux
;(set-frame-parameter nil 'fullscreen
;   (if (frame-parameter nil 'fullscreen) nil 'fullboth))
; устанавливаем позицию и размер фрейма
; (add-to-list 'default-frame-alist '(left . 0))
; (add-to-list 'default-frame-alist '(top . 0))
; (add-to-list 'default-frame-alist '(height . 45))
; (add-to-list 'default-frame-alist '(width . 154))
;(set-frame-position (selected-frame) 0 0)
;(set-frame-size (selected-frame) 154 45)
