;;-----------------------------------------------------------------------------
;; Font
;; (set-face-attribute 'default nil :family "Lucida Sans Typewriter" :height 100)
;; (set-face-attribute 'default nil :family "Lucida Sans Typewriter" :height 110)
;; (set-face-attribute 'default nil :family "Consolas" :height 140)
;; (set-face-attribute 'default nil :family "Liberation Mono" :height 100)
(set-face-attribute 'default nil :family "Consolas" :height 130)

;;=============================================================================
;; Change font size
;;=============================================================================
(defun djcb-zoom (n)
  "with positive N, increase the font size, otherwise decrease it"
  (set-face-attribute 'default (selected-frame) :height 
    (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 10))))

(global-set-key (kbd "C-+")      '(lambda nil (interactive) (djcb-zoom 1)))
(global-set-key [C-kp-add]       '(lambda nil (interactive) (djcb-zoom 1)))
(global-set-key (kbd "C--")      '(lambda nil (interactive) (djcb-zoom -1)))
(global-set-key [C-kp-subtract]  '(lambda nil (interactive) (djcb-zoom -1)))

;;=============================================================================
;; Font decorations
;;=============================================================================
(setq font-lock-maximum-decoration t)
(global-font-lock-mode 1)
;Maximum size of a buffer for buffer fontification.
(setq font-lock-maximum-size 2560000)

(require 'font-lock)
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode t))
(make-face 'trailing-spaces-face "Face to display trailing spaces in.")
(add-hook 'font-lock-mode-hook     ; Show trailing spaces and make fixme tags standout
          (lambda ()
            (font-lock-add-keywords nil
             '(("[ \t]+$" 0 'trailing-spaces-face t)
               ("AEK:?\\|FIXME:\\|TODO:\\|BUG:" 0 'font-lock-warning-face t)))))

;; (add-hook 'fundamental-mode-hook
;;                (lambda ()
;;                 (font-lock-add-keywords nil
;;                  '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

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
;; Модуль нумерации строк
(require 'linum)
(global-linum-mode) ; Нумерация строк

(require 'minimap)

(require 'rainbow-mode)
;;-----------------------------------------------------------------------------
;; Боевая раскраска
;;указываем где будут лежать файлы расширений
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/")
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/themes/")

(require 'color-theme) ;;подгружаем "модуль раскраски"
(color-theme-initialize) ;;подгрузить библиотеку цветовых схем
;(load-file "~/.emacs.d/color-theme-6.6.0/themes/color-theme-organic-green.el")
(color-theme-organic-green);;выбрать конкретную схему
;; (require 'color-theme-solarized)
;; (color-theme-solarized-light)
;(color-theme-aliceblue-mod) 
;;-----------------------------------------------------------------------------

;;=============================================================================
;; StatusBar config
;;
(column-number-mode t)
(display-time-mode t)

(if (< emacs-major-version 24)
 (progn
   ;; battery mode:
   (require 'battery)
   (setq battery−mode−line−format " [%L %p%% %dC]")
   (display-battery-mode)))

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
  (set-frame-parameter nil 'fullscreen 
                       (if (frame-parameter nil 'fullscreen) 
                           nil 
                         'fullboth)))
(global-set-key [(meta return)] 'toggle-fullscreen)

(if (eq system-type 'windows-nt)
    ;; Максимизировать окно - Windows
    (progn            
      (defun prh:ajust-frame ()
        "Ajusts current frame to display properties"
        (interactive)
        (w32-send-sys-command 61488))
      (prh:ajust-frame))
  ;; При запуске - разворачиваем на весь экран - Linux 
  (toggle-fullscreen))

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
;; (scroll-bar-mode t)

;;=============================================================================
(setq fringe-mode t) ;Show fields

;;Folding
(global-set-key [(control meta tab)] 'fold-dwim-toggle-selective-display)
(global-set-key "\C-cf" 'semantic-tag-folding-fold-block)
(global-set-key "\C-cs" 'semantic-tag-folding-show-block)
;;=============================================================================

(setq default-indicate-buffer-boundaries '((top . left) (bottom . left) (t . right)))
(setq default-indicate-empty-lines t)

;;=============================================================================
;; Magic lisp parentheses rainbow
;;=============================================================================
;; (require 'hi-list)
;; (set-face-background 'hi-list-face "#E3F2A1")
;; (add-hook 'emacs-lisp-mode-hook 'hi-list-mode)

(require 'highlight-parentheses)
(add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
(add-hook 'python-mode-hook 'highlight-parentheses-mode)
(add-hook 'comint-mode-hook 'highlight-parentheses-mode)

(setq hl-paren-colors '("#326B6B"))
(setq hl-paren-background-colors '(
      "#00FF99" "#CCFF99" "#FFCC99" "#FF9999" "#FF99CC" 
      "#CC99FF" "#9999FF" "#99CCFF" "#99FFCC" "#7FFF00"))
;; (setq hl-paren-colors '
;;       ("#326B6B" "#66CC66" "#73CD4F" "#32CD32"
;;        "#6495ED" "#9E9B29" "#32227B" "#226B4B"))
;; (setq hl-paren-background-colors '(
;;       "#00FF99" "#CCFF99" "#FFCC99" "#FF9999" 
;;       "#FF99CC" "#CC99FF" "#9999FF" "#99CCFF" 
;;       "#99FFCC" "#7FFF00" "#73CDF4" "#DDEE00"))

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


; устанавливаем позицию и размер фрейма
; (add-to-list 'default-frame-alist '(left . 0))
; (add-to-list 'default-frame-alist '(top . 0))
; (add-to-list 'default-frame-alist '(height . 45))
; (add-to-list 'default-frame-alist '(width . 154))
;(set-frame-position (selected-frame) 0 0)
;(set-frame-size (selected-frame) 154 45)
