(require 'elpa-conf)

;;-----------------------------------------------------------------------------
;; Font
(case system-type
  ('windows-nt
   (set-face-font 'default "Consolas-14.5:antialias=subpixel"))
  ('gnu/linux
   (set-face-font 'default "FiraMonoMedium-12.0:antialias=subpixel:rgba=rgb")))

;; In case of fire use this:
;; (font-family-list)
;; (find-font (font-spec :name "FiraMono"))

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

;; Show file path & file name in app window header
(setq frame-title-format "%S: %f")

;;-----------------------------------------------------------------------------
;; Line numbers display in the buffer
(global-display-line-numbers-mode)

;;-----------------------------------------------------------------------------
;; Emacs custom color theme
(setq organic-green-boldless t)
(load-theme 'organic-green t)

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

;;-----------------------------------------------------------------------------
;; Non-nil means no need to redraw entire frame after suspending.
(setq no-redraw-on-reenter nil)
;; update isn't paused when input is detected
(setq redisplay-dont-pause t)

(show-paren-mode 1)              ;; Выделение парных скобок
(setq inhibit-startup-message t) ;; не показывать сообщение при старте
(fset 'yes-or-no-p 'y-or-n-p)    ;; не печать yes целиком
(setq default-tab-width 4)       ;; количество пробелов в табуляции
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode t)
(set-scroll-bar-mode 'right) ; replace 'right with 'left to place it to the left

(setq query-replace-highlight t)

;;=============================================================================
;; full screen toggle using command+[RET]
(defvar my-fullscreen-p t "Check if fullscreen is on or off")

(defun toggle-fullscreen ()
  (interactive)
  (if (eq system-type 'windows-nt)
      ;; Windows
      (progn
        (setq my-fullscreen-p (not my-fullscreen-p))
        (if my-fullscreen-p
            ;; set non-fullscreen
            (if (fboundp 'w32-send-sys-command)
                ;; WM_SYSCOMMAND restore #xf120
                (w32-send-sys-command 61728)
              (progn (set-frame-parameter nil 'width 82)
                     (set-frame-parameter nil 'fullscreen 'fullheight)))
          ;; set fullscreen
          (if (fboundp 'w32-send-sys-command)
              ;; WM_SYSCOMMAND maximaze #xf030
              (w32-send-sys-command 61488)
            (set-frame-parameter nil 'fullscreen 'fullboth))))
    ;; Linux
    (set-frame-parameter nil 'fullscreen
                         (if (frame-parameter nil 'fullscreen)
                             nil
                           'fullboth))))

;; (setq window-setup-hook 'toggle-fullscreen)

;;=============================================================================
;; Continuation lines
;;
;; Non-nil means do not display continuation lines.
;; Instead, give each line of text just one screen line.
(setq truncate-lines nil)
;; A value of nil means to respect the value of `truncate-lines'.
(setq truncate-partial-width-windows nil)

;;=============================================================================
;; fringes
(setq fringe-mode t) ; Show fields
(setq-default indicate-buffer-boundaries
      '((top . left) (bottom . left) (t . right)))
(setq-default indicate-empty-lines t)

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

(defun my-web-mode-hook ()
  (my-coding-hook)
  (setq-default indent-tabs-mode nil)
  ;; (setq indent-line-function 'web-mode-indent-line)
  (setq-local indent-line-function 'indent-relative))

(defun my-lisp-coding-hook ()
  (my-common-coding-hook)
  (enable-paredit-mode))

(require 'paredit)

(add-to-list 'auto-mode-alist '("\\.iss$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.cnf$" . conf-mode))

(add-hook 'emacs-lisp-mode-hook 'my-lisp-coding-hook)
(add-hook 'lisp-mode-hook       'my-lisp-coding-hook)
(add-hook 'scheme-mode-hook     'my-lisp-coding-hook)
(add-hook 'clojure-mode-hook    'my-lisp-coding-hook)
(add-hook 'cider-mode-hook      'my-lisp-coding-hook)
(add-hook 'sbt-mode-hook        'my-coding-hook)
(add-hook 'java-mode-hook       (lambda () (rainbow-delimiters-mode t)))
(add-hook 'markdown-mode-hook   'my-coding-hook)
(add-hook 'mql-mode-hook        'my-coding-hook)
(add-hook 'tex-mode-hook        'my-coding-hook)
(add-hook 'lua-mode-hook        'my-coding-hook)
(add-hook 'tcl-mode-hook        'my-coding-hook)
(add-hook 'python-mode-hook     'my-coding-hook)
(add-hook 'comint-mode-hook     'my-coding-hook)
(add-hook 'js-mode-hook         'my-coding-hook)
(add-hook 'typescript-mode-hook 'my-coding-hook)
(add-hook 'tide-mode            'my-coding-hook)
(add-hook 'sql-mode-hook        'my-coding-hook)
(add-hook 'mql-mode-hook        'my-coding-hook)
(add-hook 'go-mode-hook         'my-coding-hook)
(add-hook 'powershell-mode-hook 'my-coding-hook)
(add-hook 'rust-mode-hook       'my-coding-hook)
(add-hook 'web-mode-hook        'my-web-mode-hook)

;;-----------------------------------------------------------------------------
;; Replace lambda with λ.
;;
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\(lambda\\)\\>" (0 (prog1 ()
                       (compose-region (match-beginning 1)
                                       (match-end 1)
                                       ?λ))))))

;;-----------------------------------------------------------------------------
;; Fix `describe-face' fn call when `hl-line' is enabled.
;;
(defun my-face-at-point ()
  (let ((face (get-text-property (point) 'face)))
    (or (and (face-list-p face)
             (car face))
        (and (symbolp face)
             face))))

(defun what-face (pos)
  (interactive)
  (message "Face: %s" (my-face-at-point)))

(defun my-describe-face (&rest ignore)
  (interactive (list (read-face-name "Describe face"
                                     (or (my-face-at-point) 'default)
                                     t)))
  ;; This only needs to change the `interactive` spec, so:
  nil)

(eval-after-load "hl-line"
  '(advice-add 'describe-face :before #'my-describe-face))

(provide 'look-and-feel)
