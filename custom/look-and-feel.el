
;;-------------------------------------------------------------------
;; Font
(pcase system-type
  ('windows-nt
   (set-face-font 'default "Consolas-14.5:antialias=subpixel"))
  ('gnu/linux
   (set-face-font 'default "FiraMonoMedium-12.0:antialias=subpixel:rgba=rgb")))

;; In case of fire use this:
;; (font-family-list)
;; (find-font (font-spec :name "FiraMono"))

;;-------------------------------------------------------------------
;; Bell
;;
;;;turn off the bell http://www.emacswiki.org/cgi-bin/wiki?AlarmBell
(setq ring-bell-function 'ignore)

;;-------------------------------------------------------------------
;; Show frame number, file path & file name in app window header
(setq frame-title-format
      '((:eval (when (require 'dash nil 'noerror)
                 (format "%s/%s"
                         (number-to-string
                          (-elem-index (selected-frame) (frame-list)))
                         (number-to-string
                          (- (length (frame-list)) 1)))))
        " : %f"))

;;-------------------------------------------------------------------
;; Line numbers display in the buffer
(global-display-line-numbers-mode)

;;-------------------------------------------------------------------
;; Emacs custom color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/organic-green-theme")
(setq organic-green-boldless t)
(setq organic-green-version 2)
(load-theme 'organic-green t)

;;-------------------------------------------------------------------
;; StatusBar config
;;
(require 'mode-line-conf)

;;-------------------------------------------------------------------
;; Cursor config
;;
(blink-cursor-mode -1)
;; highlight the line about point in the current window
(global-hl-line-mode 1)

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

;;-------------------------------------------------------------------
;; Non-nil means no need to redraw entire frame after suspending.
(setq no-redraw-on-reenter nil)
;; t means update isn't paused when input is detected.
;; Nil means display update is paused when input is detected.
;; (setq redisplay-dont-pause nil)

(show-paren-mode 1)              ;; Visualize of matching parens
(setq inhibit-startup-message t) ;; Do not show startup message
(fset 'yes-or-no-p 'y-or-n-p)    ;; Ask user a "y or n" question
(setq default-tab-width 4)       ;; Number of spaces in the tab
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode t)
(set-scroll-bar-mode 'right) ; replace 'right with 'left to place it to the left

(setq query-replace-highlight t)

;;-------------------------------------------------------------------
;; Full screen toggle using command+[RET]
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

;; start the initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;-------------------------------------------------------------------
;; Fringes
;;
(fringe-mode '(8 . 8)) ; Show fields
(setq-default indicate-buffer-boundaries '((bottom . left)))
(setq-default indicate-empty-lines nil)

;;-------------------------------------------------------------------
;; fill-column-indicator
;;
(setq-default fill-column 70)
(setq-default display-fill-column-indicator-column 80)
(global-display-fill-column-indicator-mode t)

;; del `lines' highlight
(setq whitespace-style
      '(face tabs spaces trailing  space-before-tab
             newline indentation empty space-after-tab
             space-mark tab-mark newline-mark))

;;===================================================================
;; Continuation lines
;;
;; Non-nil means do not display continuation lines.
;; Instead, give each line of text just one screen line.
(setq-default truncate-lines t)
;; A value of nil means to respect the value of `truncate-lines'.
(setq truncate-partial-width-windows nil)

;;===================================================================
;; Enables narrow possibility (`narrow-to-page' function).
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
;;===================================================================

(defun font-lock-warn-todo ()
  "Make fixme tags standout."
  (font-lock-add-keywords
   'scala-mode '(("`.*'" 0 'font-lock-function-name-face t)))
  (font-lock-add-keywords nil
                          '(("AEK:?\\|FIXME:\\|TODO:\\|BUG:"
                             0 'font-lock-warning-face t))))

;;-------------------------------------------------------------------
;; Replace lambda with λ.
;;
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\(lambda\\)\\>" (0 (prog1 ()
                       (compose-region (match-beginning 1)
                                       (match-end 1)
                                       ?λ))))))

(provide 'look-and-feel)

