
;;-----------------------------------------------------------------------------
(global-set-key [(control shift v)] 'cua-paste-pop)
(global-set-key [(meta shift v)] '(lambda() (interactive) (cua-paste-pop -1)))

;;-----------------------------------------------------------------------------
;(global-set-key (kbd "C-e") 'exchange-point-and-mark)
(global-set-key (kbd "C-e") 'cua-exchange-point-and-mark)
;;-----------------------------------------------------------------------------
(global-set-key "\M-s" 'set-mark-command)
;;-----------------------------------------------------------------------------
(require 'redo)
(global-unset-key "\C-_")
(global-unset-key [(control /)])
(global-set-key "\C-z" 'undo)                       ; Undo C-z
(global-set-key [(meta backspace)] 'undo)
(global-set-key [(control shift z)] 'redo)          ; Redo C-S-z

(global-set-key "\M-z" 'iconify-or-deiconify-frame) ; Свернуть окно
(global-set-key "\C-a" 'mark-whole-buffer)
(global-set-key "\C-\M-k" 'kill-whole-line)
(global-set-key "\C-k" 'kill-line)
(global-set-key "\C-b" 'backward-delete-char)
(global-set-key "\C-d" 'delete-char)                ; delete
(global-set-key "\C-q" 'quoted-insert)
(global-set-key [(delete)] 'delete-char)
(global-set-key "\C-l" 'recenter-top-bottom)
(global-set-key [(meta f4)] 'save-buffers-kill-terminal)

(global-set-key (kbd "M-t") 'transpose-words)
(global-set-key (kbd "M-y") '(lambda() (interactive) (transpose-words -1)))

(global-set-key (kbd "C-/") 'repeat)

;;=============================================================================
;; Select by mouse and shift
;;
;; shift + click select region
(define-key global-map (kbd "<S-down-mouse-1>") 'ignore) ; turn off font dialog
(define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
(put 'mouse-set-point 'CUA 'move)

;; ctrl + shift + click select rectange region
(require 'cua-rect)
(defun hkb-mouse-mark-cua-rectangle (event)
  (interactive "e")
  (if (not cua--rectangle)
  (cua-mouse-set-rectangle-mark event)
(cua-mouse-resize-rectangle event)))

(require 'cua-base)
(global-unset-key (kbd "<C-S-down-mouse-1>"))
(global-set-key (kbd "<C-S-mouse-1>") 'hkb-mouse-mark-cua-rectangle)
(define-key cua--rectangle-keymap (kbd "<C-S-mouse-1>") 'hkb-mouse-mark-cua-rectangle)

;;=============================================================================
;; Включаем команды изменения регистра
;;=============================================================================
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(global-set-key (concat selected-area-prefix "u") 'upcase-region)
(global-set-key (concat selected-area-prefix "l") 'downcase-region)
;;=============================================================================
(global-set-key (concat selected-area-prefix "c") 'comment-dwim)
(global-set-key (kbd "C-;") 'comment-or-uncomment-this)
(global-set-key (concat selected-area-prefix "c") 'comment-region)
(global-set-key (concat selected-area-prefix "d") 'uncomment-region)

;;=============================================================================
;; Odinary C-<right>, C-<left> movements 
;;=============================================================================
(require 'thingatpt)

(defun step-forward-word ()
  "Like odinary editors, C-<right> moves forward word."
  (interactive)
  (skip-chars-forward " \t")
  (forward-same-syntax 1))

(defun step-backward-word ()
  (interactive)
  "Like odinary editors, C-<left> moves backward word."
  (skip-chars-backward " \t")
  (forward-same-syntax -1))

(defun step-forward-select ()
  "Like odinary editors, C-S-<right> moves forward word and selects it."
  (interactive)
  (if (not mark-active)
      (cua-set-mark))
  (step-forward-word))

(defun step-backward-select ()
  "Like odinary editors, C-S-<left> moves backward word and selects it."
  (interactive)
  (if (not mark-active)
      (cua-set-mark))
  (step-backward-word))

;;=============================================================================
;; Marks&select a line
;;=============================================================================
(defun mark-line (&optional arg)
  "Marks a line from start of indentation to end"
  (interactive "p")

  (back-to-indentation)
  (cua-set-mark)
  (end-of-line arg))

(defun copy-line (&optional arg)
  "Kills a line, not including leading indentation"
  (interactive "p")
  (save-excursion
    (mark-line arg)
    (kill-ring-save (point) (mark))))

(global-set-key (kbd "C-S-c") 'copy-line)
(global-set-key (kbd "C-S-l") 'mark-line)

;;-----------------------------------------------------------------------------
(global-set-key "\C-s" 'save-buffer)
(global-set-key [f2] 'save-buffer)
;;-----------------------------------------------------------------------------
; Метки текста
(global-set-key [f5] 'bookmark-set)
(global-set-key [f6] 'bookmark-jump)

(global-set-key (kbd "C-o") 'bookmark-set)
(global-set-key (kbd "C-p") 'bookmark-jump)
;;-----------------------------------------------------------------------------

(require 'goto-last-change)
(global-set-key "\C-xx" 'goto-last-change)

;; Cancel all changes from last save
(global-set-key (kbd "C-x r") 'revert-buffer)

;;=============================================================================
;; Поиск и замена
;;=============================================================================
(global-unset-key "\C-f")
(global-set-key "\C-f" 'isearch-forward)
(global-set-key "\C-r" 'isearch-backward)

(add-hook 'isearch-mode-hook
		  '(lambda ()
			 (define-key isearch-mode-map "\C-f"
			   'isearch-repeat-forward)
			 (define-key isearch-mode-map "\C-r"
			   'isearch-repeat-backward)
			 (define-key isearch-mode-map "\C-v"
			   'isearch-yank-kill)))

(global-unset-key (kbd "M-r"))
(global-set-key (kbd "M-r") 'replace-string)

;;=============================================================================
;; Навигация по окнам
;;=============================================================================
(global-set-key [(control tab)] 'other-window) ; C-tab switchs to a next window
(windmove-default-keybindings 'meta)           ; M-up/down/left/right

;;=============================================================================
;; Навигация по буферам
;;=============================================================================
(global-set-key [(control next)] 'next-buffer) 		; C-Page Up
(global-set-key [(control prior)] 'previous-buffer)	; C-Page Down

;;=============================================================================
;; Physical line navigation
;;=============================================================================
;; do not truncate and wrap long lines
(setq truncate-partial-width-windows nil)
(setq truncate-lines nil)

;; and move up down end begin over the real visible screen lines
(require 'physical-line)
(physical-line-mode 1)

(global-set-key [(up)] 'previous-line)
(global-set-key [(down)] 'next-line)

(global-set-key [(end)] 'end-of-line)
(global-set-key [(home)] 'beginning-of-line)

;;=============================================================================
;; Meta - Навигация
;;=============================================================================
(global-set-key "\M-g" 'goto-line)
;;l - влево j - вправо i - вверх k - вниз	
(global-set-key "\M-i" 'previous-line)
(global-set-key "\M-k" 'next-line)
(global-set-key "\M-j" 'backward-char)
(global-set-key "\M-l" 'forward-char)

(global-set-key "\C-cr" 'reposition-window)
(global-unset-key "\M-\C-l")
(global-set-key "\M-\C-j" 'backward-word)
(global-set-key "\M-\C-l" 'forward-word)

(global-set-key "\M-o" 'end-of-line)
(global-set-key "\M-u" 'beginning-of-line)

(global-set-key "\M-m" 'scroll-up)
(global-set-key "\M-," 'scroll-down)

(global-set-key "\M-M" '(lambda () (interactive) (scroll-up 1)))
(global-set-key "\M-<" '(lambda () (interactive) (scroll-down 1)))

;;=============================================================================
;; Скроллинг
;;=============================================================================
;;mouse
(setq mouse-wheel-mode t)
(setq mouse-wheel-progressive-speed nil)

(setq mouse-drag-copy-region nil)

(if (eq system-type 'gnu/linux)
    (progn
      (defun smooth-scroll (increment)
        (scroll-up increment) (sit-for 0.05)
        ;; (scroll-up increment) (sit-for 0.02)
        ;; (scroll-up increment) (sit-for 0.02)
        ;; (scroll-up increment) (sit-for 0.05)
        ;; (scroll-up increment) (sit-for 0.06)
        (scroll-up increment))

      (global-set-key [(mouse-5)] '(lambda () (interactive) (smooth-scroll 1)))
      (global-set-key [(mouse-4)] '(lambda () (interactive) (smooth-scroll -1)))))

;;keyboard
(setq scroll-step 1)                     ; Шаг прокрутки
(setq next-screen-context-lines 10)      ; Number of lines of continuity when 
                                         ; scrolling by screenfuls.

;; Если тока вышла за пределы окна на число не первосходящее данное,
;; то прокрутить лишь настолько, чтобы вернуть точку в окно
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position t) ; Не изменять положение точки после прокрутки
(setq scroll-margin 0)                   ; Граница прокрутки
;;-----------------------------------------------------------------------------
;прокрутка экрана при неподвижной точке
(if (< emacs-major-version 24)
    (progn
    (global-set-key [(control down)] (lambda () (interactive) (scroll-up 1))) ; [C-down]
    (global-set-key [(control up)] (lambda () (interactive) (scroll-down 1)))) ; [C-up]
  (progn
    (global-set-key [(control down)] 'scroll-up-line) ; [C-down]
    (global-set-key [(control up)] 'scroll-down-line))) ; [C-up]

;;-----------------------------------------------------------------------------
(global-set-key [(meta control down)] 'forward-sentence)
(global-set-key [(meta control up)] 'backward-sentence)
;;-----------------------------------------------------------------------------

(require 'pager)
;;; Bind scrolling functions from pager library.
(global-set-key [next] 	   'pager-page-down)
(global-set-key [prior]	   'pager-page-up)

;;=============================================================================
;; Someday might want to rotate windows if more than 2 of them
(defun swap-windows ()
 "If you have 2 windows, it swaps them." 
 (interactive) 
 (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
 (t
 (let* ((w1 (first (window-list)))
	 (w2 (second (window-list)))
	 (b1 (window-buffer w1))
	 (b2 (window-buffer w2))
	 (s1 (window-start w1))
	 (s2 (window-start w2)))
 (set-window-buffer w1 b2)
 (set-window-buffer w2 b1)
 (set-window-start w1 s2)
 (set-window-start w2 s1)))))

(global-unset-key "\C-u")
(global-set-key "\C-u" 'swap-windows)

(defun mirror-window ()
 "Show the same buffer in the second window as in the active window." 
 (interactive) 
 (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
 (t
 (let* ((w1 (first (window-list)))
        (w2 (second (window-list)))
        (b1 (window-buffer w1))
        (s1 (window-start w1)))
   (set-window-start w2 s1)   
   (set-window-buffer w2 b1)))))

(global-unset-key (kbd "M-m"))
(global-set-key (kbd "M-m") 'mirror-window)

;;=============================================================================
;; Standard file-manipulation functions:
;;
(defun rename-file-and-buffer (new-name)
 "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
 (let ((name (buffer-name))
	(filename (buffer-file-name)))
 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (if (get-buffer new-name)
	 (message "A buffer named '%s' already exists!" new-name)
	(progn 	 (rename-file name new-name 1) 	 (rename-buffer new-name) 	 (set-visited-file-name new-name) 	 (set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
 (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	 (if (string-match dir "\\(?:/\\|\\\\)$")
	 (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))

 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (progn 	(copy-file filename newname 1) 	(delete-file filename) 	(set-visited-file-name newname) 	(set-buffer-modified-p nil) 	t)))) 
;;=============================================================================

(provide 'navigation-and-simplify-keys)

