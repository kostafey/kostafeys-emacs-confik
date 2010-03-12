;;=============================================================================
;; CUA - гумоноидизация emacs ;)
;;=============================================================================
;; Выбираем текст
;;Установка режима CUA
; M-w - копировать ; C-y - вставить
; C-w - вырезать   ; C-k - вырезать до конца строки
;(cua-selection-mode t)
;;поддержка Ctr-c,v,x,d как в windows
(require 'cua-base)
(cua-mode t)
(require 'pc-select)
(pc-selection-mode t)
(setq transient-mark-mode t)
;;-----------------------------------------------------------------------------
(global-set-key [(control shift v)] 'cua-paste-pop)
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
;;-----------------------------------------------------------------------------

;;=============================================================================
;; Перфикс для ключей, применяемых к выделенным областям
;;=============================================================================
(global-unset-key "\C-\M-a")
(defvar selected-area-prefix "\C-\M-a")

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
;;-----------------------------------------------------------------------------
(global-set-key "\C-s" 'save-buffer)
(global-set-key [f2] 'save-buffer)
;;-----------------------------------------------------------------------------
; Метки текста
;; (global-set-key [f5] 'bookmark-set)
;; (global-set-key [f6] 'bookmark-jump)

(global-set-key (kbd "C-i") 'bookmark-set)
(global-set-key (kbd "C-p") 'bookmark-jump)
;;-----------------------------------------------------------------------------

;;=============================================================================
;; Поиск
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

;;keyboard
(setq scroll-step 1)                     ; Шаг прокрутки
(setq next-screen-context-lines 10)      ; Number of lines of continuity when scrolling by screenfuls. 
;; Если тока вышла за пределы окна на число не первосходящее данное,
;; то прокрутить лишь настолько, чтобы вернуть точку в окно
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position t) ; Не изменять положение точки после прокрутки
(setq scroll-margin 0)                   ; Граница прокрутки
;;-----------------------------------------------------------------------------
;прокрутка экрана при неподвижной точке
(global-set-key [(control down)] (lambda () (interactive) (scroll-up 1))) ; [C-down]
(global-set-key [(control up)] (lambda () (interactive) (scroll-down 1))) ; [C-up]
;;-----------------------------------------------------------------------------
(global-set-key [(meta control down)] 'forward-sentence)
(global-set-key [(meta control up)] 'backward-sentence)
;;-----------------------------------------------------------------------------

(defun sfp-page-down ()
  (interactive)
  (next-line
   (- (window-text-height)
	  next-screen-context-lines)))
    
(defun sfp-page-up ()
  (interactive)
  (previous-line
   (- (window-text-height)
	  next-screen-context-lines)))
    
(global-set-key [next] 'sfp-page-down)
(global-set-key [prior] 'sfp-page-up)

(defun point-of-beginning-of-bottom-line ()
  (save-excursion
    (move-to-window-line -1)
    (point)))
(defun point-of-beginning-of-line ()
  (save-excursion
    (beginning-of-line)
    (point)))
(defun next-one-line () (interactive)
  (if (= (point-of-beginning-of-bottom-line) (point-of-beginning-of-line))
      (progn (scroll-up 1)
             (next-line 1))
    (next-line 1)))
(defun point-of-beginning-of-top-line ()
  (save-excursion
    (move-to-window-line 0)
    (point)))
(defun previous-one-line () (interactive)
  (if (= (point-of-beginning-of-top-line) (point-of-beginning-of-line))
      (progn (scroll-down 1)
             (previous-line 1))
    (previous-line 1)))

(byte-compile 'point-of-beginning-of-bottom-line)
(byte-compile 'point-of-beginning-of-line)
(byte-compile 'point-of-beginning-of-top-line)

(global-set-key (kbd "<down>") 'next-one-line)
(global-set-key (kbd "<up>") 'previous-one-line)
;;=============================================================================

(defvar change-buffer-prefix "\C-c\C-b")

(global-set-key (concat selected-area-prefix "\C-e") '(lambda () (interactive) (find-file "~/.emacs")))

(global-set-key "\C-x\C-c" '(lambda () (interactive) (switch-to-buffer "temp") (linum-mode t)))
(global-set-key (concat change-buffer-prefix "t") '(lambda () (interactive) (switch-to-buffer "temp") (linum-mode t)))

(global-set-key (concat change-buffer-prefix "p") '(lambda () (interactive) (find-file "~/.org.gpg")))
(global-set-key (concat change-buffer-prefix "k") '(lambda () (interactive) (find-file "~/.keys.org")))

(global-set-key (concat change-buffer-prefix "b") 'switch-to-buffer)

(provide 'navigation-and-simplify-keys)
