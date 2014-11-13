(require 'elpa-conf)


(require 'goto-last-change)

;; setting the PC keyboard's various keys to
;; Super or Hyper, for emacs running on Windows.
(setq w32-pass-lwindow-to-system nil
      w32-pass-rwindow-to-system nil
      w32-pass-apps-to-system nil
      w32-lwindow-modifier 'super ; Left Windows key
      w32-rwindow-modifier 'super ; Right Windows key
      w32-apps-modifier 'hyper) ; Menu key

;; ctrl + shift + click select rectange region
(require 'cua-rect)
(defun hkb-mouse-mark-cua-rectangle (event)
  (interactive "e")
  (if (not cua--rectangle)
  (cua-mouse-set-rectangle-mark event)
(cua-mouse-resize-rectangle event)))

;;=============================================================================
;; Включаем команды изменения регистра
;;=============================================================================
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;=============================================================================
;; Odinary C-<right>, C-<left> movements
;;=============================================================================
(require 'thingatpt)
;; (subword-mode)

(defun step-forward-word ()
  "Like odinary editors, C-<right> moves forward word."
  (interactive)
  (skip-chars-forward " \t")
  (forward-same-syntax 1))

(defun step-backward-word ()
  "Like odinary editors, C-<left> moves backward word."
  (interactive)
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
  (if (not mark-active)
      (progn
        (back-to-indentation)
        (cua-set-mark)
        (end-of-line arg))
    (progn
      (if (not (eq (line-beginning-position) (point)))
          (progn
            (setq mark-active nil)
            (beginning-of-line)
            (cua-set-mark)
            (setq mark-active t)))
      (end-of-line)
      (forward-line)
      (beginning-of-line))))

(defun copy-line (&optional arg)
  "Kills a line, not including leading indentation"
  (interactive "p")
  (save-excursion
    (mark-line arg)
    (kill-ring-save (point) (mark))))

(defun copy-url (&optional arg)
  "Copy a url under the cursor"
  (interactive "p")
  (let* ((beg (save-excursion
                (search-backward " " nil t arg)
                (right-char)
                (point)))
         (end (save-excursion
                (end-of-line)
                (point)))
         (url (buffer-substring beg end)))
    (kill-new url)
    (message (concat "Copied to buffer: " url))))

;;=============================================================================
;; Поиск и замена
;;=============================================================================
(add-hook 'isearch-mode-hook
          '(lambda ()
             (define-key isearch-mode-map "\C-f"
               'isearch-repeat-forward)
             (define-key isearch-mode-map "\C-r"
               'isearch-repeat-backward)
             (define-key isearch-mode-map "\C-v"
               'isearch-yank-kill)))

;; do not truncate and wrap long lines
(setq truncate-partial-width-windows nil)
(setq truncate-lines nil)

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
    (progn   (rename-file name new-name 1)   (rename-buffer new-name)    (set-visited-file-name new-name)    (set-buffer-modified-p nil))))))

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
 (progn     (copy-file filename newname 1)  (delete-file filename)  (set-visited-file-name newname)     (set-buffer-modified-p nil)     t))))
;;=============================================================================

;;=============================================================================
;; dired+
;;
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
  (mydired-sort))
;;
;;=============================================================================

;; Jump back to the last position of the cursor
(when (fboundp 'winner-mode)
  (winner-mode 1))

(defun meta-left ()
  (interactive)
  (condition-case err
      (windmove-left)
    (error
     (if (equal err '(error "No window left from selected window"))
         (progn
           (hop-backward)
           (setq this-command 'hop-backward))
       (message "%s" err)))))

(defun meta-right ()
  (interactive)
  (condition-case err
      (windmove-right)
    (error
     (if (equal err '(error "No window right from selected window"))
         (progn
           (hop-forward)
           (setq this-command 'hop-forward))
       (message "%s" err)))))

(defadvice ace-jump-char-category (around adv-ace-jump-support-umlauts activate)
  (unless (= (char-syntax (ad-get-arg 0)) ?w)
    ad-do-it)
  (setq ad-return-value 'alpha))

(provide 'navigation-and-simplify-keys)
