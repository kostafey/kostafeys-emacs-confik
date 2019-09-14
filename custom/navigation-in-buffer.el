;;; navigation-in-buffer.el --- Simplify point navigation inside buffer.

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
;; Enable case changes commands
;;=============================================================================
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;=============================================================================
;; Odinary C-<right>, C-<left> movements
;;=============================================================================
(require 'thingatpt)
;; (subword-mode)

(defun k/select ()
    (if (not mark-active)
        (cua-set-mark)))

(defun k/deselect ()
  (if (not cua--rectangle)
      (setq deactivate-mark t)))

(defun k/step-forward-word ()
  "Like odinary editors, C-<right> moves forward word."
  (skip-chars-forward " \t")
  (forward-same-syntax 1))

(defun k/step-backward-word ()
  "Like odinary editors, C-<left> moves backward word."
  (skip-chars-backward " \t")
  (forward-same-syntax -1))

(require 'eldoc)
;; Run ElDoc after this commands:
(mapc 'eldoc-add-command '(k/char-forward
                           k/char-backward
                           k/word-forward
                           k/word-backward
                           k/sexp-forward
                           k/sexp-backward
                           k/line-next
                           k/line-previous))

(defun k/line-next () (interactive) (k/deselect) (line-move 1))
(defun k/line-previous () (interactive) (k/deselect) (line-move -1))
(defun k/line-next-select () (interactive) (k/select) (line-move 1))
(defun k/line-previous-select () (interactive) (k/select) (line-move -1))

(defun k/char-forward () (interactive) (k/deselect) (right-char 1))
(defun k/char-backward () (interactive) (k/deselect) (left-char 1))
(defun k/char-forward-select () (interactive) (k/select) (right-char 1))
(defun k/char-backward-select () (interactive) (k/select) (left-char 1))

(defun k/word-forward () (interactive) (k/deselect) (k/step-forward-word))
(defun k/word-backward () (interactive) (k/deselect) (k/step-backward-word))
(defun k/word-forward-select () (interactive) (k/select) (k/step-forward-word))
(defun k/word-backward-select () (interactive) (k/select) (k/step-backward-word))

(defun k/sexp-forward () (interactive) (k/deselect) (forward-sexp 1))
(defun k/sexp-backward () (interactive) (k/deselect) (backward-sexp 1))
(defun k/sexp-forward-select () (interactive) (k/select) (forward-sexp 1))
(defun k/sexp-backward-select () (interactive) (k/select) (backward-sexp 1))

(defun k/line-beginning () (interactive) (k/deselect) (beginning-of-line))
(defun k/line-end () (interactive) (k/deselect) (end-of-line))
(defun k/line-beginning-select () (interactive) (k/select) (beginning-of-line))
(defun k/line-end-select () (interactive) (k/select) (end-of-line))

(defun k/buffer-beginning () (interactive) (k/deselect) (goto-char (point-min)))
(defun k/buffer-end () (interactive) (k/deselect) (goto-char (point-max)))
(defun k/buffer-beginning-select () (interactive) (k/select) (goto-char (point-min)))
(defun k/buffer-end-select () (interactive) (k/select) (goto-char (point-max)))

(when (require 'sgml-mode nil 'noerror)

  (defvar k/sgml-tags (list "<" ">"))

  (defun k/sgml-skip-tag-forward (&optional select)
    (interactive)
    (if (member (string (following-char)) k/sgml-tags)
        (sgml-skip-tag-forward 1)
        (if select
            (k/sexp-forward-select)
          (k/sexp-forward))))

  (defun k/sgml-skip-tag-backward (&optional select)
    (interactive)
    (if (member (string (preceding-char)) k/sgml-tags)
        (sgml-skip-tag-backward 1)
        (if select
            (k/sexp-backward-select)
          (k/sexp-backward))))

  (defun k/sgml-skip-tag-forward-select ()
    (interactive)
    (k/select)
    (k/sgml-skip-tag-forward t))

  (defun k/sgml-skip-tag-backward-select ()
    (interactive)
    (k/select)
    (k/sgml-skip-tag-backward t)))

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
;; Search & replace
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
;; Scrolling
;;=============================================================================
;; mouse
(setq mouse-wheel-mode t)
(setq mouse-wheel-progressive-speed nil)

(setq mouse-drag-copy-region nil)

(if (eq system-type 'gnu/linux)
    (progn
      (defun smooth-scroll (increment)
        (scroll-up increment) (sit-for 0.05)
        (scroll-up increment))
      (global-set-key [(mouse-5)] '(lambda () (interactive) (smooth-scroll 1)))
      (global-set-key [(mouse-4)] '(lambda () (interactive) (smooth-scroll -1)))))

;; keyboard
(setq scroll-step 1)
(setq next-screen-context-lines 10)     ; Number of lines of continuity when
                                        ; scrolling by screenfuls.

;; Если точка вышла за пределы окна на число не первосходящее данное,
;; то прокрутить лишь настолько, чтобы вернуть точку в окно
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position t) ; Не изменять положение точки после прокрутки
(setq scroll-margin 0)                   ; Граница прокрутки

;;=============================================================================
;; Someday might want to rotate windows if more than 2 of them
(defun swap-buffers (w1 w2)
  (let ((b1 (window-buffer w1))
        (b2 (window-buffer w2))
        (s1 (window-start w1))
        (s2 (window-start w2)))
    (set-window-buffer w1 b2)
    (set-window-buffer w2 b1)
    (set-window-start w1 s2)
    (set-window-start w2 s1)))

(defun swap-windows ()
  "If you have 2 windows or 2 frames, it swaps them."
  (interactive)
  (cond ((= (count-windows) 2)
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list))))
           (swap-buffers w1 w2)))
        ((= (length (frame-list)) 2)
         (let* ((w1 (first (window-list (first (frame-list)))))
                (w2 (first (window-list (second (frame-list))))))
           (swap-buffers w1 w2)))
        (t
         (message "You need exactly 2 windows or frames to do this."))))

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

(provide 'navigation-in-buffer)

;;; navigation-in-buffer.el ends here
