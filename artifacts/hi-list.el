;; hi-list.el --- Minor mode for automatically highlighting current list.

;; Copyright (C) 2010 sonota 

;; Author: sonota <yosiot8753@gmail.com>
;; Keywords: highlight

;;; Commentary:

;; ポイント位置が含まれるリストを常時ハイライトします。
;; Automatically highlighting current list.
;; Inspired by Impromptu (http://impromptu.moso.com.au/).

;;; Installation:

;; ; sample .emacs.el
;; (require 'hi-list-mode)
;; ; configure highlight bgcolor(recomend)
;; (set-face-background 'hi-list-face "#8aa")

;; ; If necessary
;; (add-hook 'emacs-lisp-mode-hook 'hi-list-mode)

;; ; リージョンが活性でない場合、ハイライト部分の開始位置、終了位置を引
;; ; 数にして指定されたコマンドを適用します。
;; ; Execute command with args START and END(endpoint of highlighted).
;; ; If region is not active, executed for region.
;; (define-key hi-list:map
;;   (kbd "C-w") (lambda () (interactive)
;;                 (apply-to-highlighted 'kill-region)))
;; (define-key hi-list:map
;;   (kbd "M-w") (lambda () (interactive)
;;                 (apply-to-highlighted 'kill-ring-save)))

;;; Usage:

;; M-x hi-list-mode RET: Toggle mode enable/disable.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customize

(defvar hi-list:lighter " hi-list")
(defvar hi-list:map)
(setq hi-list:map (make-sparse-keymap))

(defvar hi-list:limit-chars 2000
  "Do not highlight when number of chars included in a list is greater than this value.")
(defvar hi-list:repeat-sec 0.1)
(defvar hi-list:overlay nil)
(defvar hi-list:timer nil)
(copy-face 'highlight 'hi-list-face)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fundamental Functions

(defun in-string-or-doc-face-p ()
  "Return non-nil if face of current point is string or doc."
  (interactive)
  (let ((prop (get-text-property (point) 'face)))
    (if (listp prop)
        (if (or (member 'font-lock-string-face prop)
                (member 'font-lock-doc-face    prop))
            1
          nil)
      (if (or (equal 'font-lock-string-face prop)
              (equal 'font-lock-doc-face    prop))
          2
        nil))))

(defun search-string-beginning-backward ()
  "Move to beginning of string if point is in string or doc."
  (interactive)
  (let ((moved? nil))
    
    (while (in-string-or-doc-face-p)
      (forward-char -1)
      (setq moved? t))
    (if moved?
        (progn (forward-char 1)
               t)
      nil)))

(defun hi-list:disable-in-all-buffers-p ()
  (interactive)
  (not (member nil
          (mapcar
           (lambda (x)
             (with-current-buffer x
               (not hi-list-mode)))
           (buffer-list)))))

(defun apply-to-highlighted (proc)
  (cond ((and transient-mark-mode mark-active)
         (apply proc (list (region-beginning)
                           (region-end))))
        ((not (null hi-list:overlay))
         (save-excursion
           (let ((deactivate-mark nil))
             (apply proc
                    (list
                     (overlay-start hi-list:overlay)
                     (overlay-end   hi-list:overlay))))))
        (t nil)))

(defun hi-list ()
  "Highlight current list."
  (if hi-list:overlay
      (delete-overlay hi-list:overlay))
  (save-excursion
    (let ((beg nil))
      (search-string-beginning-backward)
      
      (backward-up-list)
      (setq beg (point))
      
      (forward-sexp)
      (if (< (- (point) beg) hi-list:limit-chars)
          (progn
            (setq hi-list:overlay
                  (make-overlay beg (point)))
            (overlay-put hi-list:overlay
                         'font-lock-face 'hi-list-face))))))

(defun hi-list:interval-run ()
  "Run by timer."
  (if hi-list-mode
      (hi-list)
    (hi-list-mode-stop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commands and Minor Mode Definitions

(defun hi-list-mode-start ()
  "Invoked when hi-list-mode is turned on."
  (if (not hi-list:timer)
      (setq hi-list:timer
            (run-with-idle-timer
             hi-list:repeat-sec
             t
             'hi-list:interval-run))))

(defun hi-list-mode-stop ()
  "Invoked when hi-list-mode is turned off."
  (if hi-list:overlay
      (delete-overlay hi-list:overlay))
  (if (hi-list:disable-in-all-buffers-p)
      (progn
        (cancel-timer hi-list:timer)
        (setq hi-list:timer nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode hi-list-mode
  "Automatically highlight current list."
  :lighter hi-list:lighter
  :keymap hi-list:map
  :require 'hi-list

  (if hi-list-mode
      (hi-list-mode-start)
    (hi-list-mode-stop)))

(provide 'hi-list)
;;; hi-list.el ends here
