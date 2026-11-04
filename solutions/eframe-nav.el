;;; eframe-nav.el --- Buffer and multi-frame navigation helpers  -*- lexical-binding: t; -*-

;; Helpers for moving between buffers and across frames when Emacs runs
;; with several frames (for example one frame per monitor).

(require 'cl-lib)

;; Optional integration with hopper.el (see `eframe-kill-buffer').
(defvar hop-arrived-via-hop)
(declare-function hop-backward "hopper")

(defgroup eframe-nav nil
  "Buffer and multi-frame navigation helpers."
  :group 'convenience)

(defcustom eframe-omit-buffers-patterns (list)
  "List of buffer name patterns to skip.
Any time `eframe-next-buffer' or `eframe-previous-buffer' is called
these buffers are skipped."
  :type '(repeat string)
  :group 'eframe-nav)

(defun eframe-pop-emacs ()
  (interactive)
  (previous-multiframe-window))

(defun eframe-omit-buffer-p ()
  (cl-some (lambda (pattern) (cl-search pattern (buffer-name)))
           eframe-omit-buffers-patterns))

(defun eframe-next-buffer ()
  (interactive)
  (next-buffer)
  (when (eframe-omit-buffer-p)
    (next-buffer)))

(defun eframe-previous-buffer ()
  (interactive)
  (previous-buffer)
  (when (eframe-omit-buffer-p)
    (previous-buffer)))

(defun eframe-kill-buffer ()
  "Kill current buffer.
If the buffer was entered via `hop-at-point' (see hopper.el), return to
the previous position with `hop-backward' after killing it instead of
switching to the previous buffer."
  (interactive)
  (if (and (bound-and-true-p hop-arrived-via-hop)
           (fboundp 'hop-backward))
      (progn
        (kill-buffer (current-buffer))
        (hop-backward))
    (kill-buffer (current-buffer))
    (previous-buffer)
    (when (eframe-omit-buffer-p)
      (previous-buffer))))

(defun eframe-pop-buffer (mode)
  "Find first buffer with MODE major-mode and set focus or display it."
  (let ((result-buffer nil))
    (dolist (buff (buffer-list))
      (with-current-buffer buff
        (when (eq major-mode mode)
          (setq result-buffer buff))))
    (when result-buffer
      (let ((win (get-buffer-window result-buffer t)))
        (if win
            (progn
              (select-frame-set-input-focus (window-frame win))
              (set-frame-selected-window (window-frame win) win))
          (pop-to-buffer result-buffer))))
    result-buffer))

;;-------------------------------------------------------------------
;; Multi-monitor windmove: when there is no window in the requested
;; direction but another frame exists, hop to the neighbouring frame.
;;
(defun eframe-window-on-other-frame (direction)
  "Return the nearest window on another top-level frame, or nil.
DIRECTION `right' looks in `next-window' order, `left' in the reverse one.
Only frames `next-window' visits count: hidden ones, like the child frame
corfu keeps for its popup, are in `frame-list' but no window leads to them."
  (let ((windows (cdr (window-list-1 nil nil 0))))
    (seq-find (lambda (window)
                (let ((frame (window-frame window)))
                  (not (or (eq frame (selected-frame))
                           (frame-parent frame)))))
              (if (eq direction 'right) windows (reverse windows)))))

(defun eframe-windmove-do-window-select (orig-fun &rest args)
  (let* ((direction (car args))
         (target (and (memq direction '(right left))
                      (null (apply 'windmove-find-other-window
                                   (seq-take args 3)))
                      (eframe-window-on-other-frame direction))))
    (if target
        (progn
          (select-window target)
          (select-frame-set-input-focus (window-frame target)))
      (apply orig-fun args))))

(advice-add 'windmove-do-window-select
            :around #'eframe-windmove-do-window-select)

(provide 'eframe-nav)
