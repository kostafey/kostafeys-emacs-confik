;;; aj-compilation.el -- Hides compilation buffer's window of no errors occurs.

;; Author: Anton Johansson
;; URL: `https://github.com/antonj/.emacs.d/blob/master/aj-compilation.el'
;; Patched by kostafey.

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software Foundation,
;;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.  */

;; Keywords: Compilation mode, Compilation

(setq compilation-scroll-output t)
(setq compilation-window-height 17)

(setq compilation-ask-about-save nil)
(setq compilation-save-buffers-predicate '(lambda () nil))

(defvar aj-exceptions-list (list "find" "perl")
  "Exception commands for don't hide compilation window.")

(defvar aj-compilation-saved-window-configuration nil
  "Previous window conf from before a compilation")

(defvar aj-compile-command ""
  "The compile command used by compilation-start since
  `compile-command' is only saved by `compile' command.")

;; Hide *compilation* buffer if compile didn't give erros
(defadvice compilation-start (before aj-compilation-save-window-configuration(command comint))
  "Save window configuration before compilation in
`aj-compilation-saved-window-configuration'"

  ;; compile command is not saved in compilation-start function only in
  ;; compile function (rgrep only uses compilation-start)
  (setq aj-compile-command command)
  ;; Save window configuration
  (setq aj-compilation-saved-window-configuration
        (current-window-configuration)))
(ad-activate 'compilation-start)

(defun aj-is-exception (ex-list aj-compile-command)
  "Search through exceptions list `ex-list'.
Used to decide whether or not hide compilation window."
  (if (> (length ex-list) 0)
      (let* ((ex-position (string-match (car ex-list) aj-compile-command))
             ;; Not nil and not 0 means that command was "find" at
             ;; pos 0 which means that I don't want to restore the
             ;; layout
             (is-exception (and (integerp ex-position) (zerop ex-position))))
        (if is-exception 
            t
          (aj-is-exception (cdr ex-list) aj-compile-command)))
    nil))

;; compilation-handle-exit returns (run-hook-with-args
;; 'compilation-finish-functions cur-buffer msg) Could use but it only
;; got a string describing status
(defadvice compilation-handle-exit
  (after aj-compilation-exit-function(process-status exit-status msg))
  "Hack to restore window conf"
    (when (and (eq process-status 'exit)
               (zerop exit-status)
               (not (aj-is-exception aj-exceptions-list aj-compile-command)))
      (set-window-configuration aj-compilation-saved-window-configuration)))
(ad-activate 'compilation-handle-exit)

(provide 'aj-compilation)
