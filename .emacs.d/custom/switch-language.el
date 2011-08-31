;;; switch-language.el -- Switches keyboard input method in Emacs, using OS keybindings.

;;; Copyright © 2010 - Kostafey <kostafey@gmail.com>

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

;;; Commentary:

;; Realized just for Windows Nt and EmacsW32 (partially for Linux with KDE).
;; (global-set-key [(meta shift)] 'toggle-input-method) - is dream only? ;)

;;=============================================================================
(if (eq system-type 'windows-nt)
    (progn
      (defvar lswitch-process-name "lswitch-process")
      (defvar lswitch-program-name "lswitch")
      (defun ensure-start-lswitch-process ()
        "Searches `lswitch-program-name' program in the PATH and starts it. 
It's bind to the `lswitch-process-name' process, which is not require
the confirm to be killed." 
        (if (executable-find lswitch-program-name)
            (if (not (get-process lswitch-process-name))
                (progn
                  ;; capslock - 20
                  ;; scroll - 145
                  ;; quit - q
                  (start-process lswitch-process-name nil 
                                 lswitch-program-name "145")
                  (if (get-process lswitch-process-name)
                      (process-kill-without-query
                       (get-process lswitch-process-name) t))))
          (message "`%s' is not found in the PATH" lswitch-program-name)))
      (ensure-start-lswitch-process)

      (defvar safe-language-change-flag nil)
      (defvar inner-change-permit t)
      (defun safe-language-change-revert ()
        "Actually toggles language input in the both OS and Emacs. 
Then revert back the OS input language." 
        (interactive)
        (setq safe-language-change-flag (not safe-language-change-flag))
        (when (and safe-language-change-flag inner-change-permit)
          (ensure-start-lswitch-process)
          (toggle-input-method)
          (w32-toggle-lock-key 'scroll)))
      (defun toggle-emacs-os-switching ()
        "Enable/disable `safe-language-change-revert' function's normal working." 
        (interactive)
        (setq inner-change-permit (not inner-change-permit))
        (if inner-change-permit
            (message "Emacs toggle input method")
          (message "OS toggle input method")))

      (global-set-key (kbd "C-\\") 'toggle-input-method)
      (global-set-key (kbd "<language-change>") 'safe-language-change-revert)
      (global-set-key [(control lwindow)] 'toggle-emacs-os-switching)))

;;=============================================================================
(if (eq system-type 'gnu/linux)
    ;; meta-shift-z
    (progn
      (defun toggle-to-english()
        (interactive)
        (shell-command "qdbus org.kde.kxkb /kxkb org.kde.KXKB.setLayout us"))
      (defun safe-language-change ()
        (interactive)
        (toggle-to-english)
        (toggle-input-method))

      (global-set-key (kbd "M-Z") 'safe-language-change))) ; [(meta shift z)]


;; (global-unset-key [f10])
;; (global-unset-key [f31])
;; (global-unset-key [f32])
;; (global-unset-key [f33])

;; (global-set-key [f31 f32]  'toggle-input-method)

;; (global-set-key [meta shift] '(lambda() (interactive) (message "done!")))

;; (global-set-key [f31]  'toggle-input-method)

;; (global-set-key (kbd "M-<f32>") 'toggle-input-method)
;; (global-set-key (kbd "<f31>") 'toggle-input-method)
(global-set-key (kbd "<f31>") '(lambda() (interactive) (message "done!")))
(global-set-key (kbd "<f32>") '(lambda() (interactive) (message "done!")))
(global-set-key (kbd "<f33>") '(lambda() (interactive) (message "done!")))
;; (global-set-key (kbd "<f32>") 'toggle-input-method)

;; (global-set-key [(f31)] '(lambda() (interactive) (message "done!")))

;;=============================================================================
;; Это не распространяется на последовательности клавиш, содержащие
;; буквы без модификаторов (такие как C-x b), но хоть
;; что-то. ©YuriKhan http://www.emacswiki.org/emacs/GnuEmacsRussification
(loop
  for from across "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖ\ЭЯЧСМИТЬБЮ№"
  for to   across "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"
  do
  (eval `(define-key key-translation-map 
           (kbd ,(concat "C-" (string from))) (kbd ,(concat "C-" (string to)))))
  (eval `(define-key key-translation-map 
           (kbd ,(concat "M-" (string from))) (kbd ,(concat "M-" (string to))))))

(provide 'switch-language)

;; (global-set-key (kbd "M-Z") '(lambda() (interactive) (toggle-input-method) (toggle-to-english)))
;; (global-set-key [(control lwindow)] '(lambda() (interactive) (message "done!")))
;; (global-set-key (kbd "M-Z") '(lambda() (interactive) (message "done!")))
;; (global-set-key [(meta shift)] '(lambda() (interactive) (message "done!")))
;; (global-set-key (kbd "C-q") '(lambda() (interactive) (message "done!")))
;; (global-set-key (kbd "M-Z") '(lambda() (interactive) (message "done!")))
;; (global-set-key (kbd "<language-change>") '(lambda() (interactive) (message "done!")))

;; (global-set-key (kbd "<change-keyboard-layout>") '(lambda() (interactive) (message "done!")))
