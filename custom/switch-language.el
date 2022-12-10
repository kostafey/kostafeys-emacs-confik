;;; switch-language.el -- Use cyrillic keys for commands input.

(when (eq system-type 'windows-nt)
  (require 'unicode-fonts)
  (unicode-fonts-setup))

;;------------------------------------------------------------------
;; emacs version 24 or later
;; ©juri_jurta, hakubo `http://ru-emacs.livejournal.com/82428.html'

(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (setq normal-local-function-key-map (copy-keymap local-function-key-map))
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(reverse-input-method "cyrillic-jcuken")

(defadvice read-passwd (around my-read-passwd act)
  (let ((local-function-key-map nil))
    ad-do-it))

;;====================================================================
;; Это не распространяется на последовательности клавиш, содержащие
;; буквы без модификаторов (такие как C-x b), но хоть
;; что-то. ©YuriKhan `http://www.emacswiki.org/emacs/GnuEmacsRussification'
;; (loop
;;   for from across "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖ\ЭЯЧСМИТЬБЮ№"
;;   for to   across "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>#"
;;   do
;;   (eval `(define-key key-translation-map
;;            (kbd ,(concat "C-" (string from))) (kbd ,(concat "C-" (string to)))))
;;   (eval `(define-key key-translation-map
;;            (kbd ,(concat "M-" (string from))) (kbd ,(concat "M-" (string to))))))

(provide 'switch-language)
