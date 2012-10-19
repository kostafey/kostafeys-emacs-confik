;; emacswiki.org - GnuEmacsRussification

;; Следующий фрагмент позволяет переключать кодировки в буфере по
;; нажатию одной и той же комбинации клавиш.

;; AndreyBalaguta

;; Не знаю, как вам, а мне, например, не нравится эта связка
;; set-buffer-file-coding-system… revert-buffer. В 22-м есть функция
;; recode-region, которая делает именно то, что нужно (по правде
;; сказать, я ее нашел уже после того, как написал свои функции, но
;; в сущности внутри она делает точно то же). Чуть ниже есть две
;; функции – recode-buffer-dangerous и recode-buffer-safe. Обе
;; перекодируют текущий буфер в указанную кодировку, но первая
;; оперирует также и над read-only буферами, временно помечая их
;; writable и затем снимая флаг modified, вторая же оставляет
;; read-only буферы на откуп юзеру (может, лучше было бы создавать
;; буфер-копию read-only-буфера и перекодировать уже её?):

;; Да, если что-то пойдет не так во время encode..decode в read-only
;; буфере, с ним уже ничего не сделаешь, даже изменения отменить
;; нельзя, поэтому dangerous :-)

;;=============================================================================

(defun recode-buffer-dangerous (target-coding-system)
  "* Recode buffer as if it were encoded with `target-coding-system'.
If current buffer is write-protected (`buffer-read-only'), temporarily toggle
read-only flag, recode, then turn it back."
  (interactive "zEnter target coding system: ")
  (labels ((do-recode nil
		      (encode-coding-region (point-min)
					    (point-max)
					    buffer-file-coding-system)
		      (decode-coding-region (point-min)
					    (point-max)
					    target-coding-system)
		      (set-buffer-file-coding-system target-coding-system)))
    (if buffer-read-only
	(let ((buffer-read-only nil))
	  (do-recode)
	  (set-buffer-modified-p nil))
      (do-recode))))

(defun recode-buffer-safe (target-coding-system)
  "* Recode buffer as if it were encoded with `target-coding-system'.
If current buffer is write-protected (`buffer-read-only'), do nothing."
  (interactive "zEnter target coding system: ")
  (unless buffer-read-only
    (encode-coding-region (point-min)
			  (point-max)
			  buffer-file-coding-system)
    (decode-coding-region (point-min)
			  (point-max)
			  target-coding-system)
    (set-buffer-file-coding-system target-coding-system)))

(setq evm-coding-systems-list (make-ring 10))
(ring-insert evm-coding-systems-list 'koi8-r)
(ring-insert evm-coding-systems-list 'alternativnyj)
(ring-insert evm-coding-systems-list 'iso-8859-5)
(ring-insert evm-coding-systems-list 'windows-1251)
(ring-insert evm-coding-systems-list 'mule-utf-8)

(defun recode-buffer-rotate-ring ()
  "Circle changes coding system in the current buffer.
Encoding changes through rotation of `evm-coding-systems-list'."
  (interactive)
  (let* ((keys (recent-keys))
         (len (length keys))
         (key1 (if (> len 0) (elt keys (- len 1)) nil))
         (key2 (if (> len 1) (elt keys (- len 2)) nil))
         cs)
    (if (eq key1 key2)
        (setcar evm-coding-systems-list
                (ring-plus1 (car evm-coding-systems-list)
                            (ring-length evm-coding-systems-list)))
      (setcar evm-coding-systems-list 0))
    (set-buffer-multibyte t)
    (recode-buffer-dangerous (aref (cddr evm-coding-systems-list)
                                   (car evm-coding-systems-list)))))

;;=============================================================================

(provide 'reencoding-file)

