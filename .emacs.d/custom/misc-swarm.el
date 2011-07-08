;; for XWindow
;; (defun djcb-full-screen-toggle ()
;;   "toggle full-screen mode"
;;   (interactive)
;;   (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))


;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph       
;;; Takes a multi-line paragraph and makes it into a single line of text.       
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(global-set-key (kbd "C-c q")  'unfill-paragraph)


(require 'lusty-explorer)


(require 'vel)
(setq-default vel-mode t)


(require 'cc-subword)
(add-hook 'c-mode-common-hook
	  (lambda () (c-subword-mode 1)))

;; (require 'column-marker)
;; (add-hook 'fundamental-mode 
;;           (lambda () (interactive) (column-marker-1 fill-column)))
;; (column-marker-1 fill-column)


(quail-define-package
 "cyrillic-jcuken" "Cyrillic" "RU" nil
  "ЙЦУКЕH keyboard layout widely used in Russia (ISO 8859-5 encoding)"
  nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("1" ?1) ("2" ?2) ("3" ?3) ("4" ?4) ("5" ?5) ("6" ?6) ("7" ?7) ("8" ?8)
 ("9" ?9) ("0" ?0) ("-" ?-) ("=" ?=) ("`" ?ё) ("q" ?й) ("w" ?ц) ("e" ?у)
 ("r" ?к) ("t" ?е) ("y" ?н) ("u" ?г) ("i" ?ш) ("o" ?щ) ("p" ?з) ("[" ?х)
 ("]" ?ъ) ("a" ?ф) ("s" ?ы) ("d" ?в) ("f" ?а) ("g" ?п) ("h" ?р) ("j" ?о)
 ("k" ?л) ("l" ?д) (";" ?ж) ("'" ?э) ("\\" ?\\) ("z" ?я) ("x" ?ч) ("c" ?с)
 ("v" ?м) ("b" ?и) ("n" ?т) ("m" ?ь) ("," ?б) ("." ?ю) ("/" ?.) ("!" ?!)
 ("@" ?\") ("#" ?#) ("$" ?\;) ("%" ?%) ("^" ?:) ("&" ??) ("*" ?*) ("(" ?() 
 (")" ?)) ("_" ?_) ("+" ?+) ("~" ?Ё)
 ("Q" ?Й) ("W" ?Ц) ("E" ?У) ("R" ?К) ("T" ?Е) ("Y" ?Н) ("U" ?Г) ("I" ?Ш)
 ("O" ?Щ) ("P" ?З) ("{" ?Х) ("}" ?Ъ) ("A" ?Ф) ("S" ?Ы) ("D" ?В) ("F" ?А)
 ("G" ?П) ("H" ?Р) ("J" ?О) ("K" ?Л) ("L" ?Д) (":" ?Ж) ("\"" ?Э) ("|" ?/)
 ("Z" ?Я) ("X" ?Ч) ("C" ?С) ("V" ?М) ("B" ?И) ("N" ?Т) ("M" ?Ь) ("<" ?Б)
 (">" ?Ю) ("?" ?,))

;; (setq default-input-method "cyrillic-jcuken")
;; (set-input-method 'cyrillic-jcuken)


;; RECODE ENGLISH TO RUSSIAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar u:*en/ru-table*
     '((?q  . ?й) (?w  . ?ц) (?e  . ?у)
       (?r  . ?к) (?t  . ?е) (?y  . ?н) (?u  . ?г)
       (?i  . ?ш) (?o  . ?щ) (?p  . ?з) (?[  . ?х)
       (?]  . ?ъ) (?a  . ?ф) (?s  . ?ы) (?d  . ?в)
       (?f  . ?а) (?g  . ?п) (?h  . ?р) (?j  . ?о)
       (?k  . ?л) (?l  . ?д) (?\; . ?ж) (?\' . ?э)
       (?z  . ?я) (?x  . ?ч) (?c  . ?с) (?v  . ?м)
       (?b  . ?и) (?n  . ?т) (?m  . ?ь) (?,  . ?б)
       (?.  . ?ю) (?/  . ?.) (?!  . ?!) (?@  . ?\")
       (?#  . ?№) (?$  . ?\;) (?%  . ?%) (?^  . ?:)
       (?&  . ??) (?*  . ?*) (?Q  . ?Й) (?W  . ?Ц)
       (?E  . ?У) (?R  . ?К) (?T  . ?Е) (?Y  . ?Н)
       (?U  . ?Г) (?I  . ?Ш) (?O  . ?Щ) (?P  . ?З)
       (?{  . ?Х) (?}  . ?Ъ) (?A  . ?Ф)
       (?S  . ?Ы) (?D  . ?В) (?F  . ?А) (?G  . ?П)
       (?H  . ?Р) (?J  . ?О) (?K  . ?Л) (?L  . ?Д)
       (?:  . ?Ж) (?\" . ?Э) (?Z  . ?Я) (?X  . ?Ч)
       (?C  . ?С) (?V  . ?М) (?B  . ?И) (?N  . ?Т)
       (?M  . ?Ь) (?<  . ?Б) (?>  . ?Ю) (?\? . ?,)))

;;------------------------------------------------------------------------------ 
(defun u:en/ru-recode-region (beg end &optional arg)
  "Recode the given region, that contains Russain text typed in English, into Russian.
With ARG recode from Russian o English."

  (interactive "*r\nP")
  (save-excursion
    (goto-char beg)
    (do () ((>= (point) end))
      (let* ((en-char (char-after (point)))
             (ru-char (if arg 
                          (car (rassoc en-char u:*en/ru-table*))
                        (cdr (assoc en-char u:*en/ru-table*)))))
        (delete-char 1)
        (insert (if ru-char ru-char en-char))))))
(global-set-key (kbd "C-`") 'u:en/ru-recode-region)

(add-to-list 'load-path "~/.emacs.d/google-weather/")
(require 'google-weather)
(require 'org-google-weather)
;; * Weather
;;   :PROPERTIES: ...
;;   %%(org-google-weather "New York" "en-gb")

(add-to-list 'load-path "~/.emacs.d/org-7.01h/lisp/")
(require 'org)




;(add-to-list 'default-frame-alist '(alpha . (0.90 0.85)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; el-get configuration
;;
(add-to-list 'load-path (concat site-lisp-path "el-get/el-get/"))

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

;; local sources
;; (setq el-get-sources
;;       '((:name ropemacs)
;;         (:name pymacs)))

;; (el-get 'sync)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-swarm)

