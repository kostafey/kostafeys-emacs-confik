;; for XWindow
;; (defun djcb-full-screen-toggle ()
;;   "toggle full-screen mode"
;;   (interactive)
;;   (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

(require 'lusty-explorer)

;; (require 'vel)
;; (setq-default vel-mode t)
;; (define-key vel-mode-map-default
;;   (kbd "<C-c-down>") 'vel-doscroll-enter-and-key-down)
;; (define-key vel-mode-map-default
;;   (kbd "<C-c-up>") 'vel-doscroll-enter-and-key-up)

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


;;------------------------------------------------------------------------------ 

(add-to-list 'load-path "~/.emacs.d/google-weather/")
(require 'google-weather)
(require 'org-google-weather)
;; * Weather
;;   :PROPERTIES: ...
;;   %%(org-google-weather "New York" "en-gb")


;(add-to-list 'default-frame-alist '(alpha . (0.90 0.85)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'sql)
;; (setq sql-server "192.168.181.60")
;; (setq sql-database "july")
;; (setq sql-password "K9$K7dt")
;; (setq sql-user "informix")
;; (setq sql-port "1541")

(add-to-list 'load-path (concat site-lisp-path "ejsql/"))
(require 'ejsql)

(global-set-key 
 (kbd "C-c h")  
 '(lambda() (interactive)
    (progn
      (save-buffer)
      (shell-command 
       "sphinx-build -b html C:/myworkspaces/doc/ C:/myworkspaces/doc/build-html/")
      (delete-other-windows))))


(defun shrink-whitespaces ()
  "Remove white spaces around cursor to just one or none.
If current line does not contain non-white space chars, then remove blank lines to just one.
If current line contains non-white space chars, then shrink any whitespace char surrounding cursor to just one space.
If current line is a single space, remove that space.

Calling this command 3 times will always result in no whitespaces around cursor."
  (interactive)
  (let (
        cursor-point
        line-has-meat-p  ; current line contains non-white space chars
        spaceTabNeighbor-p
        whitespace-begin whitespace-end
        space-or-tab-begin space-or-tab-end
        line-begin-pos line-end-pos
        )
    (save-excursion
      ;; todo: might consider whitespace as defined by syntax table, and also consider whitespace chars in unicode if syntax table doesn't already considered it.
      (setq cursor-point (point))

      (setq spaceTabNeighbor-p (if (or (looking-at " \\|\t") (looking-back " \\|\t")) t nil) )
      (move-beginning-of-line 1) (setq line-begin-pos (point) )
      (move-end-of-line 1) (setq line-end-pos (point) )
      ;;       (re-search-backward "\n$") (setq line-begin-pos (point) )
      ;;       (re-search-forward "\n$") (setq line-end-pos (point) )
      (setq line-has-meat-p (if (< 0 (count-matches "[[:graph:]]" line-begin-pos line-end-pos)) t nil) )
      (goto-char cursor-point)

      (skip-chars-backward "\t ")
      (setq space-or-tab-begin (point))

      (skip-chars-backward "\t \n")
      (setq whitespace-begin (point))

      (goto-char cursor-point)      (skip-chars-forward "\t ")
      (setq space-or-tab-end (point))
      (skip-chars-forward "\t \n")
      (setq whitespace-end (point))
      )

    (if line-has-meat-p
        (let (deleted-text)
          (when spaceTabNeighbor-p
            ;; remove all whitespaces in the range
            (setq deleted-text (delete-and-extract-region space-or-tab-begin space-or-tab-end))
            ;; insert a whitespace only if we have removed something
            ;; different that a simple whitespace
            (if (not (string= deleted-text " "))
                (insert " ") ) ) )

      (progn
        ;; (delete-region whitespace-begin whitespace-end)
        ;; (insert "\n")
        (delete-blank-lines)
        )
      ;; todo: possibly code my own delete-blank-lines here for better efficiency, because delete-blank-lines seems complex.
      )
    )
  )

(defun compact-uncompact-block ()
  "Remove or add line ending chars on current paragraph.
This command is similar to a toggle of `fill-paragraph'.
When there is a text selection, act on the region."
  (interactive)

  ;; This command symbol has a property “'stateIsCompact-p”.
  (let (currentStateIsCompact (bigFillColumnVal 4333999) (deactivate-mark nil))

    (save-excursion
      ;; Determine whether the text is currently compact.
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil) ) )

      (if (region-active-p)
          (if currentStateIsCompact
              (fill-region (region-beginning) (region-end))
            (let ((fill-column bigFillColumnVal))
              (fill-region (region-beginning) (region-end))) )
        (if currentStateIsCompact
            (fill-paragraph nil)
          (let ((fill-column bigFillColumnVal))
            (fill-paragraph nil)) ) )

      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)) ) ) )

(defun shrink-whitespaces-at-current-paragraph () 
  (interactive) 
  (destructuring-bind (start . end) (bounds-of-thing-at-point 'paragraph) 
    (replace-regexp "[ \t]+" " " nil start end) 
    (indent-region start end)))


(provide 'misc-swarm)

