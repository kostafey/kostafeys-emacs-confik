;;=============================================================================
;;Человеку свойственно ошибаться, но для нечеловеческих ляпов нужен компьютер.
;;                                                                  Пол Эрлих
;;
;;Kostafey's emacs confik :) 
;;started from 08.08.2008, 22:45:25

;;=============================================================================
;;Указываем где будут лежать файлы расширений
;;=============================================================================
(defvar site-lisp-path "~/.emacs.d/")
(add-to-list 'load-path site-lisp-path)

;;-----------------------------------------------------------------------------
;auto-customized custom-set-variables
(setq custom-file (concat site-lisp-path "custom/custom.el")) 
(load custom-file)

;;-----------------------------------------------------------------------------
(add-to-list 'load-path (concat site-lisp-path "custom/"))
(require 'look-and-feel)
(require 'basic-text-editing)
(require 'navigation-and-simplify-keys)
(require 'buffer-navigation)
(require 'ispell-conf)
;; (require 'maxima-conf)
(require 'auctex-conf)
(require 'ide)
(require 'completition-conf)
(require 'communications)
(require 'scheme-conf)
(require 'reencoding-file)
(require 'haskell-conf)

;;-----------------------------------------------------------------------------
(add-to-list 'load-path (concat site-lisp-path "my-task-centric/"))
(require 'calc-time)
;(require 'hibernate-mapping)

;;-----------------------------------------------------------------------------
;(require 'russian-utf8-env)
;(set-language-environment "Russian UTF-8")

;;-----------------------------------------------------------------------------
;; nxhtml
;; (load (concat site-lisp-path "nxhtml/autostart.el"))

;;-----------------------------------------------------------------------------
;; html-изация
(require 'htmlize)
(setq htmlize-output-type (quote css))

;;-----------------------------------------------------------------------------
; log4j-mode
(autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))

;;-----------------------------------------------------------------------------
;; Org-mode settings
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;введенный пароль не будет кешироваться
(setq epa-file-cache-passphrase-for-symmetric-encryption nil)

(setq org-hide-leading-stars t)
;;-----------------------------------------------------------------------------
;; session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
;Сохранять сессию перед выходом
(desktop-save-mode t)
;к сохраняемым данным добавляет еще и кодировку с которой использовался буфер
(add-to-list 'desktop-locals-to-save 'buffer-file-coding-system)

;;-----------------------------------------------------------------------------
; ELScreen
(add-to-list 'load-path (concat site-lisp-path "apel-10.7/"))
(require 'alist)
;(load "elscreen" "ElScreen" t)
(setq elscreen-prefix-key "\C-t")
(require 'elscreen)
(elscreen-create)
(elscreen-toggle-display-tab)

;;-----------------------------------------------------------------------------
;; JavaScript IDE
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;=============================================================================
;; misc
;;
(setq max-specpdl-size 5000) ;for byte-compile

;Очистка SQL
;"\|\(\\n\)\|\+
;;=============================================================================
;;
;;

(global-set-key [f7] 'auto-complete-mode)

(require 'goto-last-change)
(global-set-key "\C-xx" 'goto-last-change)

(global-set-key (kbd "C-x r") 'revert-buffer)

(global-unset-key (kbd "M-r"))
(global-set-key (kbd "M-r") 'replace-string)

(require 'pager)
;;; Bind scrolling functions from pager library.
(global-set-key [next] 	   'pager-page-down)
(global-set-key [prior]	   'pager-page-up)
;; (global-set-key '[M-up]    'pager-row-up)
;; (global-set-key '[M-down]  'pager-row-down)

;; (global-set-key '[C-tab] 'bs-cycle-next)
;; (global-set-key [S-tab] 'bs-cycle-previous) 

(put 'narrow-to-page 'disabled nil)

(message "*************************")
(message "*** .emacs loaded OK. ***")
(message "*************************")
;;
;; end of .emacs
;;
;;

;; (defun djcb-full-screen-toggle ()
;;   "toggle full-screen mode"
;;   (interactive)
;;   (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

(add-hook 'fundamental-mode-hook
               (lambda ()
                (font-lock-add-keywords nil
                 '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

(defun djcb-zoom (n)
  "with positive N, increase the font size, otherwise decrease it"
  (set-face-attribute 'default (selected-frame) :height 
    (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 10))))


(global-set-key (kbd "C-+")      '(lambda nil (interactive) (djcb-zoom 1)))
(global-set-key [C-kp-add]       '(lambda nil (interactive) (djcb-zoom 1)))
(global-set-key (kbd "C--")      '(lambda nil (interactive) (djcb-zoom -1)))
(global-set-key [C-kp-subtract]  '(lambda nil (interactive) (djcb-zoom -1)))

;;;Handy MACROS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  insert current date into the buffer at point  
(defun insert-date()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

;; Emacs does not beep when you hit `C-g' in the minibuffer or during
;; an `isearch' (http://www.emacswiki.org/cgi-bin/wiki.pl?AlarmBell)
;; (setq ring-bell-function 
;;       (lambda ()
;; 	(unless (memq this-command
;; 		      '(isearch-abort abort-recursive-edit find-file
;; 				      exit-minibuffer keyboard-quit))
;; 	  (ding))))

;;;turn off the bell http://www.emacswiki.org/cgi-bin/wiki?AlarmBell
;; (setq ring-bell-function 'ignore)

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph       
;;; Takes a multi-line paragraph and makes it into a single line of text.       
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(global-set-key (kbd "C-c q")  'unfill-paragraph)

;;; In praise of Emacs, The One True Editor
;;; 1.0 Aug 19, 1994
;;; 2.0 Aug 28, 1994
;;; Rather ecclesiastical, though.
;;; -elf
(defun praise-emacs()
  "In praise of emacs"
  (interactive)
  (message "Praise Emacs...")
  (sit-for 2)
  (message "Amen."))

(require 'rainbow-mode)
(require 'lusty-explorer)

(global-set-key (kbd "C-?") 'describe-char)

(require 'vel)
(setq-default vel-mode t)

;; (require 'hi-list)
;; (set-face-background 'hi-list-face "#E3F2A1")
;; (add-hook 'emacs-lisp-mode-hook 'hi-list-mode)

(require 'highlight-parentheses)
(add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
(setq hl-paren-colors '("#326B6B"))
(setq hl-paren-background-colors '(
      "#00FF99" "#CCFF99" "#FFCC99" "#FF9999" "#FF99CC" 
      "#CC99FF" "#9999FF" "#99CCFF" "#99FFCC" "#7FFF00"))
;; (setq hl-paren-colors '
;;       ("#326B6B" "#66CC66" "#73CD4F" "#32CD32"
;;        "#6495ED" "#9E9B29" "#32227B" "#226B4B"))
;; (setq hl-paren-background-colors '(
;;       "#00FF99" "#CCFF99" "#FFCC99" "#FF9999" 
;;       "#FF99CC" "#CC99FF" "#9999FF" "#99CCFF" 
;;       "#99FFCC" "#7FFF00" "#73CDF4" "#DDEE00"))

;; to have a menu of recently opened files
(recentf-mode 1)

(require 'cc-subword)
(add-hook 'c-mode-common-hook
	  (lambda () (c-subword-mode 1)))

;; (require 'column-marker)
;; (add-hook 'fundamental-mode 
;;           (lambda () (interactive) (column-marker-1 fill-column)))
;; (column-marker-1 fill-column)

;; do not truncate and wrap long lines
(setq truncate-partial-width-windows nil)
(setq truncate-lines nil)
;; and move up down end begin over the real visible screen lines
(require 'physical-line)
(global-set-key [(up)] 'physical-line-previous-line)
(global-set-key [(down)] 'physical-line-next-line)
(physical-line-mode 1)
(global-set-key [(end)] 'end-of-line)
(global-set-key [(home)] 'beginning-of-line)

;; Mode for automatic saving of minibuffer history.
(savehist-mode 1)

;; (w32-wh-keyboard-ll nil)

;; (if (eq system-type 'windows-nt)
;;     (progn 
;;       (defvar safe-language-change-flag nil)
;;       (defun safe-language-change ()
;;         (interactive)
;;         (setq safe-language-change-flag (not safe-language-change-flag))
;;         (when safe-language-change-flag
;;           (toggle-input-method)
;;           ;; (w32-toggle-lock-key 'capslock)
;;           ;; (w32-toggle-lock-key '(meta shift))

;;           ))
;;       (global-set-key (kbd "<language-change>") 'safe-language-change)
;;       ;; (global-set-key [(meta shift)] 'safe-language-change))
;;   (global-set-key [f13] 'toggle-input-method)))

;; (global-set-key (kbd "<language-change>") '(w32-toggle-lock-key 'capslock))


;; (global-set-key (kbd "<language-change>") '(lambda () (w32-toggle-lock-key 'scroll)))
;; (global-set-key [(meta shift)] '(w32-toggle-lock-key 'scroll))


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



;; вапвапвап sdfsdf

;; (w32-toggle-lock-key 'capslock)
;; (w32-toggle-lock-key 'scroll)

;; (global-set-key (kbd "<language-change>") '"htt")
;; (global-unset-key (kbd "<language-change>"))


 ;; (global-unset-key [(meta shift)])htt
;; (global-set-key [(meta shift)] '(w32-toggle-lock-key 'capslock))
;; (w32-toggle-lock-key 'capslock 1)
;; (if (eq system-type 'windows-nt)
;;     (progn
;;       (defun safe-language-change ()
;;         (interactive)
;;         (w32-toggle-lock-key '(kbd "<language-change>")
;;         (toggle-input-method))
;;       (global-set-key (kbd "<language-change>") 'safe-language-change)
;;       (global-set-key [f13] 'toggle-input-method))))



;; (w32-unregister-hot-key (kbd "<language-change>"))
;; (w32-unregister-hot-key '[(meta shift)])

;; (w32-send-sys-command #xF080)

;; (w32-meta-set-w32-style t nil)
;; (w32-meta-set-emacs-style)

;; (setq w32-alt-is-meta t)
;; (w32-register-hot-key [A-tab])
;; (w32-register-hot-key (kbd "<language-change>"))

;; (w32-send-sys-command #xF050)

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

(require 'browse-kill-ring)

;; show ascii table
;; optained from http://www.chrislott.org/geek/emacs/dotemacs.html
(defun ascii-table ()
  "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>"
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))
  (beginning-of-buffer))

;; battery mode:
(require 'battery)
(setq battery−mode−line−format " [%L %p%% %dC]")
(display-battery-mode)
