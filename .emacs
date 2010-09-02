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
(require 'maxima-conf)
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
(load (concat site-lisp-path "nxhtml/autostart.el"))

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

;;; erc is the emacs irc chat client-- waste time productively
(setq erc-server "irc.freenode.net"
	  erc-port 6667
	  erc-nick "Kostafey"
	  erc-user-full-name "Kostafey"
	  erc-public-away-p t;lets ppl know how long you were away
	  erc-prompt-for-password nil)

;; automatically join channels when we start-up
(require 'erc-autojoin) (erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
	  '(("freenode.net" "#emacs")))


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

(require 'hi-list)
(set-face-background 'hi-list-face "#E3F2A1")
(add-hook 'emacs-lisp-mode-hook 'hi-list-mode)

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

