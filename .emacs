;;=============================================================================
;;Человеку свойственно ошибаться, но для нечеловеческих ляпов нужен компьютер.
;;																	Пол Эрлих
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
(require 'hibernate-mapping)

;;-----------------------------------------------------------------------------
;(require 'russian-utf8-env)
;(set-language-environment "Russian UTF-8")

;;-----------------------------------------------------------------------------
;; nxhtml
;(load (concat site-lisp-path "nxhtml/autostart.el"))

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

(message "*************************")
(message "*** .emacs loaded OK. ***")
(message "*************************")
;;
;; end of .emacs
;;
;;


(global-set-key (kbd "M-t") 'transpose-words)
(global-set-key (kbd "M-y") '(lambda() (interactive) (transpose-words -1)))

;; Usage: Just enable highlight-parentheses-mode.
(require 'highlight-parentheses)

(require 'minimap)

(require 'icomplete+)

    (require 'window-number)
    (window-number-mode)

(global-set-key (kbd "C-/") 'repeat)

(global-unset-key [tab])
;;(global-set-key [tab] 'indent-relative-maybe)

(fset 'insert-as-string
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([34 end 92 110 34 43 home down] 0 "%d")) arg)))
(global-set-key (kbd "C-x C-l") 'insert-as-string)

;; (add-to-list 'load-path (concat site-lisp-path "skype/"))

;; (defun my-skype ()
;;   (interactive)
;;   (require 'skype)
;;   (setq skype--my-user-handle "Kostafey")
;;   (skype--init)
;;   (skype--open-all-users-buffer-command))
;; ()
;; (global-set-key "\M-(" 'insert-parentheses)

;; someday might want to rotate windows if more than 2 of them
(defun swap-windows ()
 "If you have 2 windows, it swaps them." (interactive) (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
 (t
 (let* ((w1 (first (window-list)))
	 (w2 (second (window-list)))
	 (b1 (window-buffer w1))
	 (b2 (window-buffer w2))
	 (s1 (window-start w1))
	 (s2 (window-start w2)))
 (set-window-buffer w1 b2)
 (set-window-buffer w2 b1)
 (set-window-start w1 s2)
 (set-window-start w2 s1)))))

(global-unset-key "\C-u")
(global-set-key "\C-u" 'swap-windows)

;; Or these two, which I consider serious omissions in the lineup of standard file-manipulation functions:

;;
;; Never understood why Emacs doesn't have this function.
;;
(defun rename-file-and-buffer (new-name)
 "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
 (let ((name (buffer-name))
	(filename (buffer-file-name)))
 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (if (get-buffer new-name)
	 (message "A buffer named '%s' already exists!" new-name)
	(progn 	 (rename-file name new-name 1) 	 (rename-buffer new-name) 	 (set-visited-file-name new-name) 	 (set-buffer-modified-p nil))))))

;;
;; Never understood why Emacs doesn't have this function, either.
;;
(defun move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
 (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	 (if (string-match dir "\\(?:/\\|\\\\)$")
	 (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))

 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (progn 	(copy-file filename newname 1) 	(delete-file filename) 	(set-visited-file-name newname) 	(set-buffer-modified-p nil) 	t)))) 
(put 'narrow-to-page 'disabled nil)
