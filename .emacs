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
;;-----------------------------------------------------------------------------
(require 'russian-utf8-env)
(set-language-environment "Russian UTF-8")
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
;;=============================================================================
;;
;;
(message "*************************")
(message "*** .emacs loaded OK. ***")
(message "*************************")
;;
;; end of .emacs
;;
;;