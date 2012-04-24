;;=============================================================================
;;Человеку свойственно ошибаться, но для нечеловеческих ляпов нужен компьютер.
;;                                                                  Пол Эрлих
;;
;;Kostafey's emacs confik :) 
;;started from 08.08.2008, 22:45:25
;;
;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;; (_)___|_| |_| |_|\__,_|\___|___/
;;

;;=============================================================================
;; Указываем где будут лежать файлы расширений
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
(require 'switch-language)
(require 'ispell-conf)
(require 'completition-conf)
(require 'communications)
(require 'reencoding-file)

(require 'ide)
;; (require 'maxima-conf)
;; (require 'haskell-conf)
(require 'java-conf)
(require 'scheme-conf)
(require 'auctex-conf)
;; (require 'python-conf)

;;-----------------------------------------------------------------------------
(add-to-list 'load-path (concat site-lisp-path "my-task-centric/"))
(require 'calc-time)
(require 'hibernate-mapping)

(global-set-key [f7] 'auto-complete-mode)

;;-----------------------------------------------------------------------------
;; History
;; To have a menu of recently opened files
(recentf-mode 1)

;; Mode for automatic saving of minibuffer history.
(savehist-mode 1)

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
;; reStructuredText settings
;; .. -*- mode: rst -*-
(add-hook 'rst-adjust-hook 'rst-toc-update)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))

;;-----------------------------------------------------------------------------
;; Org-mode settings
(add-to-list 'load-path (concat site-lisp-path "org-7.6/lisp/"))
(require 'org-install)

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
;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

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
;; in Emacs, M-x byte-compile-file RE js2.el RET
;; (byte-compile-file ".emacs.d/js2.el")
;;=============================================================================
;; misc
;;
(setq max-specpdl-size 100000) ;for byte-compile
(setq max-lisp-eval-depth 500000)

(put 'narrow-to-page 'disabled nil)

(require 'misc-swarm)
;;=============================================================================

(server-start)
;; cd ~/.emacs.d; emacs --batch -f batch-byte-compile **/*.el

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

(message "*************************")
(message "*** .emacs loaded OK. ***")
(message "*************************")
;;
;; end of .emacs
;;
;;


