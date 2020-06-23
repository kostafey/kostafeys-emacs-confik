;;=============================================================================
;; Человеку свойственно ошибаться, но для нечеловеческих ляпов нужен компьютер.
;;                                                                  Пол Эрлих
;; To err is human, but to really foul things up you need a computer.
;;                                                               Paul Ehrlich
;;
;; Kostafey's Emacs confik :)
;; started from 08.08.2008, 22:45:25
;;
;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;; (_)___|_| |_| |_|\__,_|\___|___/
;;

(package-initialize)
(server-start)
;;=============================================================================
;; Elisp extensions paths
;;=============================================================================
(defvar site-lisp-path "~/.emacs.d/") ; root extensions folder
;;-----------------------------------------------------------------------------
;; Third-party *.el files (in general low-supporded, misplaced in ELPA)
;; without any patching or with tiny patches, stored in this repo.
(defvar third-party-lisp-path (concat site-lisp-path "artifacts/"))
(add-to-list 'load-path third-party-lisp-path)
;; Personal customization
(defvar custom-conf-lisp-path (concat site-lisp-path "custom/"))
(add-to-list 'load-path custom-conf-lisp-path)
(add-to-list 'load-path (concat custom-conf-lisp-path "langs/"))
;; Reusable customization
(defvar solutions-path
  (file-name-as-directory (expand-file-name "solutions" site-lisp-path)))
(add-to-list 'load-path solutions-path)
(add-to-list 'load-path (expand-file-name "foreign" site-lisp-path))
;;-----------------------------------------------------------------------------
;auto-customized custom-set-variables
(setq custom-file (concat custom-conf-lisp-path "custom.el"))
(load custom-file)
;;-----------------------------------------------------------------------------
(require 'elpa-conf)
(add-to-list 'load-path (concat third-party-lisp-path "popup-el/"))
(require 'popup)
(add-to-list 'load-path (concat third-party-lisp-path "wrap-region.el/"))
(require 'wrap-region)

;; (add-to-list 'load-path (concat site-lisp-path "popup-switcher/"))
;; (require 'popup-switcher)
(add-to-list 'load-path (concat site-lisp-path "clomacs/src/elisp/"))
(require 'clomacs nil 'noerror)
(add-to-list 'load-path (concat site-lisp-path "ejc-sql/"))
(require 'ejc-sql)
(require 'ejc-sql-conf nil 'noerror)

(require 'functions)
(require 'file-ops)
(require 'look-and-feel)
(require 'jiraf)
(require 'ide)
(require 'switch-language)
(require 'ispell-conf)
(require 'completition-conf)
(require 'ack-conf)
(require 'irc-conf)
(require 'communications nil 'noerror)
(require 'reencoding-file)
(require 'foreign)
(require 'copy-paste-clipboard-linux)
(add-to-list 'load-path (expand-file-name "eframe-jack-in/" site-lisp-path))
(require 'eframe-jack-in)
(require 'eframe-windmove)
(require 'shell-conf)
(require 'version-control)
(require 'key-bindings)
(require 'history-conf)
(require 'text-modes-conf)
(require 'perfomance-conf)

;;-----------------------------------------------------------------------------
;; Programming languages configs
;;
(require 'java-conf)
(require 'scala-conf)
(require 'clojure-conf)
(require 'emacs-lisp-conf)
(require 'common-lisp-conf)
;; (require 'scheme-conf)
;; (require 'python-conf)
;; (require 'maxima-conf)
;; (require 'haskell-conf)
;; (require 'auctex-conf)
;; (require 'mql-mode)
(require 'sphinx-frontend)
(require 'java-script-conf)
(require 'lua-conf)
(require 'xml-conf)
(require 'go-conf)
(require 'rust-conf)

;;-----------------------------------------------------------------------------
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
