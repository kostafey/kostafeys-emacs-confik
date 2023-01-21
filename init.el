;;===================================================================
;; To err is human, but to really foul things up you need a computer.
;;                                                     Paul Ehrlich
;;
;; Kostafey's Emacs confik :)
;; started from 08.08.2008, 22:45:25
;;
;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;; (_)___|_| |_| |_|\__,_|\___|___/
;;
(server-start)
;;===================================================================
;; Elisp extensions paths
;
(defvar site-lisp-path "~/.emacs.d/") ; root extensions folder

;; Personal customization
(add-to-list 'load-path (concat site-lisp-path "custom/"))
;; Third-party *.el files (in general low-supporded, misplaced in ELPA)
;; without any patching or with tiny patches, stored in this repo.
(add-to-list 'load-path (concat site-lisp-path "artifacts/"))
;; Programming languages configs
(add-to-list 'load-path (concat site-lisp-path "custom/langs/"))
;; Reusable customization
(add-to-list 'load-path (expand-file-name "solutions" site-lisp-path))
(add-to-list 'load-path (expand-file-name "foreign" site-lisp-path))
(add-to-list 'load-path (expand-file-name "popup-switcher" site-lisp-path))

;; No third-party dependencies
(require 'basic-text-editing)
(require 'basic)
(require 'look-and-feel)
(require 'switch-language)
;; Use third-party dependencies
(require 'elpa-conf)
(require 'appearance)
(require 'minibuffer-conf)
(require 'version-control)
(require 'ack-conf)

;;-------------------------------------------------------------------
;; auto-customized custom-set-variables
(setq custom-file (concat site-lisp-path "custom/custom.el"))
(load custom-file)
;;-------------------------------------------------------------------

(use-elpa 'cider)
(use-elpa 'simple-httpd)
(add-to-list 'load-path (concat site-lisp-path "clomacs/src/elisp/"))
(require 'clomacs nil 'noerror)
(add-to-list 'load-path (concat site-lisp-path "ejc-sql/"))
(require 'ejc-sql-conf nil 'noerror)

(require 'key-bindings)

(require 'ide)
(require 'history-conf)
(require 'text-modes-conf)
(require 'foreign)
(require 'perfomance-conf)
(require 'eframe-windmove)
;; (require 'irc-conf)

;;-------------------------------------------------------------------
;; Programming languages configs
;;
;; (require 'java-conf)
(require 'scala-conf)
(require 'clojure-conf)
(require 'emacs-lisp-conf)
(require 'js-conf)
(require 'xml-conf)
(require 'go-conf)
(require 'fennel-conf)
;; (require 'common-lisp-conf)
;; (require 'mql-mode)
;; (require 'sphinx-frontend)
;; (require 'lua-conf)
;; (require 'rust-conf)
;; (require 'auctex-conf)

;;-------------------------------------------------------------------
;; Packages without customization
;;
(use-elpa 'rainbow-mode)

;;-------------------------------------------------------------------
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
