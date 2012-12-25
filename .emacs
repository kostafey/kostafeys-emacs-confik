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

(server-start)
(defvar *emacs-load-start* (current-time))
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
(defvar custom-conf-lisp-path (concat site-lisp-path "custom/"))
(add-to-list 'load-path custom-conf-lisp-path)
(defvar solutions-path
  (file-name-as-directory (expand-file-name "solutions" site-lisp-path)))
(add-to-list 'load-path solutions-path)

(require 'look-and-feel)

(require 'elpa-conf)

(require 'switch-language)
(require 'ispell-conf)
(require 'completition-conf)
(require 'communications)
(require 'reencoding-file)

(require 'copy-paste-clipboard-linux)
(require 'key-bindings)

(require 'history-conf)
(require 'text-modes-conf)

; (require 'version-control)
(require 'ide)
;; (require 'maxima-conf)
;; (require 'haskell-conf)
(require 'emacs-lisp-conf)
(require 'java-conf)
(require 'clojure-conf)
(require 'scheme-conf)
(require 'auctex-conf)
(require 'java-script-conf)
(require 'aj-compilation)
(require 'sphinx-frontend)
;; (require 'python-conf)
(require 'ejc-sql-conf nil 'noerror)

;; (require 'misc-swarm)

(require 'el-get-conf)
;;-----------------------------------------------------------------------------
(add-to-list 'load-path (concat site-lisp-path "my-task-centric/"))
(require 'calc-time)
(require 'hibernate-mapping)

(setq w32-quote-process-args t)

;;=============================================================================

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

(message "My .emacs loaded in %ds" 
         (destructuring-bind (hi lo ms) (current-time)
           (- (+ hi lo) (+ (first *emacs-load-start*) 
                           (second *emacs-load-start*)))))

(message "*************************")
(message "*** .emacs loaded OK. ***")
(message "*************************")
;;
;; end of .emacs
;;
;;


