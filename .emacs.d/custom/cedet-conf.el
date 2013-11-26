;;-----------------------------------------------------------------------------
;; Configuration is partially stolen from https://gist.github.com/3930120
;; See also: http://alexott.net/ru/writings/emacs-devenv/EmacsCedet.html
;;
;;=============================================================================
;; Cedet
;; select which submodes we want to activate
;;
(defvar cedet-root-path (concat site-lisp-path "cedet/"))
(add-to-list 'load-path cedet-root-path)
(load (concat site-lisp-path "cedet/cedet-devel-load"))

;; load contrib library
(add-to-list 'load-path (concat site-lisp-path "cedet/contrib/"))

;; включает глобальную поддержку Semanticdb;
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)

;; включает режим автоматического запоминания информации о редактируемых тагах,
;; так что вы можете перейти к ним позднее с помощью команды
;; semantic-mrub-switch-tags; 
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)

;; активирует контекстное меню привязанное к правой клавише мыши;
;; (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)

;; активирует показ названия текущего тага в верхней строке буфера;
;; (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

;; активирует подстветку первой строки текущего тага (функции, класса и т.п.);
;; (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)

;; активирует автоматический анализ кода в буферах когда Emacs "свободен" и
;; ожидает ввода данных от пользователя (idle time);
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)

;; включает подсветку вхождений локальных переменных чье имя совпадает с именем
;; текущего тага;
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)

;; Activate semantic
(semantic-mode 1)

(require 'eassist)

;; Чтобы использовать возможности по дополнению имен и показу информации о
;; функциях и классах, вам необходимо загрузить пакет semantic/ia с помощью
;; следующей команды:
(require 'semantic/ia)

(require 'semantic/db-javap)

(defun my-semantic-hook ()
  ;; (global-semantic-tag-folding-mode 1)
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

(when (cedet-ectag-version-check t)
  (semantic-load-enable-primary-ectags-support))

;; (setq global-semantic-tag-folding-mode t)

;; SRecode
;; (global-srecode-minor-mode 1)

;; EDE
;; (global-ede-mode 1)
;; (ede-enable-generic-projects)
;;
;;=============================================================================

(provide 'cedet-conf)
