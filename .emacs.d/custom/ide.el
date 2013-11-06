;;-----------------------------------------------------------------------------
;; Configuration is partially stolen from https://gist.github.com/3930120
;; See also: http://alexott.net/ru/writings/emacs-devenv/EmacsCedet.html
;;
;;=============================================================================
;; Cedet
;; select which submodes we want to activate
;;
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

;; load contrib library
(add-to-list 'load-path (concat site-lisp-path "cedet/contrib/"))
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

;; (setq global-semantic-tag-folding-mode t)

(setq speedbar-tag-split-minimum-length 200)
;;
;;=============================================================================

;;=============================================================================
;; ECB
;;
(require 'ecb)
;Перезагрузка окна методов после каждого сохранения
(setq imenu-auto-rescan 1)
;Imenu auto-rescan is disabled in buffers larger than this size (in bytes).
(setq imenu-auto-rescan-maxout 600000)
(setq imenu-max-item-length 600)
(setq imenu-use-markers t)
(setq imenu-max-items 200)

(setq speedbar-use-imenu-flag nil)

(setq ecb-compile-window-height nil)

;; user-defined ECB-layout created by the command `ecb-create-new-layout'.
(ecb-layout-define "my-left" left nil
  (ecb-split-ver 0.6875 t)
  (if (fboundp (quote ecb-set-sources-buffer)) (ecb-set-sources-buffer) (ecb-set-default-ecb-buffer))
  (dotimes (i 1) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
  (if (fboundp (quote ecb-set-directories-buffer)) (ecb-set-directories-buffer) (ecb-set-default-ecb-buffer))
  (dotimes (i 2) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
  (if (fboundp (quote ecb-set-methods-buffer)) (ecb-set-methods-buffer) (ecb-set-default-ecb-buffer))
  (dotimes (i 1) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
  (if (fboundp (quote ecb-set-sources-buffer)) (ecb-set-sources-buffer) (ecb-set-default-ecb-buffer))
  (dotimes (i 2) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
  (dotimes (i 2) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
  )

(setq ecb-layout-window-sizes
      (quote (("my-left" 
               (ecb-methods-buffer-name 0.25 . 0.66) 
               (ecb-sources-buffer-name 0.25 . 0.34)))))

(setq ecb-layout-name "my-left")

(setq ecb-auto-activate nil	  
	  ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1)
	  ecb-source-path (quote ("c:"))
	  ecb-tar-setup (quote cons)
	  ecb-tip-of-the-day nil
	  ecb-options-version "2.40")

(ecb-redraw-layout-full)

;;=============================================================================

;;-----------------------------------------------------------------------------
;; projectile
(projectile-global-mode)

(provide 'ide)
