;;-----------------------------------------------------------------------------
;; Cedet
(add-to-list 'load-path "~/.emacs.d/cedet-1.0pre6/common/")
(add-to-list 'load-path "~/.emacs.d/cedet-1.0pre6/semantic/")
(setq semantic-load-turn-useful-things-on t)
(load-file "~/.emacs.d/cedet-1.0pre6/common/cedet.el")

(require 'cedet)
;(global-set-key [?\C- ] 'semantic-ia-complete-symbol)

(defun my-semantic-hook ()
  (progn 
	 (semantic-tag-folding-mode 1)))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

(setq speedbar-tag-split-minimum-length 200)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; ECB

(setq ecb-layout-window-sizes (quote (("my-left" (0.25 . 0.66) (0.25 . 0.34))))
	  ecb-auto-activate t	  
	  ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1)
	  ecb-source-path (quote ("c:"))
	  ecb-tar-setup (quote cons)
	  ecb-tip-of-the-day nil
	  ecb-options-version "2.40")

(add-to-list 'load-path "~/.emacs.d/ecb-2.40/")
(require 'ecb)
(global-set-key (kbd "\e\el") 'ecb-toggle-ecb-windows)
(global-set-key (kbd "C-x C-a") 'ecb-activate)
(global-set-key (kbd "C-x C-q") 'ecb-deactivate)
;(global-set-key "\M-m" 'ecb-goto-window-methods)
;Перезагрузка окна методов после каждого сохранения
(setq imenu-auto-rescan 1)
;Imenu auto-rescan is disabled in buffers larger than this size (in bytes).
(setq imenu-auto-rescan-maxout 600000)
(setq imenu-max-item-length 600)
(setq imenu-use-markers t)
(setq imenu-max-items 200)

(setq speedbar-use-imenu-flag nil)


(ecb-layout-define "my-left" left nil
  (ecb-split-ver 0.6666666666666666 t)
  (if (fboundp (quote ecb-set-history-buffer)) (ecb-set-history-buffer) (ecb-set-default-ecb-buffer))
  (dotimes (i 1) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
  (dotimes (i 2) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
  (if (fboundp (quote ecb-set-methods-buffer)) (ecb-set-methods-buffer) (ecb-set-default-ecb-buffer))
  (dotimes (i 1) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
  (if (fboundp (quote ecb-set-history-buffer)) (ecb-set-history-buffer) (ecb-set-default-ecb-buffer))
  (dotimes (i 2) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
  (dotimes (i 2) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
  )

(setq ecb-layout-name "my-left"
      ecb-options-version "2.40")

(ecb-redraw-layout-full)

;;=============================================================================

(provide 'ide)