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
(setq projectile-indexing-method 'native)

(provide 'ide)

