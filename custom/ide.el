;;=============================================================================
;; imenu
;;
(require 'imenu)
;Перезагрузка окна методов после каждого сохранения
(setq imenu-auto-rescan 1)
;Imenu auto-rescan is disabled in buffers larger than this size (in bytes).
(setq imenu-auto-rescan-maxout 600000)
(setq imenu-max-item-length 600)
(setq imenu-use-markers t)
(setq imenu-max-items 200)

;;=============================================================================

;;-----------------------------------------------------------------------------
;; projectile
(projectile-mode)
(setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)

(provide 'ide)
