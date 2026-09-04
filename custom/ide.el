;;-------------------------------------------------------------------
;; imenu
;;
(require 'imenu)
;; Reload methods list on any save
(setq imenu-auto-rescan 1)
;Imenu auto-rescan is disabled in buffers larger than this size (in bytes).
(setq imenu-auto-rescan-maxout 600000)
(setq imenu-max-item-length 600)
(setq imenu-use-markers t)
(setq imenu-max-items 200)

;;-------------------------------------------------------------------
;; ztree
;;
;
(use-package ztree
  :vc (:url "https://github.com/fourier/ztree.git"
       :branch "master"))

(provide 'ide)
