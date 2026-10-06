;;; -*- lexical-binding: t -*-
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
;; hopper - jump to definitions, see solutions/hopper.el
;;
(use-package hopper
  :ensure nil
  :bind (("C-M-d" . hop-at-point)
         ("C-x d" . hop-at-point-other-window)
         ("M-S-<left>" . hop-backward)
         ("M-S-<right>" . hop-forward)
         ("<C-mouse-1>" . hop-by-mouse)))

;;-------------------------------------------------------------------
;; ztree
;;
;
(use-package ztree
  :straight '(ztree :type git :host github
			              :repo "fourier/ztree" :branch "master"))

;;-------------------------------------------------------------------
;; flycheck - on-the-fly syntax checking, used by rust-conf and js-conf
;;
(use-package flycheck
  :straight '(flycheck :type git :host github
			                 :repo "flycheck/flycheck" :branch "master")
  :defer t)

(provide 'ide)
