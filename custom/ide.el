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

;;-------------------------------------------------------------------
;; eglot
;;
(defun k/eglot-server-name (orig server)
  "Name SERVER by its program when it reports no `serverInfo' name.
Eglot falls back to the connection name, which already holds the
project and the major modes, so its buffer names repeat them twice."
  (or (plist-get (eglot--server-info server) :name)
      (when-let* ((proc (jsonrpc--process server))
                  (cmd (process-command proc))
                  ((consp cmd)))
        (file-name-nondirectory (car cmd)))
      (funcall orig server)))

(with-eval-after-load 'eglot
  (advice-add 'eglot--server-name :around #'k/eglot-server-name))

(provide 'ide)
