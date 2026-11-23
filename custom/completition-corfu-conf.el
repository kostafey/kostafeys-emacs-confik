;;; completition-corfu-conf.el -- COmpletion in Region FUnction configuration  -*- lexical-binding: t -*-

(use-package fussy
  :straight '(fussy :type git :host github
			              :repo "jojojames/fussy" :branch "main")
  :config
  (fussy-setup))

(use-package corfu
  :straight '(corfu :type git :host github
			              :repo "minad/corfu" :branch "main")

  :init
  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  (corfu-history-mode)
  (corfu-popupinfo-mode)

  ;; Enable auto completion, configure delay and quitting.
  ;; There is no trigger chars option in `corfu', a mode-specific command
  ;; bound to the trigger char is used instead, e.g. `ejc-dot-pressed'
  ;; with `ejc-complete-on-dot' set - see custom/ejc-sql-conf.el
  (setq corfu-auto t
        corfu-auto-delay 0.2
        corfu-auto-prefix 2
        corfu-quit-no-match 'separator) ;; or t

  (defun k/corfu-mode-hook ()
    ;; Settings only for Corfu
    (setq-local completion-styles '(basic substring partial-completion fussy)))
  (add-hook 'corfu-mode-hook #'k/corfu-mode-hook)

  :bind
  (:map corfu-map
   ("<prior>" . corfu-scroll-down)
   ("<next>" . corfu-scroll-up)
   ("<left>" . (lambda () (interactive) (corfu-quit) (backward-char)))
   ("<right>" . (lambda () (interactive) (corfu-quit) (forward-char))))

  :config
  ;; In `eshell-mode' RET runs the command as typed, ignoring the popup;
  ;; a candidate is taken with TAB.  The filter returns nil there, so the
  ;; key falls through `corfu-map' to `eshell-send-input', and the popup
  ;; closes once point leaves the completed region.
  (defun k/corfu-ret-filter (cmd)
    (unless (derived-mode-p 'eshell-mode) cmd))
  (define-key corfu-map (kbd "RET")
              `(menu-item "" corfu-insert :filter k/corfu-ret-filter)))

;;-------------------------------------------------------------------
;; No tags completion
;;
;; `tags-completion-at-point-function' is the stock Emacs value of
;; `completion-at-point-functions', so corfu reaches it in every buffer that
;; has no capf of its own, and it visits the tags table on the way.  Nothing
;; in this configuration builds a TAGS file, so all it ever produces is
;; "File ... does not exist" for a table left over from some old session -
;; see `history-conf.el', where `desktop' is told to stop restoring one.
;; Words in prose come from the dictionaries in `dict-conf.el' instead.
(setq-default completion-at-point-functions
              (delq 'tags-completion-at-point-function
                    (default-value 'completion-at-point-functions)))

(provide 'completition-corfu-conf)
