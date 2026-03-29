;;; completition-corfu-conf.el -- COmpletion in Region FUnction configuration

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

  ;; Enable auto completion, configure delay, trigger and quitting
  (setq corfu-auto t
        corfu-auto-delay 0.2
        corfu-auto-prefix 2
        corfu-auto-trigger "."          ;; Custom trigger characters
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
   ("<right>" . (lambda () (interactive) (corfu-quit) (forward-char)))))

(provide 'completition-corfu-conf)
