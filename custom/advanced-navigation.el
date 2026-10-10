;;; advanced-navigation.el --- Navigation packages  -*- lexical-binding: t -*-

;; The counterpart of `basic-navigation', built on third-party packages and
;; own libraries.

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
;; highlight-symbol
;;
(use-package highlight-symbol
  :straight (highlight-symbol :type git :host github
                              :repo "nschum/highlight-symbol.el"
                              :branch "master")
  :preface
  (defun k/highlight-thing-at-point ()
    "Toggle highlighting of the symbol at point, or of the active region."
    (interactive)
    (if (use-region-p)
        (highlight-symbol
         (regexp-quote (buffer-substring (region-beginning) (region-end))))
      (highlight-symbol)))

  (defun k/highlight-isearch-string ()
    "Toggle highlighting of the current search string."
    (interactive)
    (when (string-empty-p isearch-string)
      (user-error "Empty search string"))
    ;; `highlight-symbol' takes a regexp.
    (highlight-symbol (substring-no-properties
                       (if isearch-regexp
                           isearch-string
                         (regexp-quote isearch-string)))))
  :bind (("C-<f3>" . k/highlight-thing-at-point)
         ("S-<f3>" . highlight-symbol-prev)
         ("M-<f3>" . highlight-symbol-remove-all)
         ("C-M-<up>" . highlight-symbol-prev)
         ("C-M-<down>" . highlight-symbol-next)
         :map isearch-mode-map
         ("C-<f3>" . k/highlight-isearch-string)))

;;-------------------------------------------------------------------
;; ace-jump-mode
;;
(use-package ace-jump-mode
  :straight `(ace-jump-mode
              :type git :host nil
              :repo ,(pcase system-type
                       ('windows-nt
                        "https://github.com/winterTTr/ace-jump-mode.git")
                       ('gnu/linux
                        "git@github.com:winterTTr/ace-jump-mode.git"))
              :branch "master")
  :bind (("M-a" . ace-jump-mode)
         ("C-c M-a" . ace-jump-mode-pop-mark))
  :custom
  (ace-jump-mode-scope 'window))

;;-------------------------------------------------------------------
;; eframe-nav
;;
;; Local module (custom/eframe-nav.el): buffer and multi-frame navigation.
(use-package eframe-nav
  :straight nil
  ;; It installs windmove advice on load, so load it right away.
  :demand t
  :bind (("C-M-e" . eframe-pop-emacs)
         ("C-w" . eframe-kill-buffer)
         ("C-<next>" . eframe-next-buffer)
         ("C-<prior>" . eframe-previous-buffer)))

;;-------------------------------------------------------------------
;; temporary-persistent
;;
(use-package temporary-persistent
  :straight `(temporary-persistent
              :type git :host nil
              :repo ,(pcase system-type
                       ('windows-nt
                        "https://github.com/kostafey/temporary-persistent.git")
                       ('gnu/linux
                        "git@github.com:kostafey/temporary-persistent.git"))
              :branch "master")
  ;; Desktop restores the temp buffers at startup; loaded right away, it
  ;; saves them on exit and lists them in its `consult-buffer' source.
  :demand t
  :bind (("C-x C-c" . temporary-persistent-switch-buffer))
  :config
  (setq temporary-persistent-default-major-mode 'markdown-mode)
  ;; `temporary-persistent' does not pull in `consult' itself, and `consult'
  ;; is loaded lazily, so register the source once `consult' is there.
  (with-eval-after-load 'consult
    (add-to-list 'consult-buffer-sources 'temporary-persistent-consult-source t)
    (add-to-list 'consult-buffer-filter "\\`\\*temp\\(-[0-9]+\\)?\\*\\'")))

(provide 'advanced-navigation)
