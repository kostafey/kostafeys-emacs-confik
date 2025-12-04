;;; Minibuffer configuration. -*- lexical-binding: t -*-

;;-------------------------------------------------------------------
;; save minibuffer history between sessions
;;
(savehist-mode t)

;;-------------------------------------------------------------------
;; vertico.el - VERTical Interactive COmpletion
;;
(use-package vertico
  :straight '(vertico
              :type git :host github
              :repo "minad/vertico" :branch "main")
  :init
  (vertico-mode)
  :config
  (progn
    (setq completion-styles '(basic substring partial-completion flex))
    (setq read-file-name-completion-ignore-case t
          read-buffer-completion-ignore-case t
          completion-ignore-case t)))

;; Configure directory extension.
(use-package vertico-directory
  :straight '(vertico
              :type git :host github
              :repo "minad/vertico" :branch "main")
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package marginalia
  :straight '(marginalia
              :type git :host github
              :repo "minad/marginalia" :branch "main")
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package consult
  :straight '(consult
              :type git :host github
              :repo "minad/consult" :branch "main")
  :config (progn
            (setq register-preview-delay 1)
            (setq consult-async-min-input 3)
            (setq consult-async-split-style 'perl))
  :bind (("C-x b" . consult-buffer)
         ("C-S-n" . consult-project-buffer)
         ("C-x i" . consult-imenu)
         ("C-S-i" . consult-imenu-multi)
         ("C-S-f" . consult-line)
         ("C-S-r" . consult-line-multi)
         ("C-M-f" . consult-ripgrep)
         ("C-c C-f" . consult-find)     ; find file
         ;; Defined in `basic.el':
         ;; ("C-S-b" . bookmark-set)
         ;; ("C-b" . bookmark-jump) ; <f3>
         ;; ("M-b" . bookmark-delete)
         ;; ("C-c b" . bookmark-delete)
         ("<f3>" . consult-bookmark)
         ("C-b" . consult-bookmark)
         ("M-g" . consult-goto-line)
         ("C-x C-x" . consult-global-mark)))

(provide 'minibuffer-conf)
