;;; Minibuffer configuration. -*- lexical-binding: t -*-

(use-elpa 'use-package)

;;-------------------------------------------------------------------
;; save minibuffer history between sessions
;;
(use-package savehist
  :init
  (savehist-mode t))

(defcustom minibuffer-engine 'vertico
  "Minibuffer completion engine.")

(cl-case minibuffer-engine
  ('vertico
   (progn
     ;;-------------------------------------------------------------------
     ;; vertico.el - VERTical Interactive COmpletion
     ;;
     (use-package vertico
       :ensure t
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
       :after vertico
       :ensure nil
       ;; More convenient directory navigation commands
       :bind (:map vertico-map
                   ("RET" . vertico-directory-enter)
                   ("DEL" . vertico-directory-delete-char)
                   ("M-DEL" . vertico-directory-delete-word)))

     (use-package marginalia
       :ensure t
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
       :ensure t
       ;; :custom (consult-config `((consult-bookmark :preview-key any)))
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
              ("C-c C-f" . consult-find) ; find file
              ;; Defined in `basic.el':
              ;; ("C-b" . bookmark-set)
              ;; ("M-b" . bookmark-jump)
              ;; ("C-S-b" . bookmark-delete)
              ("C-c b" . consult-bookmark)
              ("M-g" . consult-goto-line)
              ("C-x C-x" . consult-global-mark)))))
  ('ido
   (progn
     ;;-------------------------------------------------------------------
     ;; ido - switch buffers by completiotion
     ;;
     (require 'ido)
     (ido-mode t)
     (ido-everywhere t)
     ;; flx configuration - fuzzy matching files and paths via ido
     (require 'flx-ido)
     (flx-ido-mode 1)
     ;; disable ido faces to see flx highlights.
     (setq ido-use-faces nil)
     ;; minibuffer autocompletition.
     (icomplete-mode))))

(provide 'minibuffer-conf)
