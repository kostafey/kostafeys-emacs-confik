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
                   ("M-DEL" . vertico-directory-delete-word)))))
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
