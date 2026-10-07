;;; -*- lexical-binding: t -*-
;;-------------------------------------------------------------------
;; Emacs custom color theme
(use-package organic-green-theme
  :straight `(organic-green-theme
              :type git :host nil
              :repo ,(pcase system-type
                       ('windows-nt
                        "https://github.com/kostafey/organic-green-theme.git")
                       ('gnu/linux
                        "git@github.com:kostafey/organic-green-theme.git"))
              :branch "master")
  :config (load-theme 'organic-green t))

;;-------------------------------------------------------------------
(use-package dash
  :straight '(dash :type git :host github
                   :repo "magnars/dash.el" :branch "master")
  :config
  ;; Font lock of dash functions in emacs lisp buffers
  (eval-after-load "dash" '(dash-enable-font-lock)))

(straight-use-package
 '(rainbow-delimiters :type git :host github
                      :repo "Fanael/rainbow-delimiters" :branch "master"))

;; This minor mode sets background color to strings that match color
;; names, e.g. #0000ff is displayed in white with a blue background.
(use-package rainbow-mode
  :straight '(rainbow-mode :type git :host github
                           :repo "emacsmirror/rainbow-mode" :branch "master")
  :config (progn
            (defun k/rainbow-only-hex ()
              "Remove named color highlights from rainbow-mode."
              (interactive)
              (font-lock-remove-keywords
               nil
               `(,@rainbow-x-colors-font-lock-keywords
                 ,@rainbow-latex-rgb-colors-font-lock-keywords
                 ,@rainbow-r-colors-font-lock-keywords
                 ,@rainbow-html-colors-font-lock-keywords
                 ,@rainbow-html-rgb-colors-font-lock-keywords))
              (font-lock-flush))
            (add-hook 'rainbow-mode-hook #'k/rainbow-only-hex)))

(straight-use-package
 '(emacs-idle-highlight-mode :type git :host codeberg
                             :repo "ideasman42/emacs-idle-highlight-mode" :branch "main"))
;; paredit.org stopped resolving; this checkout only kept working because it
;; was cloned back when it did.  The emacsmirror copy carries the same
;; history -- its tip is the commit already checked out here.
(straight-use-package
 '(paredit :type git :host github
           :repo "emacsmirror/paredit" :branch "master"))
(straight-use-package
 '(paredit-everywhere :type git :host github
                      :repo "purcell/paredit-everywhere" :branch "master"))

(straight-use-package
 '(breadcrumb :type git :host github
              :repo "joaotavora/breadcrumb" :branch "master"))

(use-package tab-line
  :ensure nil
  :hook (after-init . global-tab-line-mode)
  :config
  (setq tab-line-close-button-show nil
        tab-line-new-button-show nil
        tab-line-separator (propertize " " 'display '(space :width (3)))
        tab-line-tab-name-function #'tab-line-tab-name-buffer
        tab-line-tabs-function #'tab-line-tabs-window-buffers
        tab-line-right-button nil
        tab-line-left-button nil)

  (dolist (mode '(ediff-mode process-menu-mode))
    (add-to-list 'tab-line-exclude-modes mode))

  (global-tab-line-mode t))

;; `tab-line-auto-hscroll' switches to the hidden ` *tab-line-hscroll*'
;; buffer unconditionally, so once anything kills it every
;; `(:eval (tab-line-format))' signals "Selecting deleted buffer" and the
;; tab line silently renders as an empty strip until Emacs is restarted.
(defun k/tab-line-ensure-hscroll-buffer (&rest _)
  "Recreate `tab-line-auto-hscroll-buffer' when it has been killed."
  (unless (buffer-live-p tab-line-auto-hscroll-buffer)
    (setq tab-line-auto-hscroll-buffer
          (generate-new-buffer " *tab-line-hscroll*"))))

(advice-add 'tab-line-auto-hscroll :before #'k/tab-line-ensure-hscroll-buffer)

(global-set-key (kbd "C-<next>") 'tab-line-switch-to-next-tab)
(global-set-key (kbd "C-<prior>") 'tab-line-switch-to-prev-tab)

;;-------------------------------------------------------------------
;; breadcrumb-mode
(defun k/toggle-tab-line-breadcrumb ()
  "Toggle between `tab-line-mode' and `breadcrumb-mode'."
  (interactive)
  (if tab-line-mode
      (progn
        (tab-line-mode -1)
        (breadcrumb-mode t))
    (progn
      (breadcrumb-mode -1)
      (tab-line-mode t))))

(global-set-key (kbd "<f6>") 'k/toggle-tab-line-breadcrumb)

;;-------------------------------------------------------------------
(use-package writeroom-mode
  :straight '(writeroom-mode
              :type git :host github
              :repo "joostkremers/writeroom-mode" :branch "master")
  :config
  (add-hook 'writeroom-mode-enable-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (global-display-fill-column-indicator-mode -1)))
  (add-hook 'writeroom-mode-disable-hook
            (lambda ()
              (display-line-numbers-mode 1)
              (global-display-fill-column-indicator-mode 1)))
  :bind (("C-<f11>" . writeroom-mode)))

;;-------------------------------------------------------------------
;; Paredit customization
;;
(put 'paredit-forward 'CUA 'move)

(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map (kbd "C-M-f") nil)
     (define-key paredit-mode-map (kbd "C-<left>") nil) ; C-}
     (define-key paredit-mode-map (kbd "C-M-<left>") nil)
     (define-key paredit-mode-map (kbd "C-<right>") nil) ; C-)
     (define-key paredit-mode-map (kbd "C-M-<right>") nil)
     (define-key paredit-mode-map (kbd "C-M-<up>") nil)
     (define-key paredit-mode-map (kbd "M-<up>") nil)
     (define-key paredit-mode-map (kbd "M-<down>") nil)
     (define-key paredit-mode-map (kbd "C-j") nil)
     (define-key paredit-mode-map (kbd "C-S-M-n") 'paredit-newline)
     (define-key paredit-mode-map (kbd "C-d") nil)
     (define-key paredit-mode-map (kbd "<delete>") nil)
     (define-key paredit-mode-map (kbd "<DEL>") nil)
     (define-key paredit-mode-map (kbd "<deletechar>") nil)
     (define-key paredit-mode-map (kbd "<backspace>") nil)
     (define-key paredit-mode-map (kbd "M-r") nil)
     (define-key paredit-mode-map (kbd "M-C-'") 'paredit-raise-sexp)
     (define-key paredit-mode-map (kbd ")") 'nil)
     (define-key paredit-mode-map (kbd "]") 'nil)
     (define-key paredit-mode-map (kbd "\\") 'nil)
     (define-key paredit-mode-map (kbd "\"") 'nil)
     (define-key paredit-mode-map (kbd "C-M-d") 'nil)
     (define-key paredit-mode-map (kbd "M-q") 'nil)
     (define-key paredit-mode-map (kbd "M-r") 'nil)
     (define-key paredit-mode-map (kbd "C-M-n") 'nil)))

(eval-after-load "paredit-everywhere"
  '(progn
     (define-key paredit-everywhere-mode-map (kbd "M-r") 'replace-string)))

(global-set-key [(meta super right)] 'transpose-sexps)
(global-set-key [(meta super left)] (lambda () (interactive) (transpose-sexps -1)))

(defun my-common-coding-hook ()
  (rainbow-delimiters-mode t)   ; Magic lisp parentheses rainbow
  (idle-highlight-mode t)
  (font-lock-warn-todo))

;; -------------------------------------------------------------------
;; By default `idle-highlight-mode' skips symbols inside strings entirely;
;; in eglot buffers let it highlight them too.
(defun k/eglot-idle-highlight-setup ()
  (if (eglot-managed-p)
      (setq-local idle-highlight-exceptions-face
                  (remq 'font-lock-string-face
                        (default-value 'idle-highlight-exceptions-face)))
    (kill-local-variable 'idle-highlight-exceptions-face)))

(add-hook 'eglot-managed-mode-hook #'k/eglot-idle-highlight-setup)
;; -------------------------------------------------------------------

(defun my-coding-hook ()
  (my-common-coding-hook)
  (paredit-everywhere-mode)
  (electric-pair-mode))

(defun my-web-mode-hook ()
  (my-coding-hook)
  (setq-default indent-tabs-mode nil)
  (setq indent-line-function 'web-mode-indent-line)
  ;; (setq-local indent-line-function 'indent-relative)
  )

(defun my-lisp-coding-hook ()
  (my-common-coding-hook)
  (enable-paredit-mode))

(add-to-list 'auto-mode-alist '("\\.iss$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.cnf$" . conf-mode))

(add-hook 'emacs-lisp-mode-hook 'my-lisp-coding-hook)
(add-hook 'lisp-mode-hook       'my-lisp-coding-hook)
(add-hook 'scheme-mode-hook     'my-lisp-coding-hook)
(add-hook 'clojure-mode-hook    'my-lisp-coding-hook)
(add-hook 'cider-mode-hook      'my-lisp-coding-hook)
(add-hook 'fennel-mode-hook     'my-lisp-coding-hook)
(add-hook 'sbt-mode-hook        'my-coding-hook)
(add-hook 'java-mode-hook       (lambda () (rainbow-delimiters-mode t)))
(add-hook 'java-ts-mode-hook    (lambda () (rainbow-delimiters-mode t)))
(add-hook 'markdown-mode-hook   'my-coding-hook)
(add-hook 'tex-mode-hook        'my-coding-hook)
(add-hook 'lua-mode-hook        'my-coding-hook)
(add-hook 'python-mode-hook     'my-coding-hook)
(add-hook 'comint-mode-hook     'my-coding-hook)
(add-hook 'js-mode-hook         'my-coding-hook)
(add-hook 'js-ts-mode-hook      'my-coding-hook)
(add-hook 'typescript-mode-hook 'my-coding-hook)
(add-hook 'tide-mode            'my-coding-hook)
(add-hook 'sql-mode-hook        'my-coding-hook)
(add-hook 'go-mode-hook         'my-coding-hook)
(add-hook 'powershell-mode-hook 'my-coding-hook)
(add-hook 'rust-mode-hook       'my-coding-hook)
(add-hook 'php-mode-hook        'my-coding-hook)
(add-hook 'web-mode-hook        'my-web-mode-hook)

(provide 'appearance)
