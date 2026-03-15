;;-------------------------------------------------------------------
;; Emacs custom color theme
(straight-use-package
 '(organic-green-theme :type git :host gitlab
					             :repo "kostafey/organic-green-theme" :branch "master"))
(setq organic-green-boldless t)
(setq organic-green-version 2)
(load-theme 'organic-green t)

;;=============================================================================
;; mode-line (emacs status bar) config
;;
(use-package minions
  :straight '(minions :type git :host github
					            :repo "tarsius/minions" :branch "main")
  :config (minions-mode 1))

;;-------------------------------------------------------------------
(straight-use-package
 '(dash :type git :host github
				:repo "magnars/dash.el" :branch "master"))
;; Font lock of dash functions in emacs lisp buffers
(eval-after-load "dash" '(dash-enable-font-lock))

(straight-use-package
 '(rainbow-delimiters :type git :host github
				              :repo "Fanael/rainbow-delimiters" :branch "master"))

;; This minor mode sets background color to strings that match color
;; names, e.g. #0000ff is displayed in white with a blue background.
(straight-use-package
 '(rainbow-mode :type git :host github
				        :repo "emacsmirror/rainbow-mode" :branch "master"))

(straight-use-package
 '(emacs-idle-highlight-mode :type git :host codeberg
				                     :repo "ideasman42/emacs-idle-highlight-mode" :branch "main"))
(straight-use-package
 '(paredit :type git :host nil
				   :fetch "https://paredit.org/cgit/paredit/" :branch "master"))
(straight-use-package
 '(paredit-everywhere :type git :host github
				              :repo "purcell/paredit-everywhere" :branch "master"))

(straight-use-package
 '(tabbar :type git :host github
				  :repo "dholm/tabbar" :branch "master"))
(straight-use-package
 '(breadcrumb :type git :host github
				      :repo "joaotavora/breadcrumb" :branch "master"))

(use-package tab-line
  :ensure nil
  :hook (after-init . global-tab-line-mode)
  :config
  (setq tab-line-close-button-show nil
        tab-line-new-button-show nil
        tab-line-separator (propertize " " 'display '(space :width (4)))
        tab-line-tab-name-function #'tab-line-tab-name-buffer
        tab-line-tabs-function #'tab-line-tabs-window-buffers
        tab-line-right-button nil
        tab-line-left-button nil)
  (defun k/set-tab-theme ()
    (let* ((organic-green-black    "#444D56")
           (organic-highlight-gray "#E3F2E1")
           (organic-shadow         "#D3E0D3")
           (fg                     organic-green-black)
           (fringe-bg              organic-highlight-gray)
           (buffer-bg              (face-attribute 'default :background))
           (tab-bg                 organic-shadow))
      (set-face-attribute 'tab-line nil
			                    :background fringe-bg
			                    :foreground fg
			                    :height 0.8
			                    :inherit nil
			                    :box nil)
      (set-face-attribute 'tab-line-tab nil
			                    :background organic-shadow
			                    :foreground fg
			                    :weight 'normal
			                    :inherit nil
			                    ;; :box (list :line-width -1 :color tab-bg)
                          :box nil)
      (set-face-attribute 'tab-line-tab-inactive nil
			                    :foreground fg
			                    :background tab-bg
			                    :weight 'normal
			                    :inherit nil
                          :box nil)
      (set-face-attribute 'tab-line-highlight nil
			                    :foreground fg
			                    :background buffer-bg
			                    :weight 'normal
			                    :inherit nil
			                    :box nil)
      (set-face-attribute 'tab-line-tab-current nil
			                    :foreground fg
			                    :background buffer-bg
			                    :weight 'semi-bold
			                    :inherit nil
			                    :box nil)))
  (k/set-tab-theme)

  (dolist (mode '(ediff-mode process-menu-mode))
    (add-to-list 'tab-line-exclude-modes mode))

  (global-tab-line-mode t))

(global-set-key (kbd "C-<next>") 'tab-line-switch-to-next-tab)
(global-set-key (kbd "C-<prior>") 'tab-line-switch-to-prev-tab)

;;-------------------------------------------------------------------
;; Tabs - Tabbar

(tabbar-mode -1)

;; Hide forward and back buttons
(customize-set-variable 'tabbar-scroll-right-button '(("") ""))
(customize-set-variable 'tabbar-scroll-left-button '(("") ""))
(customize-set-variable 'tabbar-buffer-home-button '(("") ""))

;; (global-set-key (kbd "C-<next>") 'tabbar-forward-tab)
;; (global-set-key (kbd "C-<prior>") 'tabbar-backward-tab)

(setq tabbar-buffer-list-function
      '(lambda ()
         (delq nil
               (mapcar #'(lambda (b)
                           (cond
                            ;; Always include the current buffer.
                            ((eq (current-buffer) b) b)
                            ((buffer-file-name b) b)
                            ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                            ((buffer-live-p b) b)))
                       (-filter
                        (lambda (b) (or (eq (current-buffer) b)
                                   (buffer-file-name b)
                                   (string-match
                                    "^\\*temp\\(-[0-9]+\\)?\\*$"
                                    (buffer-name b))))
                        (buffer-list))))))

(defun k/select-window-fix-tabbar ()
  "Hide `tabbar' for buffers displayed in windows located
not in the top of the frame."
  (when tabbar-mode
    (-map
     (lambda (window)
       (let ((buffer (window-buffer window)))
         (with-current-buffer buffer
           (condition-case nil
               ;; Keep `tabbar' if
               (if (or
                    ;; buffer is displayed in window located
                    ;; in the top of the frame
                    (not (> (cadr (window-edges window)) 0))
                    (and
                     ;; or this buffer displayed in other window too
                     (> (length (get-buffer-window-list buffer)) 1)
                     ;; but not only in the bottom windows.
                     (not (-all?
                           (lambda (w) (> (cadr (window-edges w)) 0))
                           (get-buffer-window-list buffer)))))
                   (tabbar-local-mode -1)
                 ;; Hide `tabbar' otherwise.
                 (tabbar-local-mode 1))
             (error nil)))))
     (window-list))
    (global-set-key (kbd "C-<next>") 'tabbar-forward-tab)
    (global-set-key (kbd "C-<prior>") 'tabbar-backward-tab)))

;;-------------------------------------------------------------------
;; breadcrumb-mode
(defun toggle-tabbar-breadcrumb ()
  "Toggle between `tabbar-mode' and `breadcrumb-mode'."
  (interactive)
  (if tab-line-mode
      (progn
        ;; (tabbar-mode -1)
        (tab-line-mode -1)
        (breadcrumb-mode t))
    (progn
      (breadcrumb-mode -1)
      ;; (tabbar-mode t)
      (tab-line-mode t))))

;;-------------------------------------------------------------------
(when (require 'ejc-sql nil 'noerror)
  (add-hook 'ejc-sql-complete-query-hook 'k/select-window-fix-tabbar))

(defadvice select-window (after
                          k/select-window
                          activate)
  (k/select-window-fix-tabbar))

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
(add-hook 'markdown-mode-hook   'my-coding-hook)
(add-hook 'mql-mode-hook        'my-coding-hook)
(add-hook 'tex-mode-hook        'my-coding-hook)
(add-hook 'lua-mode-hook        'my-coding-hook)
(add-hook 'tcl-mode-hook        'my-coding-hook)
(add-hook 'python-mode-hook     'my-coding-hook)
(add-hook 'comint-mode-hook     'my-coding-hook)
(add-hook 'js-mode-hook         'my-coding-hook)
(add-hook 'js-ts-mode-hook      'my-coding-hook)
(add-hook 'typescript-mode-hook 'my-coding-hook)
(add-hook 'tide-mode            'my-coding-hook)
(add-hook 'sql-mode-hook        'my-coding-hook)
(add-hook 'mql-mode-hook        'my-coding-hook)
(add-hook 'go-mode-hook         'my-coding-hook)
(add-hook 'powershell-mode-hook 'my-coding-hook)
(add-hook 'rust-mode-hook       'my-coding-hook)
(add-hook 'php-mode-hook        'my-coding-hook)
(add-hook 'web-mode-hook        'my-web-mode-hook)

(provide 'appearance)

