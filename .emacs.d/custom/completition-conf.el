;;=============================================================================
;; Yet Another Snippet extension
;;=============================================================================
(defvar yasnippet-path (concat site-lisp-path "yasnippet-0.6.1c"))
(add-to-list 'load-path yasnippet-path)
(require 'yasnippet)                                             ;; not yasnippet-bundle
(yas/initialize)

(yas/load-directory (concat yasnippet-path "/snippets"))

(setq yas/root-directory (concat yasnippet-path "/mysnippets")) ;; Develop and keep personal snippets
(yas/load-directory yas/root-directory)                         ;; Load the snippets

(defvar yasnippet-prefix "\C-y")
(global-unset-key yasnippet-prefix)
(global-set-key (concat yasnippet-prefix "n") 'yas/new-snippet)
(global-set-key (concat yasnippet-prefix "f") 'yas/find-snippets)
(global-set-key (concat yasnippet-prefix "v") 'yas/visit-snippet-file)
(global-set-key (concat yasnippet-prefix "r") 'yas/reload-all)

;;=============================================================================
;; auto-complete
;;=============================================================================
(if (eq system-type 'gnu/linux)
    ((require 'auto-complete-config)
     (ac-config-default)))

;; (add-to-list 'load-path (concat site-lisp-path "auto-complete-1.3.1"))

;; (add-to-list 'load-path "/home/kostafey/.emacs.d/")
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "/home/kostafey/.emacs.d//ac-dict")
;; (ac-config-default)

;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete-1.3.1/ac-dict")
;; (require 'auto-complete-config)
;; (ac-config-default)

(if (eq system-type 'windows-nt)
    (when (require 'auto-complete nil t)
      (global-auto-complete-mode t)
      (setq ac-auto-start t)                  ;automatically start
      (setq ac-dwim t)                        ;Do what i mean
      (setq ac-override-local-map nil)        ;don't override local map
      ;; (set-face-background 'ac-menu-face "lightgray")
      ;; (set-face-underline 'ac-menu-face "darkgray")
      (set-face-background 'ac-selection-face "steelblue")
      (define-key ac-complete-mode-map "\t" 'ac-expand)
      (define-key ac-complete-mode-map "\r" 'ac-complete)
      (define-key ac-complete-mode-map "\M-n" 'ac-next)
      (define-key ac-complete-mode-map "\M-p" 'ac-previous)
                                        ;(auto-complete-mode)
      (global-auto-complete-mode t)
      (setq ac-auto-start 1)))

(global-set-key [f7] 'auto-complete-mode)
;;=============================================================================

(provide 'completition-conf)