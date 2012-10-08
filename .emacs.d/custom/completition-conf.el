;;=============================================================================
;; Yet Another Snippet extension
;;=============================================================================
(defvar yasnippet-path (concat site-lisp-path "yasnippet"))
(add-to-list 'load-path yasnippet-path)
(require 'yasnippet)                                             ;; not yasnippet-bundle
(yas/initialize)

(yas/load-directory (concat yasnippet-path "/snippets"))

(setq yas/root-directory (concat yasnippet-path "/mysnippets")) ;; Develop and keep personal snippets
(yas/load-directory yas/root-directory)                         ;; Load the snippets

(yas/global-mode 1)

;;=============================================================================
;; auto-complete
;;=============================================================================

(defvar ac-required-packages
  (list ;'popup
        'auto-complete)
  "Required packages for autocompletition.")

(dolist (package ac-required-packages)
  (when (not (package-installed-p package))    
    (package-install package)))

(require 'auto-complete-config)
(ac-config-default)

(global-auto-complete-mode t)
;; if a length of a word you entered is larger than the value,
;; completion will be started automatically
(setq ac-auto-start 2)
(setq ac-dwim t)               ; Do what i mean

(defun ac-page-next ()
  "Select next completion candidate per `ac-menu-height' range.
Pages down through completion menu."
  (interactive)
  (let ((counter 0))
    (dotimes (counter (1- ac-menu-height))
      (ac-next))))

(defun ac-page-previous ()
  "Select previous completion candidate per `ac-menu-height' range.
Pages up through completion menu."
  (interactive)
  (let ((counter 0))
    (dotimes (counter (1- ac-menu-height))
      (ac-previous))))

;;=============================================================================

(provide 'completition-conf)
