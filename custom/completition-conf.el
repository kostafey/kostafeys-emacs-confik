(require 'elpa-conf)

;;=============================================================================
;; Yet Another Snippet extension
;;=============================================================================
(require 'yasnippet)
;; personal snippets
(setq yas-snippet-dirs
    (append yas-snippet-dirs 
            (list (expand-file-name "mysnippets" custom-conf-lisp-path))))

(yas-global-mode 1)

(defun yas/next-field-or-maybe-expand-1 ()
  (interactive)
  (let ((yas/fallback-behavior 'return-nil))
    (unless (yas/expand)
      (yas/next-field))))

(defun open-line-or-yas ()
  (interactive)
  (cond ((and (looking-back " ") (looking-at "[\s\n}]+"))
     (insert "\n\n")
     (indent-according-to-mode)
     (previous-line)
     (indent-according-to-mode))
    ((expand-abbrev))
    (t 
     (setq *yas-invokation-point* (point))
     (yas/next-field-or-maybe-expand-1))))

;;=============================================================================
;; auto-complete
;;=============================================================================

(require 'auto-complete-config)
(ac-config-default)

(global-auto-complete-mode t)
;; if a length of a word you entered is larger than the value,
;; completion will be started automatically
(setq ac-auto-start 2)
(setq ac-dwim t)               ; Do what i mean

(setq ac-etags-requires 2)
(eval-after-load "etags"
  '(progn
     (ac-etags-setup)
     (defun etags-toggle-enable ()
       (interactive)
       (if (member 'ac-source-etags ac-sources)
           (progn
             (setq ac-sources (remove 'ac-source-etags ac-sources))
             (message "etags autocomplete off."))
         (progn
           (add-to-list 'ac-sources 'ac-source-etags)
           (message "etags autocomplete on."))))
     ;; (defun my-java-ac-mode-common-hook ()
     ;;   (add-to-list 'ac-sources 'ac-source-etags))
     ;; (add-hook 'java-mode-hook 'my-java-ac-mode-common-hook)
     ))

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

;; minibuffer autocompletition.
(require 'icomplete+)
(icomplete-mode)

(provide 'completition-conf)
