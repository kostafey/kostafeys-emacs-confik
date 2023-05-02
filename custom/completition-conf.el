(require 'elpa-conf)

;;===================================================================
;; Yet Another Snippet extension
;;
(use-elpa 'yasnippet)
;; personal snippets
(setq yas-snippet-dirs
    (append yas-snippet-dirs
            (list "~/.emacs.d/custom/mysnippets")))

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

;;===================================================================
;; auto-complete
;;
(use-elpa 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode nil)

;; if a length of a word you entered is larger than the value,
;; completion will be started automatically
(setq ac-auto-start 2)
(setq ac-dwim t)               ; Do what i mean

(use-elpa 'ac-etags)
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
           (message "etags autocomplete on."))))))

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
;; company-mode
;;
(use-elpa 'company)

;; The minimum prefix length for idle completion.
(setq company-minimum-prefix-length 1)

(defun k/company-select-next (&optional arg)
  "Select the next candidate in the list.

With ARG, move by that many elements."
  (interactive "p")
  (when (company-manual-begin)
    (if (= company-selection (1- company-candidates-length))
        (company-set-selection 0)
      (company-set-selection (+ (or arg 1) company-selection)))))

(defun k/company-select-previous (&optional arg)
  "Select the previous candidate in the list.

With ARG, move by that many elements."
  (interactive "p")
  (if (= company-selection 0)
      (company-select-next (1- company-candidates-length))
      (company-select-next (if arg (- arg) -1))))

;; Setting up similar to `auto-complete-mode' behavior:
;; https://github.com/company-mode/company-mode/wiki/Switching-from-AC

(setq company-require-match 'never)
(setq company-auto-complete t)

(defun my-company-visible-and-explicit-action-p ()
  (and (company-tooltip-visible-p)
       (company-explicit-action-p)))

(defun company-ac-setup ()
  "Sets up `company-mode' to behave similarly to `auto-complete-mode'."
  (setq company-require-match nil)
  (setq company-auto-complete #'my-company-visible-and-explicit-action-p))

(company-ac-setup)

;; Company quickhelp
(use-elpa 'company-quickhelp)
(company-quickhelp-mode)
(eval-after-load 'company
  '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))

;;=============================================================================
;; common completion functions
;;
(defcustom k/complete-frontend 'company-mode
  "Selected completion frontend."
  :type '(choice
          (const :tag "auto-complete" :value auto-complete)
          (const :tag "company-mode" :value company-mode)))

(case k/complete-frontend
  ('auto-complete
   (global-auto-complete-mode t))
  ('company-mode
   (progn
     (add-hook 'after-init-hook 'global-company-mode)
     (company-quickhelp-mode))))

(defun start-complete ()
  "Start `company-mode' or `auto-complete-mode' completion."
  (interactive)
  (cond ((bound-and-true-p auto-complete-mode) (auto-complete))
        ((bound-and-true-p company-mode) (company-complete))))

(defun switch-completion-frontend ()
  "Switch between `company-mode' and `auto-complete-mode' completion frontends."
  (interactive)
  (cl-labels ((switch-to-company
               ()
               (auto-complete-mode -1)
               (company-mode t)
               (message (format
                         "%s enabled"
                         (propertize "company-mode"
                                     'face 'font-lock-keyword-face))))
              (switch-to-auto-complete
               ()
               (company-mode -1)
               (auto-complete-mode t)
               (message (format
                         "%s enabled"
                         (propertize "auto-complete-mode"
                                     'face 'font-lock-keyword-face)))))
    (cond ((bound-and-true-p auto-complete-mode)
           (switch-to-company))
          ((bound-and-true-p company-mode)
           (switch-to-auto-complete))
          (t
           (switch-to-company)))))

(provide 'completition-conf)
