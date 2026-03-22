(straight-use-package
 '(auto-complete :type git :host github
				         :repo "auto-complete/auto-complete" :branch "master"))
(straight-use-package
 '(ac-etags :type git :host github
				    :repo "emacsorphanage/ac-etags" :branch "master"))
(straight-use-package
 '(company :type git :host github
				   :repo "company-mode/company-mode" :branch "master"))
(straight-use-package
 '(company-quickhelp :type git :host github
				             :repo "company-mode/company-quickhelp" :branch "master"))
(straight-use-package
 '(company-fuzzy :type git :host github
				         :repo "jcs-elpa/company-fuzzy" :branch "master"))

;;===================================================================
;; auto-complete
;;
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode -1)

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

(define-key ac-complete-mode-map [next] 'ac-page-next)
(define-key ac-complete-mode-map [prior] 'ac-page-previous)
(define-key ac-complete-mode-map (kbd "C-f") 'ac-isearch)

;;=============================================================================
;; company-mode
;;
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
(company-quickhelp-mode)

(defun k/company-complete-selection ()
  (interactive)
  (if (eq major-mode 'eshell-mode)
      (eshell-send-input)
    (company-complete-selection)))

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)
     ;; Disable complete on RET for `eshell-mode'.
     (define-key company-active-map (kbd "RET") #'k/company-complete-selection)
     (define-key company-active-map (kbd "<return>") #'k/company-complete-selection)))

(use-package company-fuzzy
  ;; :hook (company-mode . company-fuzzy-mode)
  :init
  (setq company-fuzzy-sorting-backend 'flx
        company-fuzzy-prefix-on-top nil
        company-fuzzy-show-annotation t
        company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@")
        global-company-fuzzy-mode nil))

(define-key company-search-map (kbd "<escape>") 'company-search-abort)
(define-key company-active-map (kbd "<escape>") 'company-abort)
(define-key company-active-map (kbd "<left>") #'(lambda() (interactive)
                                                  (company-abort)
                                                  (k/char-backward)))
(define-key company-active-map (kbd "<right>") #'(lambda() (interactive)
                                                   (company-abort)
                                                   (k/char-forward)))
(define-key company-active-map (kbd "C-<left>") #'(lambda() (interactive)
                                                    (company-abort)
                                                    (k/word-backward)))
(define-key company-active-map (kbd "C-<right>") #'(lambda() (interactive)
                                                     (company-abort)
                                                     (k/word-forward)))
(define-key company-active-map (kbd "<up>") 'k/company-select-previous)
(define-key company-active-map (kbd "<down>") 'k/company-select-next)
(define-key company-active-map [next] 'company-next-page)
(define-key company-active-map [prior] 'company-previous-page)
(define-key company-active-map (kbd "C-f") 'company-search-candidates)
(define-key company-active-map (kbd "<tab>") 'company-complete-selection)

;;=============================================================================
;; common completion functions
;;
(defcustom k/complete-frontend 'company-mode
  "Selected completion frontend."
  :type '(choice
          (const :tag "auto-complete" :value auto-complete)
          (const :tag "company-mode" :value company-mode)))

(with-eval-after-load 'completition-conf
  (cl-case k/complete-frontend
    ('auto-complete
     (global-auto-complete-mode t))
    ('company-mode
     (progn
       (add-hook 'after-init-hook 'global-company-mode)
       (company-quickhelp-mode)))))

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
