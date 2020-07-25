;;--------------------------------------------------------------------
;; Emacs SQL client `ejc-sql'.
;;
(require 'ejc-sql)

(setq nrepl-sync-request-timeout 60)
(setq clomacs-httpd-default-port 8090) ; Use a port other than 8080.

;; Show results of SQL snippets evaluation in `org-mode'
;; in dedicated buffer.
(setq ejc-org-mode-show-results nil)
(setq ejc-use-flx t)                          ; Enable `flx' fuzzy matching.
(setq ejc-result-table-impl 'ejc-result-mode) ; Set major-mode for results.
;; (setq ejc-result-table-impl 'orgtbl-mode)  ; Default major-mode for results.

;; Since `winner-mode' is enabled and M-<arrow> keys are used for
;; windows navigation, so disable this keys for `orgtbl-mode-map'.
(define-key orgtbl-mode-map (kbd "<return>") nil)
(define-key orgtbl-mode-map (kbd "M-<left>") nil)
(define-key orgtbl-mode-map (kbd "M-<right>") nil)
(define-key orgtbl-mode-map (kbd "M-<down>") nil)
(define-key orgtbl-mode-map (kbd "M-<up>") nil)
;; Use C-M-<arrow> keys instead.
(define-key orgtbl-mode-map (kbd "C-M-<left>") 'org-table-move-column-left)
(define-key orgtbl-mode-map (kbd "C-M-<right>") 'org-table-move-column-right)
(define-key orgtbl-mode-map (kbd "C-M-<up>") 'org-table-move-row-up)
(define-key orgtbl-mode-map (kbd "C-M-<down>") 'org-table-move-row-down)
;; Add run SQL key familiar to users of PLSQL Developer.
(define-key ejc-sql-mode-keymap (kbd "<F8>") 'ejc-eval-user-sql-at-point)

(defun k/ejc-after-emacs-init-hook ()
  ;; Require completion frontend (autocomplete or company). One of them or both.
  (require 'ejc-autocomplete)
  (require 'ejc-company)
  (push 'ejc-company-backend company-backends)
  ;; In case of `company-mode' is used by default this can be useful:
  ;; (company-quickhelp-mode)
  )

(add-hook 'after-init-hook 'k/ejc-after-emacs-init-hook)

(defun k/sql-mode-hook ()
  (ejc-sql-mode t))

(add-hook 'sql-mode-hook 'k/sql-mode-hook)

(defun k/ejc-result-mode-hook ()
  (display-line-numbers-mode))

(add-hook 'ejc-result-mode-hook 'k/ejc-result-mode-hook)

(defun k/ejc-sql-mode-hook ()
  ;; Enable one of the completion frontend by by default but not both.
  (auto-complete-mode t) ; Enable `auto-complete-mode'
  (ejc-ac-setup)
  ;; (company-mode t)    ; or `company-mode'.
  (ejc-eldoc-setup)      ; Setup ElDoc.
  (font-lock-warn-todo)       ; See custom/look-and-feel.el
  (rainbow-delimiters-mode t) ; https://github.com/Fanael/rainbow-delimiters
  (idle-highlight-mode t)     ; https://github.com/nonsequitur/idle-highlight-mode
  (paredit-everywhere-mode)   ; https://github.com/purcell/paredit-everywhere
  (electric-pair-mode))

(add-hook 'ejc-sql-minor-mode-hook 'k/ejc-sql-mode-hook)

(defun k/ejc-sql-connected-hook ()
  (ejc-set-fetch-size 100)        ; Limit for the number of records to output.
  (ejc-set-max-rows 99)           ; Limit for the number of records in ResultSet.
  (ejc-set-column-width-limit 25) ; Limit for outputing the number of chars per column.
  (ejc-set-use-unicode t)         ; Use unicode symbols for grid borders.
  )

(add-hook 'ejc-sql-connected-hook 'k/ejc-sql-connected-hook)

;; Load file with actual connections configurations -
;; `ejc-create-connection' calls.
(require 'ejc-databases nil 'noerror)

(provide 'ejc-sql-conf)

;;; ejc-sql-conf.el ends here
