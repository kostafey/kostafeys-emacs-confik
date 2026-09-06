;;; Minibuffer configuration. -*- lexical-binding: t -*-

;;-------------------------------------------------------------------
;; save minibuffer history between sessions
;;
(savehist-mode t)

;;-------------------------------------------------------------------
;; vertico.el - VERTical Interactive COmpletion
;;
(use-package vertico
  :straight '(vertico
              :type git :host github
              :repo "minad/vertico" :branch "main")
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
  :straight '(vertico
              :type git :host github
              :repo "minad/vertico" :branch "main")
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package marginalia
  :straight '(marginalia
              :type git :host github
              :repo "minad/marginalia" :branch "main")
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
  :straight '(consult
              :type git :host github
              :repo "minad/consult" :branch "main")
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
         ("C-c C-f" . consult-find)     ; find file
         ;; Defined in `basic.el':
         ;; ("C-S-b" . bookmark-set)
         ;; ("C-b" . bookmark-jump) ; <f3>
         ;; ("M-b" . bookmark-delete)
         ;; ("C-c b" . bookmark-delete)
         ("<f3>" . consult-bookmark)
         ("C-b" . consult-bookmark)
         ("M-g" . consult-goto-line)
         ("C-x C-x" . consult-global-mark)))

;;-------------------------------------------------------------------
;; Kill buffers from the completion list, like `psw-switch-buffer' does.
;;
(defun k/vertico--buffer-name (cand)
  "Return the buffer name completion candidate CAND stands for, or nil.
CAND is either a plain string, the (NAME . BUFFER) cons that
`internal-complete-buffer' hands to its predicate, or a `consult--multi'
string carrying a `multi-category' property."
  (let* ((key (if (consp cand) (car cand) cand))
         (multi (and (stringp key) (> (length key) 0)
                     (get-text-property 0 'multi-category key))))
    (cond ((bufferp key) (buffer-name key))
          (multi (and (eq (car multi) 'buffer) (cdr multi)))
          ((stringp key) (substring-no-properties key)))))

(defun k/vertico--candidate-buffer ()
  "Return the live buffer the current Vertico candidate names, or nil."
  (when (memq (vertico--metadata-get 'category) '(buffer multi-category))
    (let ((cand (and (>= vertico--index 0)
                     (nth vertico--index vertico--candidates))))
      (when-let ((name (and cand (k/vertico--buffer-name cand))))
        (get-buffer name)))))

(defun k/vertico--hide-buffer (name pred)
  "Return a predicate like PRED that also rejects the buffer called NAME."
  (lambda (cand &rest args)
    (and (not (equal name (k/vertico--buffer-name cand)))
         (or (null pred) (apply pred cand args)))))

(defun k/vertico-kill-buffer ()
  "Kill the buffer named by the current candidate, keeping the list open.
On anything that is not a buffer fall back to the global binding of the
key, so C-k still kills a line and C-d still deletes a character."
  (interactive)
  (if-let ((buf (k/vertico--candidate-buffer)))
      (let ((cand (nth vertico--index vertico--candidates))
            (name (buffer-name buf)))
        (when (kill-buffer buf)
          ;; `consult-buffer' builds its candidates once, so the killed
          ;; buffer would return on the next recompute unless it is filtered
          ;; out.  `minibuffer-completion-predicate' is bound per session,
          ;; so the wrapper goes away with the session.
          (setq minibuffer-completion-predicate
                (k/vertico--hide-buffer name minibuffer-completion-predicate))
          ;; Drop it from the current list as well, so the recompute locks
          ;; onto the next candidate instead of jumping back to the top.
          (setq vertico--candidates (delq cand vertico--candidates)
                vertico--total (length vertico--candidates)
                vertico--index (min vertico--index (1- vertico--total))
                vertico--lock-candidate t
                ;; Invalidate the cache; `vertico--exhibit' on
                ;; `post-command-hook' redraws right after this command.
                vertico--input nil)))
    (when-let ((fallback (global-key-binding (this-command-keys-vector))))
      (call-interactively fallback))))

(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "C-k") #'k/vertico-kill-buffer)
  (define-key vertico-map (kbd "C-d") #'k/vertico-kill-buffer))

;; `kill-buffer' asks before killing a modified buffer, and that prompt is
;; a minibuffer inside the minibuffer.
(setq enable-recursive-minibuffers t)

(provide 'minibuffer-conf)
