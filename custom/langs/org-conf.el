;;; org-conf.el --- Org mode configuration.  -*- lexical-binding: t -*-

(straight-use-package 'org)

(use-package verb
  :straight '(verb :type git :host github
			             :repo "federicotdn/verb" :branch "main"))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config (progn
            (define-key org-mode-map (kbd "C-c C-r") verb-command-map)

            (defun k/org-send-at-point ()
              (interactive)
              (if (org-in-src-block-p)
                  ;; Inside a source block evaluate it (SQL goes to ejc-sql
                  ;; via `org-babel-execute:sql'), flashing its body only.
                  (let ((element (org-element-at-point)))
                    (save-excursion
                      (goto-char (org-element-property :post-affiliated element))
                      (forward-line 1)
                      (let ((beg (point)))
                        (goto-char (org-element-property :end element))
                        (skip-chars-backward " \t\n")
                        (k/flash-region beg (line-beginning-position))))
                    (if (member (org-element-property :language element)
                                k/org-babel-side-buffer-languages)
                        (k/org-babel-execute-to-side-buffer)
                      (org-ctrl-c-ctrl-c)))
                ;; Flash the request being sent: the heading at point up
                ;; to the next heading (child headings are separate requests).
                (ignore-errors
                  (save-excursion
                    (org-back-to-heading t)
                    (k/flash-region (point)
                                    (progn (outline-next-heading) (point)))))
                ;; Show the flash before sending: expanding {{...}} templates
                ;; (e.g. a `shell-command-to-string' call) blocks Emacs, so
                ;; without a forced redisplay the overlay is gone unseen.
                (redisplay t)
                (verb-kill-all-response-buffers 1)
                (verb-send-request-on-point-other-window-stay)))

            (define-key org-mode-map (kbd "C-c C-c") 'k/org-send-at-point)
            (setq org-hide-leading-stars t)
            (setq org-log-done 'nil) ; Don't show datetime on switching to done.
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((perl . t)
               (ruby . t)
               (shell  . t)
               (python . t)
               (emacs-lisp . t)))))

;; Pasting a screenshot into an Org file saves it next to the file:
;; directory/foo.org gets directory/foo/screenshot-N.png and a link to it.
;; The saving itself is Org's `yank-media' handler; only the directory and the
;; name are ours.
(defun k/org-screenshot-directory ()
  "Directory for images pasted into the current Org buffer.
It is named after the visited file without its extension."
  (if buffer-file-name
      (file-name-sans-extension buffer-file-name)
    (expand-file-name "screenshots" default-directory)))

(defun k/org-screenshot-file-name ()
  "Next free `screenshot-N' name, without extension, for a pasted image."
  (let ((dir (k/org-screenshot-directory))
        (last 0))
    (when (file-directory-p dir)
      (dolist (file (directory-files dir nil "\\`screenshot-[0-9]+\\."))
        (when (string-match "\\`screenshot-\\([0-9]+\\)\\." file)
          (setq last (max last (string-to-number (match-string 1 file)))))))
    (format "screenshot-%d" (1+ last))))

(setq org-yank-image-save-method #'k/org-screenshot-directory)
(setq org-yank-image-file-name-function #'k/org-screenshot-file-name)

(defun k/clipboard-image-only-p ()
  "Non-nil when the clipboard holds an image and no plain text.
A copy of text from a browser or an office suite offers a picture
alongside the text; that is still a text paste.  HTML alone does not
count as text: an image copied from a browser comes with an <img> tag."
  (and (display-graphic-p)
       (let ((targets (ignore-errors (gui-get-selection 'CLIPBOARD 'TARGETS))))
         (and (vectorp targets)
              (seq-some (lambda (target)
                          (string-prefix-p "image/" (symbol-name target)))
                        targets)
              (not (seq-some (lambda (target)
                               (or (memq target '(STRING UTF8_STRING TEXT
                                                  COMPOUND_TEXT))
                                   (string-prefix-p "text/plain"
                                                    (symbol-name target))))
                             targets))))))

(defun k/org-paste-dwim (&optional arg)
  "Paste a screenshot as an image file and a link, anything else as text.
The image goes to `k/org-screenshot-directory' as `screenshot-N.png';
text is pasted by `cua-paste' with ARG."
  (interactive "P")
  (if (k/clipboard-image-only-p)
      (progn
        (when (use-region-p)
          (delete-region (region-beginning) (region-end)))
        (yank-media))
    (cua-paste arg)))

;; cua-mode remaps `yank' to `cua-paste' from a minor mode map, which
;; outranks `org-mode-map', so a binding there is never consulted.  Join
;; `emulation-mode-map-alists' ahead of cua instead, switched on per
;; buffer -- the way `ghostel-conf' reclaims C-v.
(defvar-local k/org-paste-override nil
  "Non-nil where `k/org-paste-override-map' should take effect.")

(defvar k/org-paste-override-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-v") #'k/org-paste-dwim)
    map)
  "Keymap reclaiming the paste key from cua-mode inside Org buffers.")

(add-to-list 'emulation-mode-map-alists
             `((k/org-paste-override . ,k/org-paste-override-map)))

(defun k/org-enable-paste-override ()
  "Let `k/org-paste-override-map' win over cua-mode here."
  (setq-local k/org-paste-override t))

(add-hook 'org-mode-hook 'k/org-enable-paste-override)

;; Inhibit confirmation before interactively evaluating SQL and Python
;; code blocks in Org buffers.
(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("sql" "python"))))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; ob-python writes the results file of `:results value' in the locale
;; encoding, which on Windows (cp1252) fails on Cyrillic.  Run Python in
;; its UTF-8 mode for code blocks.
(defun k/org-babel-python-utf-8 (orig &rest args)
  "Call ORIG with ARGS with Python in UTF-8 mode."
  (let ((process-environment (cons "PYTHONUTF8=1" process-environment)))
    (apply orig args)))

(with-eval-after-load 'ob-python
  (advice-add 'org-babel-python-evaluate-external-process :around
              #'k/org-babel-python-utf-8))

;; Results of source blocks in these languages go to a side window
;; instead of #+RESULTS:, the way ejc-sql and verb show theirs.
(defvar k/org-babel-side-buffer-languages '("python")
  "Languages whose source block results `k/org-send-at-point' shows aside.")

(defun k/org-babel-execute-to-side-buffer ()
  "Evaluate the source block at point, show its result in a side window."
  (interactive)
  ;; `silent' replaces only the insertion type: the block's own
  ;; `output'/`value' stays.
  (let ((result (org-babel-execute-src-block
                 nil nil '((:results . "silent"))))
        (buffer (get-buffer-create "*Org Babel Result*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (and (consp result) (cl-every #'listp result))
            (progn
              (org-mode)
              (insert (orgtbl-to-orgtbl result nil))
              (org-table-align))
          (fundamental-mode)
          (insert (if (stringp result) result (format "%S" result))))
        (goto-char (point-min))
        (view-mode 1)))
    (display-buffer buffer '(nil (inhibit-same-window . t)))))

(setq org-fontify-quote-and-verse-blocks nil)

;; Fontify markdown-style `code` with `org-property-value' face.
(defun k/org-match-backtick-code (last)
  "Match the next `code' span up to LAST, skipping src/example blocks."
  (let (found)
    (while (and (not found)
                (re-search-forward "`\\([^`\n]+\\)`" last t))
      (unless (save-match-data
                (save-excursion
                  (goto-char (match-beginning 0))
                  (org-in-block-p '("src" "example" "export"))))
        (setq found t)))
    found))

(defun k/org-backtick-code-highlight-initialize ()
  (font-lock-add-keywords
   nil '((k/org-match-backtick-code . (0 'org-property-value t)))
   'append))

(add-hook 'org-mode-hook 'k/org-backtick-code-highlight-initialize)

;; Show the target of the link at point in the echo area: a link with a
;; description displays only the description, the URL stays hidden.
(defun k/org-eldoc-link (&rest _)
  "Return the target of the Org link at point, for ElDoc."
  (when-let* ((link (org-element-lineage (org-element-context) '(link) t)))
    (propertize (org-element-property :raw-link link)
                'face 'font-lock-constant-face)))

(defun k/org-eldoc-link-setup ()
  (add-hook 'eldoc-documentation-functions #'k/org-eldoc-link nil t)
  (eldoc-mode 1))

(add-hook 'org-mode-hook 'k/org-eldoc-link-setup)

;; Global cycling (`org-shifttab') reaches SHOW ALL with
;; `(org-fold-show-all '(headings blocks))', leaving drawers like
;; :PROPERTIES: folded.  Unfold them too in that state.
(defun k/org-cycle-show-drawers (state)
  "Unfold all drawers when global cycling reaches STATE `all'."
  (when (eq state 'all)
    (org-fold-show-all '(drawers))))

;; Add after load: `add-hook' on a still unbound `org-cycle-hook' would
;; keep the defcustom from installing its default functions.
(with-eval-after-load 'org-cycle
  (add-hook 'org-cycle-hook 'k/org-cycle-show-drawers))

(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)

(provide 'org-conf)
