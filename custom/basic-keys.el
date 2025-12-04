;;; basic-keys.el --- Basic keybindings & custom configuration.

;; No third-party dependencies.
(require 'redo)
(require 'basic-text-editing)
(require 'file-ops)
(require 'history-conf)
(require 'pager)

;;-------------------------------------------------------------------
;; Exit & iconify emacs
(global-set-key (kbd "M-z") 'iconify-or-deiconify-frame)    ; Hide emacs frame
(global-set-key (kbd "M-<f4>") 'save-buffers-kill-terminal)
(global-set-key [escape] 'keyboard-quit)

;;-------------------------------------------------------------------
;; CUA - the core of the emacs humane ;)
(require 'cua-base)
(cua-mode t)
(setq cua-prefix-override-inhibit-delay 0.1)

;; Region selection:
(setq transient-mark-mode t)

(global-set-key (kbd "C-S-v") 'cua-paste-pop)
(global-set-key (kbd "C-M-v") #'(lambda() (interactive) (cua-paste-pop -1)))

(defun k/kill-and-copy-whole-line ()
  "Move a whole line to the kill-ring."
  (interactive)
  (kill-region (line-beginning-position) (line-end-position))
  (delete-char 1))

(global-set-key (kbd "C-M-x") 'k/kill-and-copy-whole-line)
(define-key emacs-lisp-mode-map (kbd "C-M-x") 'k/kill-and-copy-whole-line)

(global-set-key (kbd "C-e") 'cua-exchange-point-and-mark)
(global-set-key (kbd "C-a") 'mark-whole-buffer)

(defun region-selection-length ()
  (if (not mark-active)
      0
    (let ((beg (min (region-beginning)
                    (region-end)))
          (end (max (region-beginning)
                    (region-end))))
      (- end beg))))

(defun region-selection-count-lines ()
  (if (not mark-active)
      0
    (let ((beg (min (region-beginning)
                    (region-end)))
          (end (max (region-beginning)
                    (region-end))))
      (count-lines beg end))))

;;;###autoload
(defun hard-rewrite-mode ()
  "Workaround for the case when selected text not replaced by insertions."
  (interactive)
  (defadvice cua-paste (before k/cua-paste activate)
    (if mark-active
        (delete-region (point) (mark))))
  (defadvice self-insert-command (before k/self-insert-command activate)
    (if mark-active
        (delete-region (point) (mark))))
  (message (format
            "Hard %s mode enabled."
            (propertize "rewrite"
			                  'face 'font-lock-keyword-face))))

;;-------------------------------------------------------------------
;; Undo & redo
(global-unset-key "\C-_")

(global-set-key (kbd "C-z") 'undo)          ; Undo C-z
(global-set-key [(meta backspace)] 'undo)
(global-set-key (kbd "C-S-z") 'redo)       ; Redo C-S-z

(global-set-key (kbd "C-q") 'quoted-insert)
(global-set-key [(delete)] 'delete-char)
;; (global-set-key (kbd "M-SPC") 'just-one-space) - default
(global-set-key (kbd "C-c SPC") 'just-one-space)
(global-set-key (kbd "C-<delete>") 'just-one-space)

;;-------------------------------------------------------------------
;; Save & revert
(global-set-key (kbd "C-s") 'save-buffer)
;; Cancel all changes from last save
(global-set-key (kbd "C-x r") 'revert-buffer)
(global-set-key (kbd "C-x RET r") 'revert-buffer-with-coding-system)

;;-------------------------------------------------------------------
;; Odinary C-<right>, C-<left> movements
;;
(defun k/select ()
  (if (not mark-active)
      (cua-set-mark)))

(defun k/deselect ()
  (if (not cua--rectangle)
      (setq deactivate-mark t)))

(defun k/step-forward-word ()
  "Like odinary editors, C-<right> moves forward word."
  (skip-chars-forward " \t")
  (forward-same-syntax 1))

(defun k/step-backward-word ()
  "Like odinary editors, C-<left> moves backward word."
  (skip-chars-backward " \t")
  (forward-same-syntax -1))

(defun k/line-next (&optional select) (interactive)
       (if select (k/select) (k/deselect)) (line-move 1))
(defun k/line-previous (&optional select) (interactive)
       (if select (k/select) (k/deselect)) (line-move -1))

(defun k/char-forward (&optional select) (interactive)
       (if select (k/select) (k/deselect)) (right-char 1))
(defun k/char-backward (&optional select) (interactive)
       (if select (k/select) (k/deselect)) (left-char 1))

(defun k/word-forward (&optional select) (interactive)
       (if select (k/select) (k/deselect)) (k/step-forward-word))
(defun k/word-backward (&optional select) (interactive)
       (if select (k/select) (k/deselect)) (k/step-backward-word))

(defun k/sexp-forward (&optional select) (interactive)
       (if select (k/select) (k/deselect)) (forward-sexp 1))
(defun k/sexp-backward (&optional select) (interactive)
       (if select (k/select) (k/deselect)) (backward-sexp 1))

(defun k/line-beginning (&optional select) (interactive)
       (if select (k/select) (k/deselect)) (beginning-of-line))
(defun k/line-end (&optional select) (interactive)
       (if select (k/select) (k/deselect)) (end-of-line))

(defun k/buffer-beginning (&optional select) (interactive)
       (if select (k/select) (k/deselect)) (goto-char (point-min)))
(defun k/buffer-end (&optional select) (interactive)
       (if select (k/select) (k/deselect)) (goto-char (point-max)))

(global-set-key (kbd "<up>")          'k/line-previous)
(global-set-key (kbd "<down>")        'k/line-next)
(global-set-key (kbd "S-<up>")        #'(lambda () (interactive) (k/line-previous t)))
(global-set-key (kbd "S-<down>")      #'(lambda () (interactive) (k/line-next t)))

(global-set-key (kbd "<right>")       'k/char-forward)
(global-set-key (kbd "<left>")        'k/char-backward)
(global-set-key (kbd "S-<right>")     #'(lambda () (interactive) (k/char-forward t)))
(global-set-key (kbd "S-<left>")      #'(lambda () (interactive) (k/char-backward t)))

(global-set-key (kbd "C-<right>")     'k/word-forward)
(global-set-key (kbd "C-<left>")      'k/word-backward)
(global-set-key (kbd "C-S-<right>")   #'(lambda () (interactive) (k/word-forward t)))
(global-set-key (kbd "C-S-<left>")    #'(lambda () (interactive) (k/word-backward t)))

(global-set-key (kbd "C-M-<right>")   'k/sexp-forward)
(global-set-key (kbd "C-M-<left>")    'k/sexp-backward)
(global-set-key (kbd "C-M-S-<right>") #'(lambda () (interactive) (k/sexp-forward t)))
(global-set-key (kbd "C-M-S-<left>")  #'(lambda () (interactive) (k/sexp-backward t)))

(global-set-key (kbd "<end>")         'k/line-end)
(global-set-key (kbd "<home>")        'k/line-beginning)
(global-set-key (kbd "S-<end>")       #'(lambda () (interactive) (k/line-end t)))
(global-set-key (kbd "S-<home>")      #'(lambda () (interactive) (k/line-beginning t)))

(global-set-key (kbd "C-<home>")      'k/buffer-beginning)
(global-set-key (kbd "C-<end>")       'k/buffer-end)
(global-set-key (kbd "C-S-<home>")    #'(lambda () (interactive) (k/buffer-beginning t)))
(global-set-key (kbd "C-S-<end>")     #'(lambda () (interactive) (k/buffer-end t)))

(global-set-key (kbd "C-s-<down>")    'forward-sentence)
(global-set-key (kbd "C-s-<up>")      'backward-sentence)

;;-----------------------------------------------------------------------------
;; Eldoc
(require 'eldoc)
;; Run ElDoc after this commands:
(mapc 'eldoc-add-command '(k/char-forward
                           k/char-backward
                           k/word-forward
                           k/word-backward
                           k/sexp-forward
                           k/sexp-backward
                           k/line-next
                           k/line-previous))

;;-----------------------------------------------------------------------------
;; cua-mode in org-mode
(eval-after-load "org"
  '(progn
    (define-key org-mode-map (kbd "S-<left>") nil)
    (define-key org-mode-map (kbd "S-<right>") nil)
    (define-key org-mode-map (kbd "C-S-<left>") nil)
    (define-key org-mode-map (kbd "C-S-<right>") nil)
    (define-key org-mode-map (kbd "C-S-M-<left>") nil)
    (define-key org-mode-map (kbd "C-S-M-<right>") nil)
    (define-key org-mode-map (kbd "S-<up>") nil)
    (define-key org-mode-map (kbd "S-<down>") nil)
    (define-key org-mode-map (kbd "M-<up>") nil)
    (define-key org-mode-map (kbd "M-<down>") nil)
    (define-key org-mode-map (kbd "M-<left>") nil)
    (define-key org-mode-map (kbd "M-<right>") nil)
    (define-key org-mode-map (kbd "C-c C-p") 'k/el-pprint-eval-last-sexp)
    (define-key org-mode-map (kbd "C-a") nil)
    (define-key org-mode-map (kbd "M-a") nil)
    (define-key org-mode-map (kbd "C-j") 'join-next-line-space-n)))

;;===================================================================
;; Scrolling
;;
;; Scrolling without point movement
(global-set-key (kbd "C-<down>") 'scroll-up-line)
(global-set-key (kbd "C-<up>") 'scroll-down-line)

(global-set-key (kbd "M-g") 'goto-line)

(defun copy-to-clipboard-buffer-line-number ()
  (interactive)
  "Copy current line number to the clipboard."
  (let ((result (kill-new
                 (number-to-string
                  (line-number-at-pos (point))))))
    (message result)
    result))

(global-set-key (kbd "C-M-g g") 'copy-to-clipboard-buffer-line-number)

;; Bind scrolling functions from pager library.
(global-set-key [next]     'pager-page-down)
(global-set-key [prior]    'pager-page-up)

(setq next-screen-context-lines 10)     ; Number of lines of continuity when
                                        ; scrolling by screenfuls.

;; keyboard
(setq scroll-step 1)

;; If point moves off-screen, redisplay will scroll by up to
;; `scroll-conservatively' lines in order to bring point just barely
;; onto the screen again.
(setq scroll-conservatively 50)
;; Point keeps its screen position if the scroll command moved it
;; vertically out of the window, e.g. when scrolling by full screens.
(setq scroll-preserve-screen-position t)
;; Trigger automatic scrolling whenever point gets within this many lines
;; of the top or bottom of the window.
(setq scroll-margin 0)

;; mouse
(setq mouse-wheel-mode t)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-drag-copy-region nil)

(if (eq system-type 'gnu/linux)
    (progn
      (defun smooth-scroll (increment)
        (scroll-up increment) (sit-for 0.05)
        (scroll-up increment))
      (global-set-key [(mouse-5)] #'(lambda () (interactive) (smooth-scroll 1)))
      (global-set-key [(mouse-4)] #'(lambda () (interactive) (smooth-scroll -1)))))

;;-------------------------------------------------------------------
;; sgml-mode
(require 'sgml-mode nil 'noerror)

(defvar k/sgml-tags (list "<" ">"))

(defun k/sgml-skip-tag-forward (&optional select)
  (interactive)
  (if select
      (k/select)
    (k/deselect))
  (if (member (string (following-char)) k/sgml-tags)
      (sgml-skip-tag-forward 1)
    (sgml-forward-sexp 1)))

(defun k/sgml-skip-tag-backward (&optional select)
  (interactive)
  (if select
      (k/select)
    (k/deselect))
  (if (member (string (preceding-char)) k/sgml-tags)
      (sgml-skip-tag-backward 1)
    (sgml-forward-sexp -1)))

(defun k/sgml-skip-tag-forward-select ()
  (interactive)
  (k/sgml-skip-tag-forward t))

(defun k/sgml-skip-tag-backward-select ()
  (interactive)
  (k/sgml-skip-tag-backward t))

;; html/xml tags navigation
(defun k/define-xml-jumps (mode-map)
  ;; (require 'sgml-mode)
  (define-key mode-map (kbd "C-M-<right>") 'k/sgml-skip-tag-forward)
  (define-key mode-map (kbd "C-M-<left>") 'k/sgml-skip-tag-backward)
  (define-key mode-map (kbd "C-M-S-<right>") 'k/sgml-skip-tag-forward-select)
  (define-key mode-map (kbd "C-M-S-<left>") 'k/sgml-skip-tag-backward-select))

(defun kostafey-html-mode-hook ()
  (k/define-xml-jumps html-mode-map))

(defun kostafey-nxml-mode-hook ()
  (k/define-xml-jumps nxml-mode-map))

(add-hook 'html-mode-hook 'kostafey-html-mode-hook)
(add-hook 'nxml-mode-hook 'kostafey-nxml-mode-hook)

;;===================================================================
;;                         Point hyper-jumps
;;
;;-------------------------------------------------------------------
;; Bookmarks
;;
(global-set-key (kbd "C-S-b") 'bookmark-set)
(global-set-key (kbd "C-b") 'bookmark-jump)
(global-set-key (kbd "M-b") 'bookmark-delete)
(global-set-key (kbd "C-c b") 'bookmark-delete)

;;-------------------------------------------------------------------
;; Search & replace
;;
(global-unset-key (kbd "C-f"))
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward)
;;(global-set-key (kbd "M-e") 'isearch-edit-string) - default

(global-set-key (kbd "C-c M-R") 'replace-regexp)
(global-set-key (kbd "M-R") 'query-replace)
(global-unset-key (kbd "C-M-a"))
(global-set-key (kbd "C-M-a k") 'keep-lines)
(global-set-key (kbd "C-M-a f") 'flush-lines)

(defun k/isearch-key (key)
  (isearch-done)
  (execute-kbd-macro (kbd key)))

(defun k/isearch-ret () (interactive) (k/isearch-key "RET"))
(defun k/isearch-down () (interactive) (k/isearch-key "<down>"))
(defun k/isearch-up () (interactive) (k/isearch-key "<up>"))

(defun k/isearch-mode-hook ()
  (define-key isearch-mode-map (kbd "C-f")    'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "C-r")    'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "C-v")    'isearch-yank-kill)
  (define-key isearch-mode-map (kbd "RET")    'k/isearch-ret)
  (define-key isearch-mode-map (kbd "<down>") 'k/isearch-down)
  (define-key isearch-mode-map (kbd "<up>")   'k/isearch-up))

(add-hook 'isearch-mode-hook 'k/isearch-mode-hook)

(global-unset-key (kbd "M-r"))
(global-set-key (kbd "M-r") 'replace-string)

;;===================================================================
;; Rotate windows if more than 2 of them
;;
(defun swap-buffers (w1 w2)
  (let ((b1 (window-buffer w1))
        (b2 (window-buffer w2))
        (s1 (window-start w1))
        (s2 (window-start w2)))
    (set-window-buffer w1 b2)
    (set-window-buffer w2 b1)
    (set-window-start w1 s2)
    (set-window-start w2 s1)))

(defalias 'first 'car)
(defalias 'second 'cadr)

(defun swap-windows ()
  "If you have 2 windows or 2 frames, it swaps them."
  (interactive)
  (cond ((= (count-windows) 2)
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list))))
           (swap-buffers w1 w2)))
        ((= (length (frame-list)) 2)
         (let* ((w1 (first (window-list (first (frame-list)))))
                (w2 (first (window-list (second (frame-list))))))
           (swap-buffers w1 w2)))
        (t
         (message "You need exactly 2 windows or frames to do this."))))

(defun mirror-window ()
  "Show the same buffer in the second window as in the active window."
  (interactive)
  (let ((mirror #'(lambda ()
                    (let* ((w1 (first (window-list)))
                           (w2 (second (window-list)))
                           (b1 (window-buffer w1))
                           (s1 (window-start w1)))
                      (set-window-start w2 s1)
                      (set-window-buffer w2 b1))) ))
    (cond ((= (count-windows) 1)
           (progn
             (split-window-right)
             (funcall mirror)))
          ((= (count-windows) 2)
           (funcall mirror))
          (t
           (message "You need exactly 2 windows to do this.")))))

;;===================================================================
;; Windows navigation
;;
(global-set-key [M-f2] 'swap-windows)
(global-set-key (kbd "C-u") 'swap-windows)

(global-unset-key (kbd "M-m"))
(global-set-key (kbd "M-m") 'mirror-window)

(global-set-key [(control tab)] 'other-window) ; C-tab switchs to a next window
(windmove-default-keybindings 'meta)           ; M-up/down/left/right

;;===================================================================
;; Switch frame
;; C-x 5 o - default
(global-set-key (kbd "s-<tab>") ' other-frame)

;;===================================================================
;; Jump back to the last position of the cursor
;;
(when (fboundp 'winner-mode)
  (winner-mode 1))

(defun meta-left ()
  (interactive)
  (condition-case err
      (windmove-left)
    (error
     (if (equal err '(error "No window left from selected window"))
         (progn
           (hop-backward)
           (setq this-command 'hop-backward))
       (message "%s" err)))))

(defun meta-right ()
  (interactive)
  (condition-case err
      (windmove-right)
    (error
     (if (equal err '(error "No window right from selected window"))
         (progn
           (hop-forward)
           (setq this-command 'hop-forward))
       (message "%s" err)))))

(global-set-key (kbd "M-<left>") 'meta-left)
(global-set-key (kbd "M-<right>") 'meta-right)

;;===================================================================
;;                        Text transformations
;
;; Line operations
(global-set-key (kbd "C-n") 'open-line)
(global-set-key (kbd "C-j") 'join-next-line-space-n)
(global-set-key (kbd "C-c j") 'join-next-line-n)
(global-set-key (kbd "C-c d") 'duplicate-line)

(defun k/kill-whole-line ()
  "Deletes a whole line, but does not put it in the kill-ring."
  (interactive)
  (delete-region (line-beginning-position) (line-end-position))
  (delete-char 1))

(defun k/kill-line ()
  "Deletes a line, but does not put it in the kill-ring."
  (interactive)
  (delete-region (point) (line-end-position)))

(global-set-key (kbd "C-c c") 'center-line)
(global-set-key (kbd "C-M-k") 'k/kill-whole-line)
(global-set-key (kbd "C-k") 'k/kill-line)

(global-set-key (kbd "C-;") 'comment-or-uncomment-this)
(global-set-key (kbd "C-/") 'comment-or-uncomment-this)

;;-------------------------------------------------------------------
;; Marks & select a line
;;
(defun mark-line (&optional arg)
  "Marks a line from start of indentation to end"
  (interactive "p")
  (if (not mark-active)
      (progn
        (back-to-indentation)
        (cua-set-mark)
        (end-of-line arg))
    (progn
      (if (not (eq (line-beginning-position) (point)))
          (progn
            (setq mark-active nil)
            (beginning-of-line)
            (cua-set-mark)
            (setq mark-active t)))
      (end-of-line)
      (forward-line)
      (beginning-of-line))))

(global-set-key (kbd "C-S-l") 'mark-line)

(defun copy-line (&optional arg)
  "Kills a line, not including leading indentation"
  (interactive "p")
  (save-excursion
    (mark-line arg)
    (kill-ring-save (point) (mark))))

(defun copy-simple (beg end)
  "Save the current region to the kill ring after stripping extra whitespace and new lines"
  (interactive "r")
  (if (not mark-active)
      (copy-line)
    (copy-region-as-kill beg end)
    (with-temp-buffer
      (yank)
      (goto-char 0)
      (while (looking-at "[ \t\n]")
        (delete-char 1))
      (compact-uncompact-block)
      (mark-whole-buffer)
      (kill-region (point-min) (point-max)))))

(defun compact-uncompact-block ()
  "Remove or add line ending chars on current paragraph.
This command is similar to a toggle of `fill-paragraph'.
When there is a text selection, act on the region."
  (interactive)

  ;; This command symbol has a property “'stateIsCompact-p”.
  (let (currentStateIsCompact (bigFillColumnVal 4333999) (deactivate-mark nil))

    (save-excursion
      ;; Determine whether the text is currently compact.
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil) ) )

      (if (region-active-p)
          (if currentStateIsCompact
              (fill-region (region-beginning) (region-end))
            (let ((fill-column bigFillColumnVal))
              (fill-region (region-beginning) (region-end))) )
        (if currentStateIsCompact
            (fill-paragraph nil)
          (let ((fill-column bigFillColumnVal))
            (fill-paragraph nil)) ) )

      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)))))

(global-set-key (kbd "C-S-c") 'copy-line)
(global-set-key (kbd "C-M-c") 'copy-simple)

(defun copy-url (&optional arg)
  "Copy a url under the cursor"
  (interactive "p")
  (let* ((beg (save-excursion
                (search-backward " " nil t arg)
                (right-char)
                (point)))
         (end (save-excursion
                (end-of-line)
                (point)))
         (url (buffer-substring beg end)))
    (kill-new url)
    (message (concat "Copied to buffer: " url))))

(global-set-key (kbd "C-c u") 'copy-url)

;;-------------------------------------------------------------------
;; Long lines
;; do not truncate and wrap long lines
(setq truncate-partial-width-windows nil)
(setq truncate-lines nil)
;; Toggle whether to fold or truncate long lines for the current buffer.
(global-set-key (kbd "C-c C-l") 'toggle-truncate-lines)

;;-------------------------------------------------------------------
;; Paragraph operations

(global-set-key (kbd "C-c q")  'unfill-paragraph)

;;-------------------------------------------------------------------
;; Word operations
(global-set-key (kbd "M-t") 'words-transpose)
(global-set-key (kbd "M-y") #'(lambda() (interactive) (transpose-words -1)))

;;-------------------------------------------------------------------
;; Rectangle operations

(global-set-key (kbd "C-M-a c") 'copy-rectangle-to-clipboard)
(global-set-key (kbd "C-M-a n") 'rectangle-number-lines)
;(global-set-key (kbd "M-u") 'cua-upcase-rectangle) - default

;;-------------------------------------------------------------------
;; Upcase/downcase

(global-set-key (kbd "C-S-<up>") 'toggle-letter-case)
(global-set-key (kbd "C-S-<down>") 'toggle-date-or-camelcase-underscores)

;; Enable case changes commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(global-set-key (kbd "C-M-a l") 'downcase-region)
(global-set-key (kbd "C-M-a d") 'downcase-region)
(global-set-key (kbd "C-M-a u") 'upcase-region)

;;-----------------------------------------------------------------------------
;; Region & misc operations
(global-set-key (kbd "C-M-a :") 'align-by-column)
(global-set-key (kbd "C-M-a '") 'align-by-quote)
(global-set-key (kbd "C-M-a a") 'align-regexp)

(global-set-key (kbd "C-`") 'u:en/ru-recode-region)

;;=============================================================================
;; Look changes
;;
(global-set-key (kbd "M-RET") 'toggle-fullscreen)

;;=============================================================================
;; Gathering information
;;
(global-set-key (kbd "C-?") 'describe-char)
(global-set-key (kbd "C-M-a C-c") 'count-words-region)

(global-set-key (kbd "M-p") 'copy-to-clipboard-buffer-file-path)
(global-set-key (kbd "M-f") 'copy-to-clipboard-buffer-file-name)

(defun copy-to-clipboard-git-branch ()
  (interactive)
  "Copy current branch name to the clipboard."
  (let* ((branch (car (vc-git-branches)))
         (result (kill-new branch)))
    (message result)
    result))

(global-set-key (kbd "C-p") 'copy-to-clipboard-git-branch)

;;===================================================================
;; Buffers navigation
;;
(global-set-key (kbd "C-x C-f") 'find-file)
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "C-c f") 'choose-from-recentf)
(global-set-key (kbd "C-o")   ; the plain prompt for file path
                #'(lambda () (interactive)
                    (find-file (read-from-minibuffer "Enter file path: "))))
(global-set-key (kbd "C-x C-r") 'sudo-edit)
(global-set-key (kbd "C-x b") 'switch-to-buffer)

;; ibuffer - list of all buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; (require 'bs) ;; other list of buffers
;; (global-set-key (kbd "C-x C-n") 'bs-show)

(global-set-key (kbd "C-x w") 'kill-buffer)
(global-set-key (kbd "C-w") 'kill-buffer)

;;-------------------------------------------------------------------
;; Kill or create buffer(s)
;;
(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(defun kill-special-buffers ()
  "Kill special buffers but the current one."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (buffer-file-name buffer))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer)))))

(defun find-file-from-clipboard ()
  "Open file or directory path from clipboard (kill ring) if path exists."
  (interactive)
  (let ((file-path (current-kill 0)))
    (if (file-exists-p file-path)
        (find-file file-path)
      (message "Can't find file '%s'" file-path))))

(global-set-key (kbd "C-c w") 'kill-other-buffers)
(global-set-key (kbd "C-c C-w") 'kill-special-buffers)
(global-set-key (kbd "C-x a s") 'find-file-from-clipboard)
(global-set-key (kbd "C-c k") 'delete-this-buffer-and-file)

;;-------------------------------------------------------------------
;; Switch buffers
;;
(if (require 'tabbar nil 'noerror)
    (progn
      (global-set-key (kbd "C-<next>") 'tabbar-forward-tab)
      (global-set-key (kbd "C-<prior>") 'tabbar-backward-tab))
  (progn
    (global-set-key (kbd "C-<next>") 'next-buffer)
    (global-set-key (kbd "C-<prior>") 'previous-buffer)))

;;-------------------------------------------------------------------
;; buffers shortcuts
(global-set-key (kbd "C-M-a e")
                #'(lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-x m")
                #'(lambda () (interactive) (switch-to-buffer "*Messages*")))

;;-------------------------------------------------------------------
;; File variables
(setq safe-local-variable-values '((scala-indent:step . 2)))

;;===================================================================
;;                               Mouse
;;
;; Select by mouse and shift
;;-------------------------------------------------------------------
;; shift + click select region
(define-key global-map (kbd "<S-down-mouse-1>") 'ignore) ; turn off font dialog
(define-key global-map (kbd "<S-mouse-1>") #'(lambda (e)
                                              (interactive "e")
                                              (if (not mark-active)
                                                  (cua-set-mark))
                                              (mouse-set-point e)))

(provide 'basic-keys)
