;;; basic-keys.el --- Basic keybindings & custom configuration.  -*- lexical-binding: t -*-

;; No third-party dependencies.
(require 'basic-text-editing)
(require 'basic-navigation)
(require 'file-ops)
(require 'history-conf)
(require 'pager)
(require 'last-change)
(require 'web-browse)

;;-------------------------------------------------------------------
;; Exit & iconify emacs
(global-set-key (kbd "M-z") 'iconify-or-deiconify-frame)    ; Hide emacs frame
(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-k f") 'make-frame)
(global-set-key (kbd "M-<f4>") 'save-buffers-kill-terminal)
(global-set-key (kbd "<escape>") 'keyboard-quit)
;; Exit minibuffer
(define-key minibuffer-local-map (kbd "<escape>") 'abort-recursive-edit)

;;-------------------------------------------------------------------
;; CUA - the core of the emacs humane ;)
(require 'cua-base)
(cua-mode t)
(setq cua-prefix-override-inhibit-delay 0.1)

;; Region selection:
(setq transient-mark-mode t)

;; C-SPC completes; the mark is set with C-@ or by shift-selection.
(global-set-key (kbd "C-SPC") 'completion-at-point)

(global-set-key (kbd "C-S-v") 'cua-paste-pop)
(global-set-key (kbd "C-M-v") #'(lambda() (interactive) (cua-paste-pop -1)))

(global-set-key (kbd "C-S-x") 'k/kill-and-copy-whole-line)
(global-set-key (kbd "C-M-x") 'k/kill-and-copy-whole-line)
(define-key emacs-lisp-mode-map (kbd "C-M-x") 'k/kill-and-copy-whole-line)

(global-set-key (kbd "C-e") 'cua-exchange-point-and-mark)
(global-set-key (kbd "C-a") 'mark-whole-buffer)

;;-------------------------------------------------------------------
;; Undo & redo
(global-unset-key "\C-_")

;; Linear undo: `undo' (also bound to C-z by cua-mode) never undoes an undo.
(setq undo-no-redo t)
(global-set-key (kbd "C-z") 'undo)          ; Undo C-z
(global-set-key [(meta backspace)] 'undo)
(global-set-key (kbd "C-S-z") 'undo-redo)  ; Redo C-S-z
;; Jump back through the positions of recent changes
(global-set-key (kbd "C-x x") 'last-change-jump)

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
    (define-key org-mode-map (kbd "C-j") 'join-next-line-space-n)
    (define-key org-mode-map (kbd "C-x t") 'org-todo)
    (define-key org-mode-map (kbd "C-x d") 'org-open-at-point)
    (define-key org-mode-map (kbd "C-S-<up>") 'toggle-letter-case)
    (define-key org-mode-map (kbd "C-S-<down>") 'toggle-date-or-camelcase-underscores)
    (global-set-key (kbd "C-c l") 'org-store-link)
    (global-set-key (kbd "C-c a") 'org-agenda)))

;;===================================================================
;; Scrolling
;;
;; Scrolling without point movement
(global-set-key (kbd "C-<down>") 'scroll-up-line)
(global-set-key (kbd "C-<up>") 'scroll-down-line)

(global-set-key (kbd "M-g") 'goto-line)

(global-set-key (kbd "C-M-g g") 'copy-to-clipboard-buffer-line-number)

;; Bind scrolling functions from pager library.
(global-set-key [next]     'pager-page-down)
(global-set-key [prior]    'pager-page-up)

(when (eq system-type 'gnu/linux)
  (global-set-key [(mouse-5)] #'(lambda () (interactive) (smooth-scroll 1)))
  (global-set-key [(mouse-4)] #'(lambda () (interactive) (smooth-scroll -1))))

;;-------------------------------------------------------------------
;; sgml-mode
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
;; Windows navigation
;;
(global-set-key [M-f2] 'swap-windows)
(global-set-key (kbd "C-u") 'swap-windows)

(global-unset-key (kbd "M-m"))
(global-set-key (kbd "M-m") 'mirror-window)

(global-set-key [(control tab)] 'other-window) ; C-tab switchs to a next window

;;===================================================================
;; Switch frame
;; C-x 5 o - default
(global-set-key (kbd "s-<tab>") ' other-frame)

;; ------------------------------------------------------------
;; windmove
(global-set-key (kbd "M-<left>") 'meta-left)
(global-set-key (kbd "M-<right>") 'meta-right)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)

;; Resize windows
(global-set-key (kbd "s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-s-<left>") (lambda () (interactive)
                                     (shrink-window-horizontally 20)))
(global-set-key (kbd "S-s-<right>") (lambda () (interactive)
                                      (enlarge-window-horizontally 20)))
(global-set-key (kbd "s-<down>") 'shrink-window)
(global-set-key (kbd "s-<up>") 'enlarge-window)

;;===================================================================
;;                        Text transformations
;
;; Line operations
(global-set-key (kbd "C-n") 'open-line)
(global-set-key (kbd "C-j") 'join-next-line-space-n)
(global-set-key (kbd "C-c j") 'join-next-line-n)
(global-set-key (kbd "C-c d") 'duplicate-line)

(global-set-key (kbd "C-c c") 'center-line)
(global-set-key (kbd "C-M-k") 'k/kill-whole-line)
(global-set-key (kbd "C-k") 'k/kill-line)

(global-set-key (kbd "C-;") 'comment-or-uncomment-this)
(global-set-key (kbd "C-/") 'comment-or-uncomment-this)

;;-------------------------------------------------------------------
;; Marks & select a line
;;
(global-set-key (kbd "C-S-l") 'mark-line)
(global-set-key (kbd "C-S-c") 'copy-line)
(global-set-key (kbd "C-M-c") 'copy-simple)
(global-set-key (kbd "C-c u") 'copy-url)

;;-------------------------------------------------------------------
;; Long lines, see `truncate-lines' in basic-look-and-feel
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

(global-set-key (kbd "C-M-a n") 'rectangle-number-lines)
;(global-set-key (kbd "M-u") 'cua-upcase-rectangle) - default

;;-------------------------------------------------------------------
;; Upcase/downcase

(global-set-key (kbd "C-S-<up>") 'toggle-letter-case)
(global-set-key (kbd "C-S-<down>") 'toggle-date-or-camelcase-underscores)

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
(global-set-key (kbd "M-o") 'copy-file-name-and-line)
(global-set-key (kbd "M-O") 'copy-file-path-and-line)
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
(global-set-key (kbd "C-c w") 'kill-other-buffers)
(global-set-key (kbd "C-c C-w") 'kill-special-buffers)
(global-set-key (kbd "C-x a s") 'find-file-from-clipboard)
(global-set-key (kbd "C-c k") 'delete-this-buffer-and-file)

;;-------------------------------------------------------------------
;; Switch buffers
;;
(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key (kbd "C-<prior>") 'previous-buffer)

;;-------------------------------------------------------------------
;; buffers shortcuts
(global-set-key (kbd "C-M-a e")
                #'(lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-x m")
                #'(lambda () (interactive) (switch-to-buffer "*Messages*")))

;;-------------------------------------------------------------------
;; Function keys toggling modes
(global-set-key (kbd "<f9>") 'auto-fill-mode)  ; enable/disable lines auto-fill
(global-set-key (kbd "<f10>") 'smerge-mode)
(global-set-key (kbd "<f12>") 'flyspell-mode)  ; enable/disable spell checking

;;-------------------------------------------------------------------
;; Web browser, see solutions/web-browse.el
(global-set-key (kbd "C-c g") 'web-browse-google)
(global-set-key (kbd "C-c C-g") 'web-browse-google-query)
(global-set-key (kbd "C-M-w") 'web-browse-google-home)
(global-set-key (kbd "C-x u") 'web-browse-url)

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
