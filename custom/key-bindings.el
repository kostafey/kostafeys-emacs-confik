;;; key-bindings.el -- A collection of key bindings (default and custom).

;;; Commentary:

;; Attempting to make emacs a little bit humane.

;;=============================================================================
;; Exit & iconify emacs
(global-set-key (kbd "M-z") 'iconify-or-deiconify-frame)    ; Hide emacs frame
(global-set-key (kbd "M-<f4>") 'save-buffers-kill-terminal)

;;=============================================================================
;; CUA - the core of the emacs humane ;)
;;
(require 'cua-base)
(require 'hopper)
(cua-mode t)
(setq cua-prefix-override-inhibit-delay 0.1)
;;
;; Region selection:
(setq transient-mark-mode t)

;;-----------------------------------------------------------------------------
(global-set-key (kbd "C-S-v") 'cua-paste-pop)
(global-set-key (kbd "C-M-v") '(lambda() (interactive) (cua-paste-pop -1)))

(global-set-key (kbd "C-M-c") '(lambda(beg end) (interactive "r")
                                 (append-to-buffer "temp" beg end)
                                 (save-excursion
                                   (set-buffer "temp")
                                   (insert "\n"))))
;;-----------------------------------------------------------------------------
(global-set-key (kbd "C-e") 'cua-exchange-point-and-mark)
(global-set-key (kbd "C-S-e") '(lambda() (interactive) (cua-exchange-point-and-mark 1)))
;;-----------------------------------------------------------------------------
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "M-s") 'set-mark-command)
;;-----------------------------------------------------------------------------

(require 'redo)
(global-unset-key "\C-_")

(global-set-key (kbd "C-z") 'undo)              ; Undo C-z
(global-set-key [(meta backspace)] 'undo)
(global-set-key (kbd "C-S-z") 'redo)            ; Redo C-S-z

(global-unset-key (kbd "C-'"))
(global-set-key (kbd "C-'") 'repeat)

(global-set-key "\C-b" 'backward-delete-char)
(global-set-key "\C-d" 'delete-char)            ; delete
(global-set-key "\C-q" 'quoted-insert)
(global-set-key [(delete)] 'delete-char)

;;-----------------------------------------------------------------------------
(global-set-key (kbd "C-s") 'save-buffer)
;; Cancel all changes from last save
(global-set-key (kbd "C-x r") 'revert-buffer)
(global-set-key (kbd "C-x RET r") 'revert-buffer-with-coding-system)

;;=============================================================================
;; Keyboard prefixes
;;
(global-unset-key "\C-\M-a")
(defvar selected-area-prefix "\C-\M-a")
(defvar change-buffer-prefix "\C-c\C-b")

(require 'buffer-navigation)
(require 'navigation-and-simplify-keys)
(require 'basic-text-editing)

;;=============================================================================
;;           Basic point movements & change buffer's position
;;
;; Physical line navigation:
;; move up down end begin over the real visible screen lines
(require 'physical-line)
(physical-line-mode 1)

(global-set-key (kbd "<up>")   'previous-line)
(global-set-key (kbd "<down>") 'next-line)
(global-set-key (kbd "<end>")  'end-of-line)
(global-set-key (kbd "<home>") 'beginning-of-line)
;;-----------------------------------------------------------------------------
(global-set-key (kbd "C-<right>")   'step-forward-word)
(global-set-key (kbd "C-<left>")    'step-backward-word)
(put 'step-forward-word 'CUA 'move)
(put 'step-backward-word 'CUA 'move)
;;-----------------------------------------------------------------------------
(global-set-key (kbd "C-M-<down>") 'forward-sentence)
(global-set-key (kbd "C-M-<up>") 'backward-sentence)
;;-----------------------------------------------------------------------------
;; cua-mode in org-mode
(eval-after-load "org"
  '(progn
    (define-key org-mode-map (kbd "S-<left>") nil)
    (define-key org-mode-map (kbd "S-<right>") nil)
    (define-key org-mode-map (kbd "C-S-<left>") nil)
    (define-key org-mode-map (kbd "C-S-<right>") nil)
    (define-key org-mode-map (kbd "S-<up>") nil)
    (define-key org-mode-map (kbd "S-<down>") nil)
    (define-key org-mode-map (kbd "M-<up>") nil)
    (define-key org-mode-map (kbd "M-<down>") nil)
    (define-key org-mode-map (kbd "M-<left>") nil)
    (define-key org-mode-map (kbd "M-<right>") nil)
    (define-key org-mode-map (kbd "C-a") nil)
    (define-key org-mode-map (kbd "M-a") nil)))

;;-----------------------------------------------------------------------------
(require 'pager)
;; Bind scrolling functions from pager library.
(global-set-key [next]     'pager-page-down)
(global-set-key [prior]    'pager-page-up)
;;-----------------------------------------------------------------------------
;; Scrolling without point movement
(global-set-key (kbd "C-l") 'recenter-top-bottom)

(if (< emacs-major-version 24)
    (progn
    (global-set-key [(control down)] (lambda () (interactive) (scroll-up 1))) ; [C-down]
    (global-set-key [(control up)] (lambda () (interactive) (scroll-down 1)))) ; [C-up]
  (progn
    (global-set-key [(control down)] 'scroll-up-line) ; [C-down]
    (global-set-key [(control up)] 'scroll-down-line))) ; [C-up]
;;
;;=============================================================================

;;=============================================================================
;;                         Point hyper-jumps
;;
;;-----------------------------------------------------------------------------
; bookmarks
(global-set-key (kbd "C-b") 'bookmark-set)
(global-set-key (kbd "M-b") 'bookmark-jump)
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-x x") 'goto-last-change)

;;-----------------------------------------------------------------------------
;; Search & replace
(global-unset-key (kbd "C-f"))
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward)
;;(global-set-key (kbd "M-e") 'isearch-edit-string) - default

(global-unset-key (kbd "M-r"))
(global-set-key (kbd "M-r") 'replace-string)

(global-set-key (kbd "C-M-f") 'ack) ;; instead of 'rgrep
(global-set-key (kbd "C-c C-f") 'ack-file)

(when (require 'highlight-symbol nil 'noerror)
  (eval-after-load "highlight-symbol"
    '(progn
       (global-set-key [(control f3)] 'highlight-symbol-at-point)
       (global-set-key [f3] 'highlight-symbol-next)
       (global-set-key [(shift f3)] 'highlight-symbol-prev)
       (global-set-key [(meta f3)] 'highlight-symbol-remove-all))))

(defun kostafey-markdown-mode-hook ()
  (define-key markdown-mode-map (kbd "C-M-<up>") 'highlight-symbol-prev)
  (define-key markdown-mode-map (kbd "C-M-<down>") 'highlight-symbol-next))
(add-hook 'markdown-mode-hook 'kostafey-markdown-mode-hook)

;; ace-jump-mode
(global-unset-key (kbd "M-a"))
(when (require 'ace-jump-mode nil 'noerror)
  (define-key global-map (kbd "M-a") 'ace-jump-mode))
;;
;;=============================================================================

;;=============================================================================
;;                           Intellectual point jumps
;;
;;-----------------------------------------------------------------------------
;; html/xml tags navigation
(defun kostafey-html-mode-hook ()
  (define-key html-mode-map (kbd "C-n") 'sgml-skip-tag-forward)
  (define-key html-mode-map (kbd "C-b") 'sgml-skip-tag-backward))
(add-hook 'html-mode-hook 'kostafey-html-mode-hook)
(defun kostafey-nxml-mode-hook ()
  (require 'sgml-mode)
  (define-key nxml-mode-map (kbd "C-n") 'sgml-skip-tag-forward)
  (define-key nxml-mode-map (kbd "C-b") 'sgml-skip-tag-backward))
(add-hook 'nxml-mode-hook 'kostafey-nxml-mode-hook)
;;-----------------------------------------------------------------------------
;; elisp goto definition
(define-key emacs-lisp-mode-map (kbd "C-M-d") 'hop-at-point)
;;
;;=============================================================================

;;=============================================================================
;;                              Command executions
;; smex configuration
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
;;
;;=============================================================================

;;=============================================================================
;;                        Text transformations
;;-----------------------------------------------------------------------------
;; Basic text transformations
(global-set-key (kbd "C-n") 'newline)
(global-set-key (kbd "C-o") 'open-line)
;;-----------------------------------------------------------------------------
;; Word operations
(global-set-key (kbd "M-t") 'transpose-words)
(global-set-key (kbd "M-y") '(lambda() (interactive) (transpose-words -1)))
;;-----------------------------------------------------------------------------
;; Line operations
(global-set-key (kbd "C-j") 'join-next-line-space-n)
(global-set-key (kbd "C-c j") 'join-next-line-n)
(global-set-key (kbd "C-c C-j") 'join-next-line-semicolon-n)

(global-set-key (kbd "C-c c") 'center-line)

(global-set-key (kbd "C-M-k") 'kill-whole-line)
(global-set-key (kbd "C-k") 'kill-line)

(global-set-key (kbd "C-S-c") 'copy-line)
(global-set-key (kbd "C-S-l") 'mark-line)
(global-set-key (kbd "C-c u") 'copy-url)
(global-set-key (kbd "C-c d") 'duplicate-line)
;; Toggle whether to fold or truncate long lines for the current buffer.
(global-set-key (kbd "C-c C-l") 'toggle-truncate-lines)
;;-----------------------------------------------------------------------------
;; Paragraph operations
(global-set-key (kbd "C-c q")  'unfill-paragraph)

;;-----------------------------------------------------------------------------
;; Rectangle operations
(global-set-key (kbd "C-M-a n") 'rectangle-number-lines)
(global-set-key (kbd "C-M-a v") 'string-insert-rectangle)
(global-set-key (kbd "C-M-a c") 'copy-rectangle-to-clipboard)
(global-set-key (kbd "C-M-a r") 'yank-rectangle)
;(global-set-key (kbd "M-u") 'cua-upcase-rectangle) - default

;;-----------------------------------------------------------------------------
;; Upcase/downcase
(global-set-key (kbd "C-S-<up>") 'toggle-letter-case)
;; (global-unset-key "\C-\M-c")
(global-set-key (kbd "C-M-a l") 'downcase-region)
(global-set-key (kbd "C-M-a d") 'downcase-region)
(global-set-key (kbd "C-M-a u") 'upcase-region)
(global-set-key (concat selected-area-prefix "u") 'upcase-region)
(global-set-key (concat selected-area-prefix "l") 'downcase-region)

;;-----------------------------------------------------------------------------
;; Region & misc operations
(global-set-key (kbd "C-M-a :") 'align-by-column)
(global-set-key (kbd "C-M-a '") 'align-by-quote)
(global-set-key (kbd "C-M-a a") 'align-regexp)

(global-set-key (kbd "C-;") 'comment-or-uncomment-this)
(global-set-key (kbd "C-/") 'comment-or-uncomment-this)

(global-set-key (kbd "C-`") 'u:en/ru-recode-region)

(global-set-key (kbd "C-M-R") 'replace-regexp)
(global-set-key (kbd "M-R") 'query-replace)
(global-set-key (kbd "C-M-a k") 'keep-lines)
(global-set-key (kbd "C-M-a f") 'flush-lines)
;;
;;=============================================================================

;;=============================================================================
;; Meta - Навигация
;;=============================================================================
(global-set-key "\M-g" 'goto-line)
;;l - влево j - вправо i - вверх k - вниз
(global-set-key "\M-i" 'previous-line)
(global-set-key "\M-k" 'next-line)
(global-set-key "\M-j" 'backward-char)
(global-set-key "\M-l" 'forward-char)

(global-set-key "\C-cr" 'reposition-window)
(global-unset-key "\M-\C-l")
(global-set-key "\M-\C-j" 'backward-word)
(global-set-key "\M-\C-l" 'forward-word)

(global-set-key "\M-o" 'end-of-line)
(global-set-key "\M-u" 'beginning-of-line)

(global-set-key "\M-m" 'scroll-up)
(global-set-key "\M-," 'scroll-down)

(global-set-key "\M-M" '(lambda () (interactive) (scroll-up 1)))
(global-set-key "\M-<" '(lambda () (interactive) (scroll-down 1)))

;;=============================================================================
;; Look changes
;;
(global-set-key [(meta return)] 'toggle-fullscreen)
;; Folding
(global-set-key [(control meta tab)] 'fold-dwim-toggle-selective-display)
;; Change font size
(global-set-key (kbd "C-+")      '(lambda nil (interactive) (djcb-zoom 1)))
(global-set-key [C-kp-add]       '(lambda nil (interactive) (djcb-zoom 1)))
(global-set-key (kbd "C--")      '(lambda nil (interactive) (djcb-zoom -1)))
(global-set-key [C-kp-subtract]  '(lambda nil (interactive) (djcb-zoom -1)))
;;
;;=============================================================================

;;=============================================================================
;; Gathering information
;;
(global-set-key (kbd "C-?") 'describe-char)
(global-set-key "\C-\M-a\C-c" 'count-words-region)
(global-set-key (kbd "M-p") 'copy-to-clipboard-buffer-file-path)
(global-set-key (kbd "M-f") 'copy-to-clipboard-buffer-file-name)
;;
;;=============================================================================

;;=============================================================================
;; Buffers navigation
;;
(global-set-key (kbd "C-w") 'prh:kill-current-buffer)
(defun kostafey-magit-mode-hook ()
  (define-key magit-mode-map (kbd "C-w") 'prh:kill-current-buffer))
(add-hook 'magit-mode-hook 'kostafey-magit-mode-hook)

(global-set-key (kbd "C-c w") 'kill-other-buffers)

(global-set-key (kbd "C-x w") 'kill-buffer)
(global-set-key (kbd "C-c k") 'delete-this-buffer-and-file)

;; (global-set-key (kbd "C-x <right>") 'next-buffer) - default
;; (global-set-key (kbd "C-x <left>") 'previous-buffer) - default

(global-set-key [(control next)] 'next-buffer)      ; C-Page Up
(global-set-key [(control prior)] 'previous-buffer) ; C-Page Down

;;-----------------------------------------------------------------------------
;; tabbar - switch buffers by tabs
(global-set-key [(shift super left)] 'tabbar-backward-tab)
(global-set-key [(shift super right)] 'tabbar-forward-tab)
;;-----------------------------------------------------------------------------
;; ido - switch buffers by completiotion
(require 'ido)
(ido-mode t)
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-c f") 'ido-choose-from-recentf)
(global-set-key (kbd "C-x f") ; the plain prompt for file path
                '(lambda () (interactive)
                   (find-file (read-from-minibuffer "Enter file path: "))))
(global-set-key (kbd "C-x C-r") 'sudo-edit)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (concat change-buffer-prefix "b") 'switch-to-buffer)
;;-----------------------------------------------------------------------------
;; ibuffer - list of all buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;-----------------------------------------------------------------------------
(require 'bs) ;; other list of buffers
(global-set-key (kbd "C-x C-n") 'bs-show)
;;-----------------------------------------------------------------------------
;; buffers shortcuts
(global-set-key (concat selected-area-prefix "\C-e")
                '(lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (concat change-buffer-prefix "e")
                '(lambda () (interactive) (find-file "~/.emacs.d/init.el")))

(global-set-key "\C-x\C-c" 'switch-to-temp-buffer)
(global-set-key (concat change-buffer-prefix "t") 'switch-to-temp-buffer)

(global-set-key (concat change-buffer-prefix "p")
                '(lambda () (interactive) (find-file "~/.org.gpg")))
(global-set-key (concat change-buffer-prefix "k")
                '(lambda () (interactive) (find-file "~/.keys.org")))
(global-set-key (concat change-buffer-prefix "k")
                '(lambda () (interactive) (find-file "~/.keys.org")))
(global-set-key (kbd "C-x m")
                '(lambda () (interactive) (switch-to-buffer "*Messages*")))

(global-set-key (kbd "C-x t") 'visit-term-buffer)
(global-set-key (kbd "C-c g") 'google)
;;
;;=============================================================================

;;=============================================================================
;; Windows navigation
;;
(global-unset-key "\C-u")
(global-set-key "\C-u" 'swap-windows)

(global-unset-key (kbd "M-m"))
(global-set-key (kbd "M-m") 'mirror-window)

(global-set-key [(control tab)] 'other-window) ; C-tab switchs to a next window
(windmove-default-keybindings 'meta)           ; M-up/down/left/right

(defun kostafey-markdown-mode-hook ()
  (define-key markdown-mode-map (kbd "M-<left>") nil)
  (define-key markdown-mode-map (kbd "M-<right>") nil))
(add-hook 'markdown-mode-hook 'kostafey-markdown-mode-hook)

(global-set-key (kbd "M-<left>") 'meta-left)
(global-set-key (kbd "M-<right>") 'meta-right)

(global-set-key (kbd "s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<down>") 'shrink-window)
(global-set-key (kbd "s-<up>") 'enlarge-window)
;;
;;=============================================================================

;;=============================================================================
;; Modes enabling
;;
;; auto-complete-mode
(define-key ac-mode-map (kbd "C-SPC") 'auto-complete) ;; to 'set-mark-command use M-s instead
(define-key ac-complete-mode-map [next] 'ac-page-next)
(define-key ac-complete-mode-map [prior] 'ac-page-previous)
(define-key ac-complete-mode-map (kbd "C-f") 'ac-isearch)

(global-set-key [f1] 'psw-switch-buffer)
(global-set-key (kbd "C-S-n") 'psw-switch-projectile-files)
(global-set-key (kbd "s-q") 'psw-navigate-files)
(eval-after-load "eassist"
  '(global-set-key [f2] 'psw-switch-function))
;; (global-set-key [f7] 'ispell-buffer); проверить орфографию в текущем буфере
;; (global-set-key [f8] 'ispell-region); 'ispell-word
(global-set-key [f4] 'shell)
(global-set-key [f5] 'bookmark-set)
(global-set-key [f6] 'bookmark-jump)
(global-set-key [f7] 'auto-complete-mode)
(require 'reencoding-file)
(global-set-key [f8] 'recode-buffer-rotate-ring)
(global-set-key [f9] 'auto-fill-mode); вкл/выкл автозаполнения
(global-set-key [f10] 'flyspell-english)
(global-set-key [f11] 'flyspell-russian)
(global-set-key [f12] 'flyspell-mode); вкл/выкл проверки орфографии "на ходу"

;; yasnippet
(defvar yasnippet-prefix "\C-y")
(global-unset-key yasnippet-prefix)
(global-set-key (concat yasnippet-prefix "n") 'yas/new-snippet)
(global-set-key (concat yasnippet-prefix "f") 'yas/find-snippets)
(global-set-key (concat yasnippet-prefix "v") 'yas/visit-snippet-file)
(global-set-key (concat yasnippet-prefix "r") 'yas/reload-all)

(global-set-key (kbd "S-<tab>") 'open-line-or-yas)
(global-set-key (kbd "C-S-<tab>") 'yas-prev-field)
;;
;;=============================================================================

;;============================================================================
;; Paredit customization
;;
(put 'paredit-forward 'CUA 'move)
(eval-after-load "paredit"
  '(progn
    (define-key paredit-mode-map (kbd "C-M-f") nil)
    (define-key paredit-mode-map (kbd "C-<left>") nil)  ; C-}
    (define-key paredit-mode-map (kbd "C-M-<left>") nil)
    (define-key paredit-mode-map (kbd "C-<right>") nil) ; C-)
    (define-key paredit-mode-map (kbd "C-M-<right>") nil)
    (define-key paredit-mode-map (kbd "C-M-<up>") nil)
    (define-key paredit-mode-map (kbd "M-<up>") nil)
    (define-key paredit-mode-map (kbd "M-<down>") nil)
    (define-key paredit-mode-map (kbd "C-j") nil)
    (define-key paredit-mode-map (kbd "C-S-M-n") 'paredit-newline)
    (define-key paredit-mode-map (kbd "C-d") nil)
    (define-key paredit-mode-map (kbd "<delete>") nil)
    (define-key paredit-mode-map (kbd "<DEL>") nil)
    (define-key paredit-mode-map (kbd "<deletechar>") nil)
    (define-key paredit-mode-map (kbd "<backspace>") nil)
    (define-key paredit-mode-map (kbd "M-r") nil)
    (define-key paredit-mode-map (kbd "M-C-'") 'paredit-raise-sexp)
    (define-key paredit-mode-map (kbd ")") 'nil)
    (define-key paredit-mode-map (kbd "]") 'nil)
    (define-key paredit-mode-map (kbd "\\") 'nil)
    (define-key paredit-mode-map (kbd "\"") 'nil)
    ;;(define-key paredit-mode-map (kbd "M-q") 'nil)
    (define-key paredit-mode-map (kbd "C-M-d") 'nil)
    ))

(global-set-key [(meta super right)] 'transpose-sexps)
(global-set-key [(meta super left)] (lambda () (interactive) (transpose-sexps -1)))
;;
;;============================================================================

;; speedbar
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;;=============================================================================
;; ecb
(global-set-key (kbd "\e M-l") 'ecb-toggle-ecb-windows)
(global-set-key (kbd "M-w") 'ecb-toggle-ecb-windows)
(global-set-key (kbd "C-x C-a") 'ecb-activate)
(global-set-key (kbd "C-x C-q") 'ecb-deactivate)
;; (global-set-key "\M-m" 'ecb-goto-window-methods)
;;
;;=============================================================================

;;=============================================================================
;; Mode keys & programming language specific keys.
;;
;;----------------------------------------------------------------------
;; Java
(defun kostafey-java-mode-hook ()
  (define-key java-mode-map (kbd "C-a") nil)
  (define-key java-mode-map (kbd "C-h j") 'javadoc-lookup)
  (define-key java-mode-map (kbd "C-<f1>") 'javadoc-lookup)
  (define-key java-mode-map (kbd "C-M-d") 'hop-at-point))
(add-hook 'java-mode-hook 'kostafey-java-mode-hook)

(global-set-key (kbd "C-<f10>") 'tomcat-toggle)
(global-set-key (kbd "C-<f9>") 'maven-tomcat-deploy)

;;----------------------------------------------------------------------
;; lisp
(global-set-key (kbd "M-e") 'eval-print-last-sexp)

;;----------------------------------------------------------------------
;; CIDER - Nrepl.el
;;
(global-unset-key (kbd "C-n"))
(defun kostafey-clojure-mode-hook ()
  (define-key clojure-mode-map (kbd "C-n j") 'cider-jack-in)
  (define-key clojure-mode-map (kbd "C-n e b") 'cider-eval-buffer)
  (define-key clojure-mode-map (kbd "C-n q") 'cider-quit)
  (define-key clojure-mode-map (kbd "C-h j") 'javadoc-lookup)
  (define-key clojure-mode-map (kbd "C-M-d") 'hop-at-point)
  (define-key clojure-mode-map (kbd "C-c C-l") nil)
  (define-key clojure-mode-map (kbd "C-M-<up>") 'highlight-symbol-prev)
  (define-key clojure-mode-map (kbd "C-M-<down>") 'highlight-symbol-next))
(add-hook 'clojure-mode-hook 'kostafey-clojure-mode-hook)
(global-set-key (kbd "C-<f5>") 'initialize-cljs-repl)
;;----------------------------------------------------------------------
;; SQL
;;
(when (require 'ejc-sql nil 'noerror)
  (eval-after-load "ejc-sql"
    '(progn
       (global-set-key (kbd "C-x C-s") 'ejc-switch-to-sql-editor-buffer))))
;;
;;----------------------------------------------------------------------
;; Magit & ahg
;;
(global-unset-key (kbd "M-w"))
(eval-after-load "version-control"
  '(progn
     (global-set-key (kbd "M-w") 'get-vc-status)))

(eval-after-load "ahg"
  '(progn
     (define-key ahg-status-mode-map [tab] 'ahg-status-diff)))
;;
;;----------------------------------------------------------------------
;; multiple-cursors
;;
(when (require 'multiple-cursors nil 'noerror)
  ;; When you have an active region that spans multiple lines, the following will
  ;; add a cursor to each line:
  (global-set-key (kbd "C-S-m") 'mc/edit-lines)
  ;; When you want to add multiple cursors not based on continuous lines, but
  ;; based on keywords in the buffer, use:
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))
;;
;;=============================================================================

;;=============================================================================
;;                               Mouse
;;
;; Select by mouse and shift
;;-----------------------------------------------------------------------------
;; shift + click select region
(define-key global-map (kbd "<S-down-mouse-1>") 'ignore) ; turn off font dialog
(define-key global-map (kbd "<S-mouse-1>") '(lambda (e)
                                              (interactive "e")
                                              (if (not mark-active)
                                                  (cua-set-mark))
                                              (mouse-set-point e)))
;;-----------------------------------------------------------------------------
;; ctrl + shift + click select rectange region
(global-unset-key (kbd "<C-S-down-mouse-1>"))
(global-set-key (kbd "<C-S-mouse-1>") 'hkb-mouse-mark-cua-rectangle)
(define-key cua--rectangle-keymap (kbd "<C-S-mouse-1>") 'hkb-mouse-mark-cua-rectangle)
(global-set-key (kbd "<C-down-mouse-1>") 'hop-by-mouse)
;;
;;=============================================================================

(provide 'key-bindings)

;; see https://github.com/skeeto/elfeed for details.
(global-set-key (kbd "M-<f2>") 'elfeed)
