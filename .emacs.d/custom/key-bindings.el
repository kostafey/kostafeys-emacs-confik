
;;=============================================================================
;; CUA - гумоноидизация emacs ;)
;;=============================================================================
(cua-mode t)
(setq transient-mark-mode t)
(setq x-select-enable-clipboard t)

;;=============================================================================
;; Перфикс для ключей, применяемых к выделенным областям
;;=============================================================================
(global-unset-key "\C-\M-a")
(defvar selected-area-prefix "\C-\M-a")

(defvar change-buffer-prefix "\C-c\C-b")

(require 'navigation-and-simplify-keys)
(require 'buffer-navigation)

(global-set-key (kbd "C-<right>") 'step-forward-word)
(global-set-key (kbd "C-<left>") 'step-backward-word)
(global-set-key (kbd "C-S-<right>") 'step-forward-select)
(global-set-key (kbd "C-S-<left>") 'step-backward-select)

(global-set-key (kbd "C-+")      '(lambda nil (interactive) (djcb-zoom 1)))
(global-set-key [C-kp-add]       '(lambda nil (interactive) (djcb-zoom 1)))
(global-set-key (kbd "C--")      '(lambda nil (interactive) (djcb-zoom -1)))
(global-set-key [C-kp-subtract]  '(lambda nil (interactive) (djcb-zoom -1)))

(global-set-key [(meta return)] 'toggle-fullscreen)

;; Toggle whether to fold or truncate long lines for the current buffer.
(global-set-key (kbd "C-c C-l") 'toggle-truncate-lines)

;;Folding
(global-set-key [(control meta tab)] 'fold-dwim-toggle-selective-display)
(global-set-key "\C-cf" 'semantic-tag-folding-fold-block)
(global-set-key "\C-cs" 'semantic-tag-folding-show-block)

(require 'basic-text-editing)

(global-set-key (kbd "C-?") 'describe-char)

(global-unset-key "\C-\M-a")
(global-set-key "\C-\M-a\C-c" 'count-words-region)

(global-set-key "\C-j" 'join-next-line-space-n)
(global-set-key "\C-cj" 'join-next-line-n)
(global-set-key "\C-c\C-j" 'join-next-line-semicolon-n)

(global-set-key "\C-cd" 'duplicate-line)

(global-set-key (kbd "C-;") 'comment-or-uncomment-this)

(global-set-key (kbd "C-c q")  'unfill-paragraph)

(global-set-key (kbd "C-M-a :") 'align-by-column)

(global-set-key (kbd "C-`") 'u:en/ru-recode-region)

(global-set-key (kbd "C-c w") 'downcase-word)
;; (global-unset-key "\C-\M-c")
(global-set-key (kbd "C-M-a l") 'downcase-region)
(global-set-key (kbd "C-M-a d") 'downcase-region)
(global-set-key (kbd "C-M-a u") 'upcase-region)

(global-set-key (kbd "C-M-a n") 'rectangle-number-lines)
(global-set-key (kbd "C-M-a v") 'string-insert-rectangle)
(global-set-key (kbd "C-M-a c") 'copy-rectangle-to-clipboard)

;;=============================================================================
;; Навигация по буферам
;;=============================================================================
;; (global-set-key (kbd "C-x <right>") 'next-buffer)
;; (global-set-key (kbd "C-x <left>") 'previous-buffer)

;; (global-set-key (kbd "M-u") 'cua-upcase-rectangle)

(global-set-key (kbd "C-M-R") 'replace-regexp)
(global-set-key (kbd "M-R") 'query-replace)
(global-set-key (kbd "C-M-a k") 'keep-lines)


(require 'bubble-buffer)
(global-set-key (kbd "C-x <up>") 'bubble-buffer-next)
(global-set-key (kbd "C-x <down>") 'bubble-buffer-previous)

(provide 'key-bindings)
