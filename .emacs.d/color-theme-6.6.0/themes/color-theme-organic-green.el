;;; color-theme-organic-green.el ---

;; License is WTFPL http://sam.zoy.org/wtfpl/
;; started from 08.11.2009
;; Author: Kostefey

(require 'color-theme)

;; (list-colors-display)
;; (rainbow-mode)
;; (css-palette-mode)

; фон 
;#F0FFF0
;#E3F2E1
;#E3F2A1

;цвета

;#7FFF00
;#95E454
;#66CDAA
;#CE7B00
;#32CD32
;#73CD4F
;#66CC66
;#73CDF4
;#6495ED
;#6666CC
;#8000FF
;#A020F0
;#0066CC
;#000099
;#FFAEAE
;#DDEE00
;#EEDD00
;#B1B100
;#717100
;#339966
;#44AAFF
;#F9F9F9
;#F0FFF0

(defvar organic-fg "#326B6B")
(defvar organic-bg "#F0FFF0")
(defvar organic-cursor-fg "#225522")
(defvar organic-comment-fg "gray50")
(defvar organic-string-fg "#119911")

(defvar desert-doc-fg "IndianRed3")
(defvar desert-kw-fg "#225522")
(defvar desert-function-fg "PaleGreen3")
;(defvar desert-variable-fg desert-fg)
(defvar desert-type-fg "PaleGreen3") ; unfortunately, vim doesn't highlight class names, when they are defined
(defvar organic-constant-fg "#000000")
(defvar desert-warning-fg "goldenrod")
(defvar organic-builtin-fg "MediumPurple3")
(defvar desert-paren-fg "PaleGreen3")
(defvar desert-hl-fg "black")
(defvar desert-hl-bg "olivedrab")
(defvar desert-minibuffer-fg "#225522")
(defvar desert-linum-fg "yellow")
(defvar desert-mhl-bg "gray90") ; mode-line, header-line
(defvar desert-fixme-fg "orangered")
(defvar desert-fixme-bg "yellow2")

;(cursor-color . "#225522")

;;;###autoload


(defun color-theme-organic-green ()
  "Emacs color theme... it's crazy, but I like it :)"
  (interactive)
  (color-theme-install
   (append
    (list 'color-theme-organic-green
          `((background-color . ,organic-bg)
            (foreground-color . ,organic-fg)
            (cursor-color . ,organic-cursor-fg)
            (mouse-color . ,organic-cursor-fg)
            (background-mode . light))

		  `(font-lock-builtin-face ((t (:foreground ,organic-builtin-fg))))
          `(font-lock-comment-face ((t (:foreground ,organic-comment-fg))))
          `(font-lock-string-face ((t (:foreground ,organic-string-fg))))

          `(font-lock-warning-face ((t (:bold t :weight semi-bold :foreground "#AA0000"))))

          '(fringe ((t (:background "#E5E5E5"))))
          '(fringe ((t (:foreground "#DDEE00"))))

		  `(font-lock-constant-face ((t (:foreground ,"#3465BD"))))
          
		  '(font-lock-function-name-face ((t (:bold t :foreground "Blue" :weight extra-bold ))))
		  '(font-lock-keyword-face ((t (:bold t :foreground "Purple" :weight semi-bold))))
		  '(font-lock-type-face ((t (:italic t :foreground "ForestGreen" :slant italic))))
		  '(font-lock-variable-name-face ((t (:foreground "DarkGoldenrod" :width condensed))))

          '(jabber-roster-user-chatty ((t (:inherit font-lock-type-face :bold tx))))
          '(jabber-roster-user-online ((t (:inherit font-lock-keyword-face :bold t))))
          `(jabber-roster-user-offline ((t (:foreground ,organic-fg :background ,organic-bg))))
          '(jabber-roster-user-away ((t (:inherit font-lock-doc-face))))
          '(jabber-roster-user-xa ((t (:inherit font-lock-doc-face))))
          '(jabber-roster-user-dnd ((t (:inherit font-lock-comment-face))))
          '(jabber-roster-user-error ((t (:inherit font-lock-warning-face))))

          '(jabber-title-small ((t (:height 1.2 :weight bold))))
          '(jabber-title-medium ((t (:inherit jabber-title-small :height 1.2))))
          '(jabber-title-large ((t (:inherit jabber-title-medium :height 1.2))))

          '(jabber-chat-prompt-local ((t (:inherit font-lock-string-face :bold t))))
          '(jabber-chat-prompt-foreign ((t (:inherit font-lock-function-name-face :bold nil))))
          '(jabber-chat-prompt-system ((t (:inherit font-lock-comment-face :bold t))))
          '(jabber-rare-time-face ((t (:inherit font-lock-function-name-face :bold nil))))

          '(jabber-activity-face ((t (:inherit jabber-chat-prompt-foreign))))
          '(jabber-activity-personal-face ((t (:inherit jabber-chat-prompt-local :bold t))))

		  '(speedbar-tag-face ((t (:foreground "DarkSlateGray4"))))

		  '(font-latex-bold-face ((t (:bold t :foreground "DarkOliveGreen"))))
		  '(font-latex-italic-face ((t (:italic t :foreground "DarkOliveGreen"))))
		  '(font-latex-math-face ((t (:foreground "DarkGoldenrod"))))
		  '(font-latex-sedate-face ((t (:foreground "DimGray"))))
		  '(font-latex-string-face ((t (nil))))
		  '(font-latex-warning-face ((t (:bold t :weight semi-bold :foreground "#00CC00"))))

          '(quack-pltish-paren-face ((((class color) (background light)) (:foreground "#53AD2F"))))
          '(quack-pltish-keyword-face ((t (:foreground "#A020F0" :weight bold))))          

		  ))))

(provide 'color-theme-organic-green)

(color-theme-organic-green)

;;; color-theme-desert.el ends here