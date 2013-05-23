;;; organic-green-theme.el --- Non-contrast green color theme.

;;; Copyright © 2009-2013 - Kostafey <kostafey@gmail.com>

;; This file is not [yet] part of GNU Emacs, but is distributed under
;; the same terms.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(make-face 'mode-line-default-face)
(make-face 'mode-line-header)

(deftheme organic-green
  "Non-contrast green color theme.
Basic, Font Lock, Isearch, Jabber, rst faces are included.")

(let* ((class '((class color) (min-colors 89)))
       ;; Organic-green palette colors.

       ;; green palette
       ;; green1 ;green2 ;green3 ;green4
       ;; http://en.wikipedia.org/wiki/Sea_Eye_Lake
       (green-tea      "#D0F0C0") (lime         "#BFFF00")
       (gray-green     "#ACE1AF") (yellow-green "#ADFF2F")
       (мох            "#ADDFAD") (warm-green   "#95E454")
       (light-green    "#A0F0A0") (sun-green    "#7FFF00")
       (emerald        "#50C878") (cham-1       "#8ae234")                                    
       (sea-eye        "#00A86B") (cham-2       "#73d216")       
       (sea-green      "#2E8B57") (cham-3       "#4e9a06")
       (geep-sea-green "#339966") (cham-4       "#346604")
       (medium-green   "#66CC66") (oak-green    "#53AD2F")
       (dark-green     "#339933") (grass-green  "#32CD32")
       (green-a        "#73CD4F")                                   

       ;; blue palette
       (blue-green        "#66CDAA")
       ;; ;#73CDF4        "#3030DF" 
       ;; ;#83DDFF        "#4045F0"   
       (blue-0            "#8cc4ff")
       (blue-1            "#729fcf") 
       (blue-2            "#3465a4") 
       (blue-3            "#204a87")

       ;; yellow palette
       (lime-pulp         "#D1E189")
       (yellow-chartreuse "#DFFF00")
       (butter-1          "#fce94f") 
       (butter-2          "#edd400") 
       (butter-3          "#c4a000")
       (dark-yellow       "#808000")
   
       ;; misc palette
       (orange-1 "#fcaf3e") (plum-1   "#ad7fa8")
       (orange-2 "#f57900") (plum-2   "#75507b")
       (orange-3 "#ce5c00") (plum-3   "#5c3566")
       (choc-1   "#e9b96e") (red-1    "#ef2929")
       (choc-2   "#c17d11") (red-2    "#cc0000")
       (choc-3   "#8f5902") (red-3    "#a40000")
       (alum-1   "#eeeeec")
       (alum-2   "#d3d7cf") 
       (alum-3   "#babdb6")
       (alum-4   "#888a85") 
       (alum-5   "#5f615c") 
       (alum-6   "#2e3436")
       
       ;; basic colors
       (organic-fg "#326B6B")
       (organic-bg "#F0FFF0")
       (organic-cursor-fg "#225522")
       (organic-comment-fg "gray50")
       (organic-string-fg "#119911")
       (organic-constant-fg "#3465BD")
       (organic-builtin-fg "#009292") ; "#009494" "#008C8C" "MediumPurple3"
       (minor-green-highlight-background "#D5F0D5")
       (tiny-green-highlight-background "#E3F2E1")      
       (minor-grey-highlight-background "#DAEADA")
       (minor-yellow-highlight-background "#F2FFC0") ;#E3F2A1
       (minor-blue-highlight-background "#C0E0FF")
       (minor-red-highlight-background "#FFF0F0"))

  (custom-theme-set-faces
   'organic-green
   `(default ((,class (:foreground ,organic-fg :background ,organic-bg))))
   `(cursor ((,class (:background ,organic-cursor-fg))))
   `(hl-line ((,class (:background "#A0F0A0" :inverse-video nil))))

   `(mode-line-default-face ((,class (:foreground ,organic-fg))))
   `(mode-line-header ((t (:foreground "gray25" :weight bold))))

   ;; Highlighting faces
   `(fringe ((,class (:background "#E5E5E5" :foreground "gray40"))))
   `(highlight ((,class (:background ,minor-green-highlight-background))))
   `(region ((,class (:foreground ,organic-fg :background ,"#EEEEA0"))))
   `(cua-rectangle ((,class (:foreground ,organic-fg :background ,"#BFFF00"))))   
   `(secondary-selection ((,class (:background ,blue-0))))
   `(isearch ((,class (:foreground ,organic-fg :background "yellow" :inverse-video nil))))
   `(lazy-highlight ((,class (:background "#DDEE00" :inverse-video nil))))
   `(trailing-whitespace ((,class (:background ,red-1))))

   ;; Mode line faces
   `(mode-line ((,class (:box (:line-width -1 :style released-button)
			 :background ,alum-2 :foreground ,alum-6))))
   `(mode-line-inactive ((,class (:box (:line-width -1 :style released-button)
				  :background ,alum-1 :foreground ,alum-6))))

   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:weight bold :foreground ,blue-3))))
   `(escape-glyph ((,class (:foreground ,red-3))))
   `(error ((,class (:foreground ,red-3))))
   `(warning ((,class (:foreground ,orange-3))))
   `(success ((,class (:foreground ,cham-3))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,organic-builtin-fg))))
   `(font-lock-comment-face ((,class (:foreground ,organic-comment-fg))))
   `(font-lock-constant-face ((,class (:foreground ,organic-constant-fg))))
   `(font-lock-function-name-face ((,class (:weight extra-bold :foreground "blue"))))
   `(font-lock-keyword-face ((,class (:weight semi-bold :foreground "purple"))))
   `(font-lock-string-face ((,class (:foreground ,organic-string-fg))))
   `(font-lock-type-face ((,class (:slant italic :foreground "ForestGreen"))))
   `(font-lock-variable-name-face ((,class (:width condensed :foreground "DarkGoldenrod"))))
   `(font-lock-warning-face ((,class (:foreground "#AA0000"))))

   ;; Button and link faces
   `(link ((,class (:underline t :foreground ,blue-3))))
   `(link-visited ((,class (:underline t :foreground ,blue-2))))

   ;; Jabber
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

   ;; LaTeX
   '(font-latex-bold-face ((t (:bold t :foreground "DarkOliveGreen"))))
   '(font-latex-italic-face ((t (:italic t :foreground "DarkOliveGreen"))))
   '(font-latex-math-face ((t (:foreground "DarkGoldenrod"))))
   '(font-latex-sedate-face ((t (:foreground "DimGray"))))
   '(font-latex-string-face ((t (nil))))
   '(font-latex-warning-face ((t (:bold t :weight semi-bold :foreground "#00CC00"))))

   ;; quack
   '(quack-pltish-paren-face ((((class color) (background light)) (:foreground "#53AD2F"))))
   '(quack-pltish-keyword-face ((t (:foreground "#A020F0" :weight bold))))

   ;; erc
   '(erc-action-face ((t (:foreground "gray" :weight bold))))
   '(erc-command-indicator-face ((t (:foreground "black" :weight bold))))
   '(erc-nick-default-face ((t (:foreground "SlateBlue" :weight bold))))
   '(erc-input-face ((t (:foreground "#000099"))))
   '(erc-notice-face ((t (:foreground "dark sea green" :weight bold))))
   '(erc-timestamp-face ((t (:foreground "#32CD32" :weight bold))))

   ;; rst
   '(rst-definition ((t (:inherit font-lock-constant-face))) t)
   `(rst-level-1 ((t (:background ,minor-green-highlight-background))) t)   
   `(rst-level-2 ((t (:background ,minor-grey-highlight-background))))
   `(rst-level-3 ((t (:background ,minor-grey-highlight-background))))
   `(rst-level-4 ((t (:background ,minor-grey-highlight-background))))
   `(rst-level-5 ((t (:background ,minor-grey-highlight-background))))
   `(rst-level-6 ((t (:background ,minor-grey-highlight-background))))
   '(rst-block ((t (:inherit font-lock-function-name-face :bold t))) t)
   '(rst-external ((t (:inherit font-lock-constant-face))) t)
   '(rst-directive ((t (:inheit font-lock-builtin-face))) t)
   '(rst-literal ((t (:inheit font-lock-string-face))))
   '(rst-emphasis1 ((t (:inherit italic))) t)
   `(rst-adornment ((t (:bold t :foreground ,blue-2))))

   ;; whitespace-mode
   `(whitespace-empty ((t (:background ,organic-bg :foreground "lightgray"))) t)
   `(whitespace-indentation ((t (:background ,organic-bg :foreground "lightgray"))) t)
   `(whitespace-newline ((t (:background ,organic-bg :foreground "lightgray"))) t)
   `(whitespace-space-after-tab ((t (:background ,organic-bg :foreground "lightgray"))) t)
   `(whitespace-tab ((t (:background ,organic-bg :foreground "lightgray"))) t)
   `(whitespace-hspace ((t (:background ,organic-bg :foreground "lightgray"))) t)
   `(whitespace-line ((t (:background ,organic-bg :foreground "lightgray"))) t)
   `(whitespace-space ((t (:background ,organic-bg :foreground "lightgray"))) t)
   `(whitespace-space-before-tab ((t (:background ,organic-bg :foreground "lightgray"))) t)
   `(whitespace-trailing ((t (:background ,organic-bg :foreground ,plum-1))) t)

   ;; magit
   '(magit-diff-add ((t (:foreground "#339933"))) t)
   `(magit-diff-del ((t (:foreground ,red-2))) t)
   '(magit-item-highlight ((t (:background "#E3F2E1"))) t)

   ;; misc
   '(nxml-element-local-name ((t (:foreground "#0066CC" :weight normal))) t)
   '(speedbar-tag-face ((t (:foreground "DarkSlateGray4"))))
   '(yas/field-highlight-face ((t (:background "#DDEE00"))))
   `(idle-highlight ((t (:foreground ,organic-fg :background ,minor-yellow-highlight-background))) t)
   `(comint-highlight-prompt ((t (:foreground ,organic-constant-fg :weight bold))) t)
   ))

(provide-theme 'organic-green)

;; (load-theme 'organic-green t)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; organic-green-theme.el ends here
