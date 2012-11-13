;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;Настройки AucTeX
;;
;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)

;\mathtext.sty
;tex\latex\cyrillic\t2aenc.def
;fonts\source\lh\lh-t2a\lacodes.mf
;fonts\source\lh\lh-t2a\lacodes.mf
;;;;;;;;;;;;;; pdf2dsc
(require 'tex-mik)
(add-hook 'LaTeX-mode-hook 'LaTeX-install-toolbar)
(setq TeX-parse-self t)             ; Enable parse on load.
(setq TeX-auto-save t)              ; Enable parse on save.
(setq-default TeX-master nil)       ; Query for master file.
(setq TeX-PDF-mode t)
(setq TeX-interactive-mode t)
(setq TeX-source-specials-mode 1)

(if (eq system-type 'gnu/linux)
    ;; meta-shift-z
    (progn
      (setq TeX-view-program-list '(("Okular" "okular --unique %o#src:%n%b")))
      (setq TeX-view-program-selection '((output-pdf "Okular")))))

;;модифицируем меню
;;; some more menu entries in the command list:
;;; see tex-mik.el from package auctex: %v is defined in tex-mik.el
;;; other variables are defined in tex.el from auctex
;;; the meaning of some auctex-varibles:
        ;symbols defined in tex.el and tex-mik.el:
        ;%b name slave tex-file  %t name master tex-file   
        ;%d dvi-file  %f ps-file 
        ;%l "latex --src-specials"
        ;%n line number  %p printcommand  %q "lpq"  
        ;%r (TeX-style-check TeX-print-style)
        ;%s master-file-name without extention
        ;%v yap command view line
(eval-after-load "tex"
  '(progn
     (add-to-list 'TeX-command-list
		  (list "->PS landscape for pdf"
			"dvips %d -N0 -Ppdf -G0 -T 297mm,210mm -o %f " 
			'TeX-run-command nil t))
     (add-to-list 'TeX-command-list
		  (list "All Texify run-viewer"
			"texify --tex-opt=--src --run-viewer --clean %s.tex"
			'TeX-run-command nil t))))
;;
;;Настройки PreviewLatex
;; (load "preview-latex.el" nil t t) 

;; (setenv "PATH" (concat "/usr/local/texlive/2011/bin/x86_64-linux/:" (getenv "PATH")))
;; (add-to-list 'exec-path "/usr/local/texlive/2011/bin/x86_64-linux/")
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'auctex-conf)

