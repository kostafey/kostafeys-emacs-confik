;;=============================================================================
;; el-get configuration
;;

(add-to-list 'load-path (concat site-lisp-path "el-get/el-get/"))

(unless (require 'el-get nil t)
  ;; get stable branch
  (url-retrieve 
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el" 
   (lambda (s) 
     (goto-char (point-max)) 
     (eval-print-last-sexp))))

;; get master branch
;; (url-retrieve 
;;  "https://raw.github.com/dimitri/el-get/master/el-get-install.el" 
;;  (lambda (s) 
;;    (let (el-get-master-branch) 
;;      (goto-char (point-max)) 
;;      (eval-print-last-sexp))))

(setq el-get-byte-compile nil)

(setq el-get-sources
      ;; look and feel
      '((:name highlight-symbol
               :after (lambda () 
                        (global-set-key [(control f3)] 'highlight-symbol-at-point)
                        (global-set-key [f3] 'highlight-symbol-next)
                        (global-set-key [(shift f3)] 'highlight-symbol-prev)
                        (global-set-key [(meta f3)] 'highlight-symbol-remove-all)))
        ;;(:name pager)
        ;;-----------------------------------------------------------------------------       
        ;; minibuffer
        ; режим автозавршения команды в минибуфере
        (:name icomplete+
               :description "Extensions to `icomplete.el'."
               :type emacswiki
               :features "icomplete+"
               :after (lambda () (icomplete-mode)))
        ;=============================================================================
        ;; Wrap text with punctation or tag
        ;; (require 'wrap-region)
        ;; (wrap-region-mode t)
        ;; (:name wrap-region
        ;;        :description "Wrap text with punctation or tag"
        ;;        :post-init (lambda ()
        ;;                     (autoload 'wrap-region-mode "wrap-region" nil t))
        ;;        :after (lambda ()
        ;;                 (progn
        ;;                   (wrap-region-global-mode t)
        ;;                   (wrap-region-add-wrapper "*" "*"))))
        ;;-----------------------------------------------------------------------------
        (:name rainbow-mode
               :description "Displays color names with colored background."
               :type emacswiki
               :url "http://bzr.savannah.gnu.org/lh/emacs/elpa/download/head:/rainbowmode0.1.el-20101118182351-xme90jru269t6msr-12/rainbow-mode.el"
               :features rainbow-mode)

        ))


(setq my-packages
      (append
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)
;; (el-get 'sync)

;; local sources
;; (setq el-get-sources
;;       '((:name ropemacs)
;;         (:name pymacs)))

(provide 'el-get-conf)

