;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Настройка Maxima
;;
;;(setq maxima-version "5.22.1")
(setq maxima-version "5.18.1")
(setq maxima-path 
      (format "c:/Program Files/Maxima-%s/share/maxima/%s/emacs/"
              maxima-version maxima-version))
(add-to-list 'load-path maxima-path)
;;
;; Подключаем Imaxima
;;
;; (load (concat maxima-path "setup-imaxima-imath.el"))
(setq imaxima-tmp-dir "C:\\Windows\\tmp")
(setq imaxima-gs-bin-dir "C:\\gs\\gs8.70\\bin")
;;
;; Подключаем Maxima
;; указываем где будут лежать файлы расширений
;;
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'maxima-mode "maxima" "Maxima mode" t)
; C-c C-C - выполнить строку
; C-c C-r - выполнить выделенный блок
; C-c C-b - выполнить буфер (т.е. файл)
(setq auto-mode-alist (cons '("\\.mxm" .  maxima-mode) auto-mode-alist))
;; M-X emaxima-mode
(autoload 'emaxima-mode "emaxima" "EMaxima" t)
(add-hook 'emaxima-mode-hook 'emaxima-mark-file-as-emaxima)
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'maxima-conf)