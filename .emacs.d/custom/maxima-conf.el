;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Настройка Maxima
;; 
(load "c:/Program Files/Maxima-5.18.1/share/maxima/5.18.1/emacs/setup-imaxima-imath.el")
(setq imaxima-tmp-dir "C:\\Windows\\tmp")
(setq imaxima-gs-bin-dir "C:\\gs\\gs8.70\\bin")
;; Подключаем Maxima
;; указываем где будут лежать файлы расширений
(add-to-list 'load-path "c:/Program Files/Maxima-5.18.1/share/maxima/5.18.1/emacs/")
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