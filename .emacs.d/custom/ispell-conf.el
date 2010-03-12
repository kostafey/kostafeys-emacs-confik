;;=============================================================================
;;
;;Настройка проверки правописания Ispell
;;
(require 'flyspell)
(require 'ispell)

(setq
 ; i like aspel, and you?
 ispell-program-name "aspell"

 ; my dictionary-alist, using for redefinition russian dictionary
 ispell-dictionary-alist 
 '(("english"                       ; English
    "[a-zA-Z]"
    "[^a-zA-Z]"
    "[']"
    nil
    ("-d" "en")
    nil iso-8859-1)
   ("russian"                       ; Russian
    "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
    "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
    "[-]"
    nil
    ("-C" "-d" "ru")
    nil utf-8)
   (nil                             ; Default
    "[A-Za-z]"
    "[^A-Za-z]"
    "[']"
    nil
    ("-C")
    nil iso-8859-1))

 ispell-russian-dictionary "russian"
 ispell-english-dictionary "english"
 flyspell-default-dictionary ispell-russian-dictionary
 flyspell-dictionary ispell-russian-dictionary
 ispell-dictionary ispell-english-dictionary
 ispell-local-dictionary ispell-russian-dictionary
 ispell-extra-args '("--sug-mode=ultra"))

(defun flyspell-russian ()
  (interactive)
  (flyspell-mode t)
  (ispell-change-dictionary ispell-russian-dictionary)
  (flyspell-buffer)
  (message "Russian dictionary - Spell Checking completed."))

; English
(defun flyspell-english ()
  (interactive)
  (flyspell-mode t)
  (ispell-change-dictionary ispell-english-dictionary)
  (flyspell-buffer)
  (message "English dictionary - Spell Checking completed."))

(setq ispell-highlight-face (quote flyspell-incorrect))
(setq ispell-have-new-look t)
(setq ispell-enable-tex-parser t)
;(add-hook 'text-mode-hook 'flyspell-russian)
(setq flyspell-delay 1)
(setq flyspell-always-use-popup t)

(global-set-key [f1] 'ispell-word)
;; (global-set-key [f7] 'ispell-buffer); проверить орфографию в текущем буфере
(global-set-key [f8] 'ispell-region)
(global-set-key [f9] 'auto-fill-mode); вкл/выкл автозаполнения
(global-set-key [f10] 'flyspell-english)
(global-set-key [f11] 'flyspell-russian)
(global-set-key [f12] 'flyspell-mode); вкл/выкл проверки орфографии "на ходу"
;;
;;=============================================================================

(provide 'ispell-conf)