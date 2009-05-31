;; russian-utf8-env.el -- Create russian utf-8 environment
;;
;; Copyright (C) 2007 - Kirill A. Korinskiy - catap@catap.ru
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301 USA

(require 'mule)

(set-language-info "Russian UTF-8" 'tutorial "TUTORIAL.ru")
(set-language-info "Russian UTF-8" 'documentation "Support for Russian using utf-8 and the russian-computer input method.")
(set-language-info "Russian UTF-8" 'sample-text "Russian (Русский)	Здравствуйте!")
(set-language-info "Russian UTF-8" 'coding-priority '(mule-utf-8 cyrillic-koi8 cyrillic-iso-8bit))
(set-language-info "Russian UTF-8" 'coding-system '(mule-utf-8))
(set-language-info "Russian UTF-8" 'unibyte-display 'mule-utf-8)
;; в версии 23 более не актуально
;;(set-language-info "Russian UTF-8" 'charset '(mule-utf-8))
(set-language-info "Russian UTF-8" 'input-method "russian-computer")
(set-language-info "Russian UTF-8" 'setup-function (lambda nil
						     (if (and (fboundp 'w32-add-charset-info)
							      (not
							       (boundp 'w32-unicode-charset-defined)))
							 (w32-add-charset-info "iso10646-1" 'w32-charset-ansi t))))
(provide 'russian-utf8-env)

;;; Local Variables:
;;; mode: emacs-lisp
;;; coding: utf-8-unix
;;; End:
