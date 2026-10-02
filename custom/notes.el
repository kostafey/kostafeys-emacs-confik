;;; notes.el --- Switch among the notes kept in one directory. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; `k/notes' selects a note in `k/notes-dir' with `consult', whether it is
;; visited in a buffer or not.  Every candidate shows the file name followed
;; by the title of the note -- the `#+title:' of an org note, the first
;; level 1 header of a markdown one -- and the input narrows the notes
;; by both.
;; Typing a name that matches no note creates it.  `k/notes-search' greps
;; the notes with `consult-ripgrep'.

;;; Code:

(require 'seq)

;; `consult' is loaded lazily, see `k/notes'.
(declare-function consult--multi "consult")
(declare-function consult--file-state "consult")
(declare-function consult-ripgrep "consult")

(defvar k/notes-dir "C:/workspace/notes"
  "Directory holding the notes `k/notes' switches among.")

(defvar k/notes-history nil
  "Completion history of `k/notes'.")

(defvar k/notes-title-regexps
  '(("org"      . "^#\\+title:[ \t]*\\(.*?\\)[ \t]*$")
    ("md"       . "^#[ \t]+\\(.*?\\)\\(?:[ \t]+#+\\)?[ \t]*$")
    ("markdown" . "^#[ \t]+\\(.*?\\)\\(?:[ \t]+#+\\)?[ \t]*$"))
  "Alist of a note file extension and the regexp of the note title.
Group 1 of the regexp is the title: the `#+title:' of an org note, the
first level 1 header of a markdown one.")

(defun k/notes--title (file)
  "Return the title of the note FILE, or nil if it has none.
The regexp of the title is the one `k/notes-title-regexps' has for the
extension of FILE.  When FILE is visited, its buffer is searched, so
unsaved edits count; otherwise only the head of the file is."
  (when-let* ((regexp (cdr (assoc-string (file-name-extension file)
                                         k/notes-title-regexps t))))
    (let ((search
           (lambda ()
             (save-excursion
               (save-restriction
                 (widen)
                 (goto-char (point-min))
                 (let ((case-fold-search t))
                   (when (re-search-forward regexp nil t)
                     (let ((title (match-string-no-properties 1)))
                       (unless (string-empty-p title) title)))))))))
      (if-let* ((buffer (get-file-buffer file)))
          (with-current-buffer buffer (funcall search))
        (with-temp-buffer
          (insert-file-contents file nil 0 4096)
          (funcall search))))))

(defun k/notes--items ()
  "Return the notes of `k/notes-dir', most recently modified first.
Every note is a pair of its completion string -- the file name, padded
to a common width, followed by the `#+title:' of the note -- and the
absolute file name, which the preview and the annotation take."
  (let* ((files (seq-filter #'file-regular-p
                            (directory-files k/notes-dir t "\\`[^.#].*[^~#]\\'")))
         (files (sort files #'file-newer-than-file-p))
         (width (seq-reduce (lambda (acc file)
                              (max acc (string-width (file-name-nondirectory file))))
                            files 0)))
    (mapcar (lambda (file)
              (let ((name (file-name-nondirectory file))
                    (title (k/notes--title file)))
                (cons (if title
                          (concat (truncate-string-to-width name width 0 ?\s)
                                  " "
                                  (propertize (truncate-string-to-width
                                               title 60 0 nil t)
                                              'face 'completions-annotations))
                        name)
                      file)))
            files)))

(defun k/notes--new (name)
  "Visit a new note NAME in `k/notes-dir'.
It is an .org file unless NAME has an extension of its own."
  (find-file (expand-file-name
              (if (file-name-extension name) name (concat name ".org"))
              k/notes-dir)))

(defvar k/notes--source
  (list :name     "Notes"
        :narrow   ?n
        ;; The `file' category lets Marginalia annotate, and Embark act on,
        ;; the absolute file name every candidate stands for.
        :category 'file
        :face     'consult-file
        :state    #'consult--file-state
        :items    #'k/notes--items
        :new      #'k/notes--new)
  "Notes source for `consult--multi', see `k/notes'.")

(defun k/notes ()
  "Switch to a note in `k/notes-dir', or create a new one.
The notes are listed whether they are visited in a buffer or not, most
recently modified first.  Typing narrows them by their file names and
their `#+title:' alike.  Typing a name that matches no note creates it,
as an .org file unless the name has an extension of its own."
  (interactive)
  (require 'consult)
  ;; The source lists the directory on every call, so it cannot be missing.
  (make-directory k/notes-dir t)
  (consult--multi (list k/notes--source)
                  :prompt "Note: "
                  :require-match (confirm-nonexistent-file-or-buffer)
                  :history 'k/notes-history
                  :sort nil))

(defun k/notes-search ()
  "Search the notes in `k/notes-dir' with `consult-ripgrep'."
  (interactive)
  (consult-ripgrep k/notes-dir))

(global-set-key (kbd "M-<f1>") #'k/notes)
(global-set-key (kbd "C-x M-<f1>") #'k/notes-search)

(provide 'notes)

;;; notes.el ends here
