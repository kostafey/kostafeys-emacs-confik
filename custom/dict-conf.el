;;; dict-conf.el -- Dictionary word completion for corfu. -*- lexical-binding: t -*-

;; Completes any English or Russian word from a plain word list, as a
;; `completion-at-point' function, so the candidates show up in the usual
;; `corfu' popup next to the other capfs.
;;
;; Why not `cape-dict'
;; -------------------
;; `cape-dict' shells out to grep, which does not survive this platform:
;;
;; - The Git for Windows GNU grep 3.0 aborts (SIGABRT) whenever `-i' is
;;   passed, even for a pure ASCII pattern.  `cape-dict' passes `-i' as soon
;;   as `cape-dict-case-fold' is non-nil, which is the default.
;; - Emacs encodes subprocess arguments in the Windows ANSI code page (cp1252
;;   here, see `w32-ansi-code-page'), so a Cyrillic search pattern is mangled
;;   before grep ever sees it and never matches a UTF-8 word list.
;;
;; Instead the word list is searched inside Emacs.  `k/dict-download' writes
;; the lists down-cased, de-duplicated and sorted by `string<', which lets
;; `k/dict--lower-bound' binary-search the buffer: a lookup costs ~20 line
;; comparisons instead of a full scan of a 35 MB file.  The case of the typed
;; prefix is restored afterwards by `k/dict--restore-case'.

(require 'url)

(defvar k/dict-directory (expand-file-name "dict" user-emacs-directory)
  "Directory holding the word list files.")

(defvar k/dict-sources
  '(("en.txt" "https://raw.githubusercontent.com/dwyl/english-words/master/words_alpha.txt" utf-8)
    ("ru.txt" "https://raw.githubusercontent.com/danakt/russian-words/master/russian.txt" windows-1251))
  "Word lists to fetch: (FILE-NAME URL CODING).
CODING is the coding system of the downloaded file; it is re-encoded to
UTF-8 on the way in.")

(defvar k/dict-files
  (list (expand-file-name "en.txt" k/dict-directory)
        (expand-file-name "ru.txt" k/dict-directory))
  "Word list files searched by `k/dict-capf', in order of preference.")

(defvar k/dict-limit 40
  "Maximal number of candidates returned per word list.")

(defvar k/dict-min-prefix 3
  "Shortest prefix that triggers a dictionary lookup.
Below this the popup is noise: two letters match thousands of words.")

(defvar k/dict-prog-mode-completion 'comments
  "Where dictionary words are offered in `prog-mode' buffers.

  nil         nowhere, the popup holds identifiers only
  `comments'  in comments and strings, which is where the prose is
  t           everywhere in the buffer

`comments' by default: a comment is written in English or Russian like
any other text, while the code around it is not.  `cape-dabbrev' pays
no attention to this setting, so a code buffer completes the words of
the buffers around it whichever value is in force.

Cycle it with `k/dict-cycle-prog-mode-completion', or set it
buffer-locally, from a mode hook, to decide one language at a time.")

(defconst k/dict-prog-mode-states '(nil comments t)
  "The values `k/dict-prog-mode-completion' cycles through.")

(defun k/dict--state-description (state)
  "Return how STATE of `k/dict-prog-mode-completion' reads out loud."
  (pcase state
    ('nil "off")
    ('comments "comments and strings")
    (_ "everywhere")))

(defun k/dict-cycle-prog-mode-completion (&optional buffer-only)
  "Cycle dictionary completion in code buffers.
The states follow `k/dict-prog-mode-states': off, comments and strings,
everywhere.  With a prefix argument BUFFER-ONLY, cycle it in this buffer
alone and leave the other buffers as they are."
  (interactive "P")
  (let* ((rest (cdr (memq k/dict-prog-mode-completion k/dict-prog-mode-states)))
         (value (car (or rest k/dict-prog-mode-states))))
    (if buffer-only
        (setq-local k/dict-prog-mode-completion value)
      (kill-local-variable 'k/dict-prog-mode-completion)
      (setq-default k/dict-prog-mode-completion value))
    (message "Dictionary completion in code buffers: %s%s"
             (k/dict--state-description value)
             (if buffer-only " (this buffer)" ""))))

;;-------------------------------------------------------------------
;; Word list preparation

(defun k/dict--normalize (file)
  "Down-case, de-duplicate and sort FILE in place.
The result is sorted by `string<', the order `k/dict--lower-bound'
binary-searches on.  Returns the number of words written."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (downcase-region (point-min) (point-max))
    ;; No TRIM argument to `split-string' here: it matches "[ \t]+\\'" from
    ;; every field to the end of the whole string, which turns the split of a
    ;; multi-megabyte word list quadratic.  Carriage returns are already gone,
    ;; `insert-file-contents' converts the line endings when it decodes.
    (let ((words (sort (split-string (buffer-string) "\n" t) #'string<))
          (unique nil))
      ;; The list is sorted, so duplicates are adjacent.
      (dolist (word words)
        (unless (equal word (car unique))
          (push word unique)))
      (setq unique (nreverse unique))
      (erase-buffer)
      (insert (mapconcat #'identity unique "\n") "\n")
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region (point-min) (point-max) file nil 'quiet))
      (length unique))))

(defun k/dict-download (&optional force)
  "Download the word lists in `k/dict-sources' into `k/dict-directory'.
Existing files are kept unless FORCE is non-nil (\\[universal-argument])."
  (interactive "P")
  (make-directory k/dict-directory t)
  (dolist (source k/dict-sources)
    (pcase-let* ((`(,name ,url ,coding) source)
                 (target (expand-file-name name k/dict-directory)))
      (if (and (file-exists-p target) (not force))
          (message "Dictionary %s already downloaded" name)
        (message "Downloading %s..." url)
        (let ((temp (make-temp-file "k-dict-")))
          (unwind-protect
              (progn
                (url-copy-file url temp t)
                (with-temp-buffer
                  (let ((coding-system-for-read coding))
                    (insert-file-contents temp))
                  (let ((coding-system-for-write 'utf-8-unix))
                    (write-region (point-min) (point-max) target nil 'quiet))))
            (delete-file temp)))
        (message "Normalizing %s..." name)
        (k/dict-unload)
        (message "Dictionary %s ready: %d words"
                 name (k/dict--normalize target))))))

;;-------------------------------------------------------------------
;; Lookup

(defvar k/dict--buffers nil
  "Alist of (FILE . BUFFER) holding the loaded word lists.")

(define-derived-mode k/dict-mode fundamental-mode "Dict"
  "Major mode of the buffers holding the word lists.
It exists to keep those buffers out of the commands that collect words
from every buffer sharing the current major mode: `dabbrev-expand' and
`cape-dabbrev' both do, and they would otherwise scrape the whole word
list, drowning the popup in hundreds of candidates."
  (setq buffer-undo-list t
        buffer-read-only t))

(defun k/dict--buffer (file)
  "Return a buffer with the contents of word list FILE, loading it on demand."
  (let ((buffer (cdr (assoc file k/dict--buffers))))
    (unless (buffer-live-p buffer)
      (when (file-readable-p file)
        (message "Loading dictionary %s..." (file-name-nondirectory file))
        (setq buffer (get-buffer-create
                      (format " *dict %s*" (file-name-nondirectory file))))
        (with-current-buffer buffer
          (k/dict-mode)
          (let ((coding-system-for-read 'utf-8)
                (inhibit-read-only t))
            (insert-file-contents file)))
        (setf (alist-get file k/dict--buffers nil nil #'equal) buffer)
        (message nil)))
    buffer))

(defun k/dict-unload ()
  "Drop the loaded word lists from memory."
  (interactive)
  (dolist (entry k/dict--buffers)
    (when (buffer-live-p (cdr entry))
      (kill-buffer (cdr entry))))
  (setq k/dict--buffers nil))

(defun k/dict--line ()
  "Return the current line of the word list buffer."
  (buffer-substring-no-properties (point) (line-end-position)))

(defun k/dict--lower-bound (prefix)
  "Put point at the first line that is not `string<' than PREFIX.
The buffer has to be sorted by `string<'.  Both branches either advance
LO past a whole line or shrink HI, so the loop always terminates."
  (let ((lo (point-min))
        (hi (point-max)))
    (while (< lo hi)
      (goto-char (+ lo (/ (- hi lo) 2)))
      (beginning-of-line)
      (when (<= (point) lo)
        ;; The midpoint landed inside the LO line itself.
        (goto-char lo))
      (if (string< (k/dict--line) prefix)
          (progn (forward-line 1)
                 (setq lo (point)))
        (setq hi (point))))
    (goto-char lo)))

(defun k/dict--restore-case (input word)
  "Give WORD the capitalization of INPUT.
The word lists are stored down-cased, so a word completed after a
capitalized prefix has to be capitalized back."
  (cond
   ((string= input (downcase input)) word)
   ((string-empty-p word) word)
   ((and (> (length input) 1) (string= input (upcase input))) (upcase word))
   (t (concat (upcase (substring word 0 1)) (substring word 1)))))

(defun k/dict--matches (file prefix)
  "Return up to `k/dict-limit' words of word list FILE starting with PREFIX."
  (let ((buffer (k/dict--buffer file)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (k/dict--lower-bound prefix)
          (let ((words nil)
                (count 0))
            (while (and (< count k/dict-limit)
                        (not (eobp))
                        (string-prefix-p prefix (k/dict--line)))
              (push (k/dict--line) words)
              (setq count (1+ count))
              (forward-line 1))
            (nreverse words)))))))

(defun k/dict--member-p (file word)
  "Return non-nil when WORD is a line of word list FILE.
WORD has to be down-cased already: the word lists are."
  (let ((buffer (k/dict--buffer file)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (k/dict--lower-bound word)
          (equal word (k/dict--line)))))))

(defun k/dict-known-word-p (word &optional files)
  "Return non-nil when WORD is spelled the way FILES spell it.
FILES default to `k/dict-files\='.  The lookup is the binary search of
`k/dict--lower-bound\=', so asking after a single word costs the same
handful of line comparisons a completion does."
  (let ((word (downcase word)))
    (seq-some (lambda (file) (k/dict--member-p file word))
              (or files k/dict-files))))

(defun k/dict-words (input)
  "Return the dictionary words completing INPUT, cased like INPUT."
  (let ((prefix (downcase input)))
    (mapcar (lambda (word) (k/dict--restore-case input word))
            (mapcan (lambda (file) (k/dict--matches file prefix))
                    k/dict-files))))

;;;###autoload
(defun k/dict--prose-at-point-p ()
  "Return non-nil when point sits in a comment or in a string."
  (let ((state (syntax-ppss)))
    (or (nth 3 state)                   ; inside a string
        (nth 4 state))))                ; inside a comment

(defun k/dict-capf-active-p ()
  "Return non-nil when the dictionary should answer at point.
Outside `prog-mode' it always should; inside, that is what
`k/dict-prog-mode-completion' decides."
  (if (derived-mode-p 'prog-mode)
      (pcase k/dict-prog-mode-completion
        ('nil nil)
        ('comments (k/dict--prose-at-point-p))
        (_ t))
    t))

(defun k/dict-capf ()
  "Complete the word at point from the dictionaries in `k/dict-files'.
Answers in every buffer but a `prog-mode' one, where it waits for
`k/dict-prog-mode-completion'."
  (let ((beg (and (k/dict-capf-active-p)
                  (car (bounds-of-thing-at-point 'word)))))
    (when (and beg (>= (- (point) beg) k/dict-min-prefix))
      (let ((buffer (current-buffer))
            (start (copy-marker beg))
            (stop (copy-marker (point) t)))
        (list beg (point)
              (completion-table-dynamic
               (lambda (_input)
                 ;; The typed prefix is read from the buffer instead of the
                 ;; argument: non-prefix completion styles, `fussy' and
                 ;; `substring' among them, pass an empty string here.  STOP
                 ;; advances with the text typed after the popup opened.
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (k/dict-words
                      (buffer-substring-no-properties start stop))))))
              :annotation-function (lambda (_) " Dict")
              :company-kind (lambda (_) 'text)
              :exclusive 'no)))))

;;-------------------------------------------------------------------
;; Wiring into corfu

(use-package cape
  :straight '(cape :type git :host github
                   :repo "minad/cape" :branch "main")
  :config
  (defun k/dict-dabbrev-capf ()
    "Dictionary words and words of the open buffers, merged into one capf."
    (funcall (cape-capf-super #'k/dict-capf #'cape-dabbrev)))

  ;; Installed globally rather than on `text-mode-hook': `fundamental-mode'
  ;; does not derive from `text-mode', and with the tags capf gone (see
  ;; `completition-corfu-conf.el') such buffers would have no completion at
  ;; all.  A buffer-local capf list keeps its own entries first and reaches
  ;; this one through the `t' element at its end, so a major mode's own
  ;; completion still wins.
  (add-hook 'completion-at-point-functions #'k/dict-dabbrev-capf 90))

(provide 'dict-conf)
