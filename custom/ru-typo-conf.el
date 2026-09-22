;;; ru-typo-conf.el -- Correct a Russian word typed past the intended key. -*- lexical-binding: t -*-

;; The keyboard carries Latin legends only, so Russian is typed blind and
;; the misses are not arbitrary: the finger lands on a key next to the one
;; it was aiming at, and "привет" comes out as "поивет" or "ппивет".
;; That regularity is the whole method.  The word that has just been typed
;; is looked up in the Russian word list of `dict-conf.el'; if it is not
;; there, the words a single mis-hit away from it are looked up in turn,
;; and a lone hit is the correction.
;;
;; Every candidate carries the price of the mis-hit that would explain it:
;; a key to the left or right of the intended one is the likeliest miss and
;; costs the least, a dropped key the most (`k/ru-typo-costs').  The price
;; both ranks the candidates and decides who gets the last word:
;; corrections up to `k/ru-typo-quiet-cost' go into the buffer by
;; themselves, dearer ones are only named in the echo area.  A word the
;; dictionary knows is never touched, whatever it costs.
;;
;; The weak spot is the word the list has never heard of — "коммит",
;; "эмакс", a surname — where a correction would be vandalism.  Cheap
;; corrections rarely reach those (they have to spell another real word),
;; and for the rest there is `k/ru-typo-accept-word', which writes the word
;; into a personal list that is consulted first from then on.
;;
;; Why not a spell checker
;; -----------------------
;; A spell checker ranks its suggestions by an edit distance that knows
;; nothing about the keyboard: for "поивет" it weighs "привет", "полет"
;; and "совет" alike, while only the first is reachable by the miss that
;; actually happens.  Hunspell can be handed a keyboard — that is what
;; the KEY directive of an affix file is for — and ru_RU.aff does not
;; carry one.  The word list `dict-conf.el' already downloads (1.5 M
;; forms, binary-searched in place) answers the only question left: is
;; this a word, yes or no.
;;
;; Why not the LLM
;; ---------------
;; The local llama.cpp server answers in hundreds of milliseconds at best,
;; which is a long time to wait after every space, and it answers
;; differently to the same word on two different days — including for the
;; words that were never mistyped.  Generate-and-test costs one dictionary
;; lookup for a word that is spelled right, which is nearly all of them,
;; and it can only ever replace a non-word with a word.  The LLM is kept
;; for the one thing it is better at: choosing among several candidates by
;; the sentence around them, which is `k/ru-typo-on-ambiguity' set to
;; `llm'.

(require 'dict-conf)
(require 'seq)

(defvar k/ru-typo-rows
  '("йцукенгшщзхъ"
    "фывапролджэ"
    "ячсмитьбю")
  "The letter rows of the ЙЦУКЕН layout, top to bottom.
Each row sits half a key to the right of the one above it, so the key at
index I of a row touches the keys at indices I and I+1 of the row above —
the way `a' touches `q' and `w' on the Latin side of the same keycaps.")

(defvar k/ru-typo-costs
  '((same-row      . 1)
    (doubled       . 1)
    (other-row     . 2)
    (yo            . 2)
    (transposition . 2)
    (extra         . 3)
    (missing       . 4))
  "What each kind of mis-hit costs, as an alist of (KIND . COST).

  `same-row'       the key left or right of the intended one
  `doubled'        a key that bounced and typed its letter twice
  `other-row'      a key on the row above or below
  `yo'             `е' where `ё' belongs, or the other way round
  `transposition'  the second key of a pair beating the first
  `extra'          a neighbouring key caught by the side of the finger
  `missing'        a key that never registered at all

The order is the point: a finger wanders along a row more readily than
across one, a doubled letter tells its own story, and a letter that is
simply not there is the last thing to suspect.  The cost ranks the
candidates and, through `k/ru-typo-quiet-cost', decides which of them
may be applied unasked.")

(defvar k/ru-typo-quiet-cost 2
  "Dearest correction still put in the buffer without asking.
Up to this price a correction is a near certainty — a neighbouring key,
a swap, a bounce.  Above it the candidate is only named in the echo
area, because that is where the guessing starts and where a word the
dictionary has never heard of would get mangled.  Raise it to 4 to have
every candidate applied, set it to 0 to be told and never corrected.
`k/ru-typo-correct-word', called by hand, ignores it: being called is
the answer to whether the correction is wanted.")

(defvar k/ru-typo-min-length 4
  "Shortest word that is corrected, and shortest correction offered.
Below this the method stops being a method: among three letters nearly
every neighbouring key spells another real word, so a \"correction\"
would be a coin toss.")

(defvar k/ru-typo-generators
  '((k/ru-typo--substitutions   . same-row)
    (k/ru-typo--doubled-letters . doubled)
    (k/ru-typo--transpositions  . transposition)
    (k/ru-typo--caught-letters  . extra)
    (k/ru-typo--missing-letters . missing))
  "How a typed word may differ from the intended one.
Each entry pairs a generator with the cheapest mis-hit it can explain,
a key of `k/ru-typo-costs', and the list runs from the cheapest to the
dearest.  Every generator that could still beat or match what has been
found is asked — two explanations of the same price are both worth
hearing, and one of them being asked first is no argument — and the
search stops as soon as the rest could only be dearer.  That is what
keeps `k/ru-typo--missing-letters', which is far and away the slowest,
out of the common case.")

(defvar k/ru-typo-on-ambiguity 'report
  "What `k/ru-typo-mode' does when several words are one mis-hit away.

  `report'  name them in the echo area and change nothing (the default)
  `best'    take the likeliest one, if `k/ru-typo-quiet-cost' allows
  `ask'     ask, through `completing-read'
  `llm'     let the local LLM pick by the sentence around the word

`report' by default because the choice really is open: where several
words are equally near, the likeliest one is right about half the time.")

(defvar k/ru-typo-dictionary
  (expand-file-name "ru.txt" k/dict-directory)
  "The word list a correction has to be found in.
Written by `k/dict-download'.")

(defvar k/ru-typo-personal-dictionary
  (expand-file-name "ru-personal.txt" k/dict-directory)
  "Words of one's own that the big list does not carry, one per line.
Jargon, names, anything typed often enough to be safe from correction.
`k/ru-typo-accept-word' adds to it; `k/dict-directory' is outside the
repository, so the file stays private.")

;;-------------------------------------------------------------------
;; The keyboard

(defvar k/ru-typo--neighbours nil
  "Hash table built by `k/ru-typo-neighbours', or nil before the first call.")

(defun k/ru-typo-cost (kind)
  "Return what a mis-hit of KIND costs, after `k/ru-typo-costs'."
  (or (cdr (assq kind k/ru-typo-costs)) 1))

(defun k/ru-typo--touch (table a b cost)
  "Record in TABLE that keys A and B are COST apart, both ways round."
  (unless (eq a b)
    (push (cons b cost) (gethash a table))
    (push (cons a cost) (gethash b table))))

(defun k/ru-typo-neighbours ()
  "Return the table mapping each key to the keys around it.
A value is an alist of (CHARACTER . COST).  Built once from
`k/ru-typo-rows'; call `k/ru-typo-forget-keyboard' after changing those."
  (or k/ru-typo--neighbours
      (setq k/ru-typo--neighbours
            (let ((table (make-hash-table :test #'eq)))
              (dotimes (index (length k/ru-typo-rows))
                (let ((row (nth index k/ru-typo-rows))
                      (above (and (> index 0) (nth (1- index) k/ru-typo-rows))))
                  (dotimes (position (length row))
                    (let ((key (aref row position)))
                      (when (< (1+ position) (length row))
                        (k/ru-typo--touch table key (aref row (1+ position))
                                          (k/ru-typo-cost 'same-row)))
                      (when above
                        ;; The half-key offset: this key straddles the two
                        ;; keys of the row above it.
                        (when (< position (length above))
                          (k/ru-typo--touch table key (aref above position)
                                            (k/ru-typo-cost 'other-row)))
                        (when (< (1+ position) (length above))
                          (k/ru-typo--touch table key (aref above (1+ position))
                                            (k/ru-typo-cost 'other-row))))))))
              ;; `ё' is off in the corner and next to nothing, but it is
              ;; left out of a word exactly the way a mis-hit letter is.
              (k/ru-typo--touch table ?е ?ё (k/ru-typo-cost 'yo))
              table))))

(defun k/ru-typo-forget-keyboard ()
  "Drop the keyboard table and the cached corrections, so both rebuild."
  (interactive)
  (setq k/ru-typo--neighbours nil)
  (k/ru-typo-forget-words))

(defun k/ru-typo-alphabet ()
  "Return every letter the layout can produce, as a string."
  (concat (apply #'concat k/ru-typo-rows) "ё"))

;;-------------------------------------------------------------------
;; What the word could have been
;;
;; A generator takes the typed word and returns an alist of (CANDIDATE
;; . COST).  It does not ask whether a candidate is a word; that is the
;; dictionary's part, in `k/ru-typo-corrections'.

(defun k/ru-typo--substitutions (word)
  "Return WORD with one letter replaced by a key next to it."
  (let ((table (k/ru-typo-neighbours))
        (result nil))
    (dotimes (index (length word))
      (dolist (neighbour (gethash (aref word index) table))
        (let ((candidate (copy-sequence word)))
          (aset candidate index (car neighbour))
          (push (cons candidate (cdr neighbour)) result))))
    result))

(defun k/ru-typo--doubled-letters (word)
  "Return WORD with a repeated letter reduced to a single one.
The key bounced, or was held a moment too long.  Unmistakable as
mis-hits go: the twin of a letter is never what the word wanted."
  (let ((result nil))
    (dotimes (index (length word))
      (when (and (> index 0) (eq (aref word index) (aref word (1- index))))
        (push (cons (concat (substring word 0 index) (substring word (1+ index)))
                    (k/ru-typo-cost 'doubled))
              result)))
    result))

(defun k/ru-typo--transpositions (word)
  "Return WORD with two neighbouring letters swapped.
The second key of a fast pair beats the first to the switch."
  (let ((result nil))
    (dotimes (index (1- (length word)))
      (let ((candidate (copy-sequence word)))
        (aset candidate index (aref word (1+ index)))
        (aset candidate (1+ index) (aref word index))
        (push (cons candidate (k/ru-typo-cost 'transposition)) result)))
    result))

(defun k/ru-typo--caught-letters (word)
  "Return WORD with one stray letter dropped, next to the one that wanted it.
The finger caught the key beside the one it was aiming at and typed
both.  Only a letter that neighbours the letter before or after it on
the keyboard counts: anything else was meant to be there, which is what
keeps the corrector off the words it has simply never seen."
  (let ((table (k/ru-typo-neighbours))
        (result nil))
    (dotimes (index (length word))
      (let* ((letter (aref word index))
             (before (and (> index 0) (aref word (1- index))))
             (after (and (< (1+ index) (length word)) (aref word (1+ index)))))
        (when (or (and before (assq before (gethash letter table)))
                  (and after (assq after (gethash letter table))))
          (push (cons (concat (substring word 0 index) (substring word (1+ index)))
                      (k/ru-typo-cost 'extra))
                result))))
    result))

(defun k/ru-typo--missing-letters (word)
  "Return WORD with one letter inserted, for the key that never registered.
Every letter at every position: the widest net of the generators, and
the reason it is cast last.  A word of ten letters yields some three
hundred candidates, three hundred dictionary lookups — a hundred
milliseconds, paid once per word thanks to `k/ru-typo--corrections', and
only for a word already known to be broken."
  (let ((alphabet (k/ru-typo-alphabet))
        (cost (k/ru-typo-cost 'missing))
        (result nil))
    (dotimes (index (1+ (length word)))
      (dotimes (letter (length alphabet))
        (push (cons (concat (substring word 0 index)
                            (string (aref alphabet letter))
                            (substring word index))
                    cost)
              result)))
    result))

;;-------------------------------------------------------------------
;; The dictionary's verdict

(defvar k/ru-typo--personal nil
  "Hash table of the words in `k/ru-typo-personal-dictionary', or nil.")

(defvar k/ru-typo--corrections (make-hash-table :test #'equal)
  "What `k/ru-typo-corrections' has already worked out, by word.
A word is asked about again every time it is typed, and the answer
cannot change until a dictionary does.")

(defun k/ru-typo-personal-words ()
  "Return the personal words as a hash table, read from file once."
  (or k/ru-typo--personal
      (setq k/ru-typo--personal
            (let ((table (make-hash-table :test #'equal)))
              (when (file-readable-p k/ru-typo-personal-dictionary)
                (with-temp-buffer
                  (let ((coding-system-for-read 'utf-8))
                    (insert-file-contents k/ru-typo-personal-dictionary))
                  (dolist (word (split-string (buffer-string) "\n" t))
                    (puthash (downcase (string-trim word)) t table))))
              table))))

(defun k/ru-typo-forget-words ()
  "Drop the personal word list and the cached corrections from memory."
  (interactive)
  (setq k/ru-typo--personal nil)
  (clrhash k/ru-typo--corrections))

(defun k/ru-typo-known-p (word)
  "Return non-nil when WORD is spelled the way the word lists spell it."
  (let ((word (downcase word)))
    (or (gethash word (k/ru-typo-personal-words))
        (k/dict-known-word-p word (list k/ru-typo-dictionary)))))

;;;###autoload
(defun k/ru-typo-accept-word (&optional word)
  "Add WORD, or the word at point, to `k/ru-typo-personal-dictionary'.
The corrector leaves it alone from then on — and may reach it as a
correction for something else, which is the other half of the point."
  (interactive)
  (let* ((word (downcase (or word
                             (let ((bounds (k/ru-typo--word-bounds)))
                               (and bounds (buffer-substring-no-properties
                                            (car bounds) (cdr bounds))))
                             (read-string "Accept word: ")))))
    (if (string-empty-p word)
        (message "No word here")
      (make-directory (file-name-directory k/ru-typo-personal-dictionary) t)
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region (concat word "\n") nil k/ru-typo-personal-dictionary
                      'append 'quiet))
      (k/ru-typo-forget-words)
      (message "%s is a word now" word))))

(defun k/ru-typo-word-p (word)
  "Return non-nil when WORD is Russian and long enough to be worth checking."
  (and (stringp word)
       (>= (length word) k/ru-typo-min-length)
       (string-match-p "\\`[а-яё]+\\'" (downcase word))))

(defun k/ru-typo--likelier-p (a b)
  "Return non-nil when correction A is likelier than B.
Cheaper first, and alphabetically among equals, so that a word offers
the same list every time it is asked."
  (if (= (cdr a) (cdr b))
      (string< (car a) (car b))
    (< (cdr a) (cdr b))))

(defun k/ru-typo--rank (corrections)
  "Return CORRECTIONS without repeats, each at its lowest cost, likeliest first.
The same word is reached again and again — from two positions, from two
generators — and the cheapest way to reach it is the one that counts."
  (let ((seen (make-hash-table :test #'equal))
        (result nil))
    (dolist (correction corrections)
      (let ((cost (gethash (car correction) seen)))
        (when (or (null cost) (< (cdr correction) cost))
          (puthash (car correction) (cdr correction) seen))))
    (maphash (lambda (word cost) (push (cons word cost) result)) seen)
    (sort result #'k/ru-typo--likelier-p)))

(defun k/ru-typo--winnow (candidates word)
  "Return the dictionary words among CANDIDATES, WORD itself excluded.
Each distinct candidate is looked up once, however many ways the
generator found to spell it."
  (let ((seen (make-hash-table :test #'equal))
        (result nil))
    (dolist (candidate candidates)
      (let ((cost (gethash (car candidate) seen)))
        (when (or (null cost) (< (cdr candidate) cost))
          (puthash (car candidate) (cdr candidate) seen))))
    (maphash (lambda (candidate cost)
               (when (and (not (equal candidate word))
                          (>= (length candidate) k/ru-typo-min-length)
                          (k/ru-typo-known-p candidate))
                 (push (cons candidate cost) result)))
             seen)
    (k/ru-typo--rank result)))

(defun k/ru-typo--corrections (word)
  "Work out what WORD may be a typo for; see `k/ru-typo-corrections'."
  (unless (k/ru-typo-known-p word)
    (let ((corrections nil))
      (dolist (entry k/ru-typo-generators)
        ;; Whatever this generator finds would cost more than what is
        ;; already in hand, so it is not asked.  The generators are in
        ;; rising order of price, so this holds for all that follow.
        (unless (and corrections
                     (< (cdar corrections) (k/ru-typo-cost (cdr entry))))
          (setq corrections
                (k/ru-typo--rank
                 (append corrections
                         (k/ru-typo--winnow (funcall (car entry) word) word))))))
      corrections)))

(defun k/ru-typo-corrections (word)
  "Return what WORD may be a typo for, likeliest first, as (WORD . COST).
Nil when WORD is a word in its own right, or when no generator in
`k/ru-typo-generators' reaches one.  Answers are remembered until
`k/ru-typo-forget-words' throws them away."
  (let* ((word (downcase word))
         (cached (gethash word k/ru-typo--corrections 'none)))
    (if (not (eq cached 'none))
        cached
      (puthash word (k/ru-typo--corrections word) k/ru-typo--corrections))))

;;-------------------------------------------------------------------
;; Putting a correction in the buffer

(defun k/ru-typo--replace (beg end word correction)
  "Replace the WORD between BEG and END with CORRECTION, cased like WORD.
Point is saved as a marker, so it keeps its place in the text when the
correction is not as long as the word was."
  (let ((replacement (k/dict--restore-case word correction)))
    (save-excursion
      (goto-char beg)
      (delete-region beg end)
      (insert replacement))
    (message "%s → %s" word replacement)))

(defun k/ru-typo--announce (word corrections)
  "Say in the echo area that WORD could be any of CORRECTIONS."
  (message "%s: %s?" word
           (mapconcat #'car (seq-take corrections 5) ", ")))

(defun k/ru-typo--afford-p (correction limit)
  "Return non-nil when CORRECTION may be applied without asking.
LIMIT is the dearest mis-hit still taken on trust, or nil for any."
  (or (null limit) (<= (cdr correction) limit)))

(defun k/ru-typo--apply (bounds word corrections ambiguity &optional limit)
  "Correct WORD, which lies between the car and cdr of BOUNDS.
CORRECTIONS come from `k/ru-typo-corrections'.  A single candidate is
taken as it stands, as long as `k/ru-typo--afford-p' allows it at LIMIT;
a dearer one is only named, because past that price a correction is a
guess.  Several candidates are settled the way AMBIGUITY says, which
takes the values of `k/ru-typo-on-ambiguity'."
  (let ((beg (car bounds))
        (end (cdr bounds))
        (best (car corrections)))
    (cond
     ((null corrections) nil)
     ((null (cdr corrections))
      (if (k/ru-typo--afford-p best limit)
          (k/ru-typo--replace beg end word (car best))
        (k/ru-typo--announce word corrections)))
     (t
      (pcase ambiguity
        ('best (if (k/ru-typo--afford-p best limit)
                   (k/ru-typo--replace beg end word (car best))
                 (k/ru-typo--announce word corrections)))
        ('ask (k/ru-typo--replace
               beg end word
               (completing-read (format "%s → " word) (mapcar #'car corrections)
                                nil t nil nil (car best))))
        ('llm (k/ru-typo--llm-choose beg end word corrections))
        (_ (k/ru-typo--announce word corrections)))))))

(defun k/ru-typo--word-bounds ()
  "Return the bounds of the word at point, or of the one just before it."
  (or (bounds-of-thing-at-point 'word)
      (save-excursion
        (skip-syntax-backward "^w" (line-beginning-position))
        (bounds-of-thing-at-point 'word))))

;;;###autoload
(defun k/ru-typo-correct-word ()
  "Correct the Russian word at point, or the one just before it.
The word is left alone unless the dictionary refuses it; then every word
a single mis-hit away is offered, `k/ru-typo-quiet-cost' notwithstanding
— the command was called for that."
  (interactive)
  (let ((bounds (k/ru-typo--word-bounds)))
    (if (null bounds)
        (message "No word here")
      (let* ((word (buffer-substring-no-properties (car bounds) (cdr bounds)))
             (corrections (and (k/ru-typo-word-p word)
                               (k/ru-typo-corrections word))))
        (cond
         ((not (k/ru-typo-word-p word)) (message "%s: not a Russian word" word))
         (corrections (k/ru-typo--apply bounds word corrections 'ask))
         ((k/ru-typo-known-p word) (message "%s is spelled right" word))
         (t (message "%s: no word is one mis-hit away" word)))))))

;;;###autoload
(defun k/ru-typo-correct-region (beg end)
  "Correct every Russian word between BEG and END.
Only the corrections `k/ru-typo-quiet-cost' vouches for are applied; the
doubtful and the ambiguous ones are counted and left for
`k/ru-typo-correct-word' to settle."
  (interactive "r")
  (let ((end (copy-marker end))
        (corrected 0)
        (doubtful nil))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "[а-яёА-ЯЁ]+" end t)
        (let* ((word (match-string-no-properties 0))
               (bounds (cons (match-beginning 0) (match-end 0)))
               (corrections (and (k/ru-typo-word-p word)
                                 (k/ru-typo-corrections word))))
          (when corrections
            (if (and (null (cdr corrections))
                     (k/ru-typo--afford-p (car corrections) k/ru-typo-quiet-cost))
                (progn (k/ru-typo--replace (car bounds) (cdr bounds) word
                                           (caar corrections))
                       (setq corrected (1+ corrected)))
              (push word doubtful))))))
    (set-marker end nil)
    (message "%d word%s corrected%s" corrected (if (= corrected 1) "" "s")
             (if doubtful
                 (format ", %d left to decide: %s"
                         (length doubtful)
                         (mapconcat #'identity (nreverse doubtful) ", "))
               ""))))

;;-------------------------------------------------------------------
;; Correcting as the word is finished

(defun k/ru-typo--post-self-insert ()
  "Correct the word the character just typed has finished.
Anything that is not a letter ends a word: a space, a comma, a newline.
Inside a `prog-mode' buffer this defers to `k/dict-prog-mode-completion',
the same setting that decides where the dictionary completes, so the
corrector stays out of the code and works on the comments."
  (when (and (characterp last-command-event)
             (not (eq (char-syntax last-command-event) ?w))
             (not buffer-read-only)
             (> (point) (point-min))
             (k/dict-capf-active-p))
    (save-excursion
      (backward-char)
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (when (and bounds (= (cdr bounds) (point)))
          (let ((word (buffer-substring-no-properties (car bounds) (cdr bounds))))
            (when (k/ru-typo-word-p word)
              (k/ru-typo--apply bounds word (k/ru-typo-corrections word)
                                k/ru-typo-on-ambiguity
                                k/ru-typo-quiet-cost))))))))

;;;###autoload
(define-minor-mode k/ru-typo-mode
  "Correct Russian words typed past the intended key, as they are finished.
A word the dictionary knows costs one lookup and is never touched; only
a word it refuses is taken apart by `k/ru-typo-generators'.  The
correction lands in the same command as the character that finished the
word, so a single \\[undo] takes both back, and
\\[k/ru-typo-accept-word] tells the corrector to leave that word alone
for good."
  :lighter " ru-typo"
  (if k/ru-typo-mode
      (progn
        (unless (file-readable-p k/ru-typo-dictionary)
          (message "No word list at %s — run M-x k/dict-download"
                   k/ru-typo-dictionary))
        (add-hook 'post-self-insert-hook #'k/ru-typo--post-self-insert nil t))
    (remove-hook 'post-self-insert-hook #'k/ru-typo--post-self-insert t)))

;; Typing Russian prose is what `text-mode' is for; uncomment to have the
;; corrector on from the start there.
;; (add-hook 'text-mode-hook #'k/ru-typo-mode)

;;-------------------------------------------------------------------
;; The LLM as a tie-breaker
;;
;; Only reached with `k/ru-typo-on-ambiguity' set to `llm', and only for
;; the words where the keyboard alone cannot decide.  The request is
;; asynchronous and the typing does not stop for it, so the word is held
;; by markers and the answer is dropped unless it names one of the
;; candidates and the word is still where it was left.

(defvar k/ru-typo-llm-context-chars 200
  "How much text before the word is sent to the LLM as context.")

(declare-function gptel-request "gptel")

(defun k/ru-typo--llm-answer (response candidates)
  "Return the candidate of CANDIDATES that RESPONSE names, if it names one."
  (when (stringp response)
    (let ((answer (downcase (string-trim response "[ \t\n\r\"'`.,]+"
                                         "[ \t\n\r\"'`.,]+"))))
      (car (member answer candidates)))))

(defun k/ru-typo--llm-choose (beg end word corrections)
  "Ask the LLM which of CORRECTIONS the WORD between BEG and END should be."
  (if (not (require 'gptel nil t))
      (k/ru-typo--announce word corrections)
    (let* ((candidates (mapcar #'car corrections))
           (start (copy-marker beg))
           (stop (copy-marker end t))
           (context (buffer-substring-no-properties
                     (max (point-min) (- beg k/ru-typo-llm-context-chars)) beg)))
      (k/ru-typo--announce word corrections)
      (gptel-request
          (format (concat "Текст: %s[%s]\n"
                          "Слово в скобках напечатано с опечаткой. "
                          "Варианты: %s.\n"
                          "Ответь одним словом из списка вариантов.")
                  context word (mapconcat #'identity candidates ", "))
        :stream nil
        :system (concat "Ты корректор русского текста. "
                        "Отвечай ровно одним словом из предложенного списка, "
                        "без пояснений и без знаков препинания.")
        :callback
        (lambda (response _info)
          (let ((choice (k/ru-typo--llm-answer response candidates)))
            (when (and choice
                       (marker-buffer start)
                       (equal word (buffer-substring-no-properties start stop)))
              (with-current-buffer (marker-buffer start)
                (k/ru-typo--replace start stop word choice)))
            (set-marker start nil)
            (set-marker stop nil)))))))

(provide 'ru-typo-conf)
