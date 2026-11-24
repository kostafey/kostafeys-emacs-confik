;;; agent-review.el --- Review diffs for agents -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, vc

;;; Commentary:

;; Comment the hunks of a diff the way a pull request is reviewed on
;; GitHub, then send the comments to a coding agent of the project in one
;; prompt.  Each comment carries the diff text it refers to: the hunk at
;; point, or the whole lines of an active region inside it.  Until they
;; are sent, the comments show under those lines, and diff buffers keep
;; showing them across refreshes.
;;
;; `agent-review-comment' comments the hunk or region at point; on an
;; already commented one it edits the comment, which an empty comment
;; removes.  `agent-review-send' sends the repository's comments to its
;; running agent, or to one running for a directory above it, asking
;; which one when several run.
;;
;; Diff buffers come from `agent-review-backends', which knows magit and
;; multi-magit; agents from `agent-review-agents', which knows
;; `codex-ide' and `pi-ide'.  Both lists take more entries.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defgroup agent-review nil
  "Review diffs for coding agents."
  :group 'tools :prefix "agent-review-")

(defcustom agent-review-agents
  '(("Codex" codex-ide-session-buffers agent-ide-send-to-buffer)
    ("Pi" pi-ide-session-buffers agent-ide-send-to-buffer))
  "Agents that review comments go to, as (NAME SESSIONS SEND).
SESSIONS (DIRECTORY) returns the live sessions of the agent run for
DIRECTORY itself, not for a directory above or below it: each a
buffer, or (LABEL . SESSION) for a session of another kind.  SEND
\(SESSION TEXT) submits the prompt TEXT to one of them.  Agents whose
SESSIONS is not defined are skipped."
  :type '(repeat (list string function function)))

(defcustom agent-review-backends
  '((multi-magit-status-mode :root agent-review--multi-magit-root
                             :hunk agent-review--magit-hunk
                             :location agent-review--magit-location)
    (magit-mode :root agent-review--magit-root
                :hunk agent-review--magit-hunk
                :location agent-review--magit-location))
  "Diff buffers to review, as (MODE . PROPERTIES).
A buffer uses the PROPERTIES of the first entry whose MODE its major
mode derives from.  They name functions called in that buffer:
 :root      (POS) returns the root of the repository the diff text at
            POS belongs to;
 :hunk      (POS) returns (START . END) of the whole lines of the hunk
            at POS, END without the last newline, or nil off a hunk;
 :location  (POS) returns (FILE LINE) of the diff line at POS: FILE
            absolute, LINE the line in FILE, either nil when unknown."
  :type '(alist :key-type symbol :value-type plist))

(defface agent-review-comment
  '((t :inherit font-lock-constant-face :slant normal))
  "Face for review comments shown under the diff lines they refer to.")

(defvar agent-review--comments nil
  "Pending review comments, oldest first.
Each is a plist: :root of the repository, :file (absolute, or nil for a
change without one), :line in that file, :text of the diff excerpt and
the :comment itself.")

(defvar agent-review--last-session nil
  "Label of the agent session the last review went to.")

(defvar-local agent-review--overlays nil
  "Overlays showing the review comments in this diff buffer.")

(declare-function slot-value "eieio" (obj slot))
(declare-function magit-section-at "magit-section" (&optional position))
(declare-function magit-section-match "magit-section"
                  (condition &optional section))
(declare-function magit-section-parent-value "magit-section" (section))

;;; Backends

(defun agent-review--backend ()
  "Return the properties of the current buffer's backend, or nil."
  (cdr (seq-find (lambda (backend) (derived-mode-p (car backend)))
                 agent-review-backends)))

(defun agent-review--call (property &rest args)
  "Call the current backend's PROPERTY function with ARGS."
  (apply (plist-get (agent-review--backend) property) args))

(defun agent-review--root (&optional pos)
  "Return the repository root of the diff text at POS, or nil.
POS defaults to point; nil also off a diff buffer."
  (when (agent-review--backend)
    (when-let* ((root (agent-review--call :root (or pos (point)))))
      (file-name-as-directory (expand-file-name root)))))

(defun agent-review-lines (beg end)
  "Return (START . END) of the whole lines from BEG to END.
END is the end of the last line, without its newline.  A region ending
at a line start does not cover that line."
  (save-excursion
    (cons (progn (goto-char beg) (line-beginning-position))
          (progn (goto-char end)
                 (when (and (bolp) (> end beg))
                   (backward-char))
                 (line-end-position)))))

(defun agent-review--magit-root (_pos)
  "Return the repository of a magit buffer, its `default-directory'."
  default-directory)

(defun agent-review--multi-magit-root (pos)
  "Return the repository of the multi-magit section at POS.
Off the section of any repository, return `default-directory', which
multi-magit sets to the repository at point."
  (let ((section (magit-section-at pos)))
    (while (and section
                (not (eq (slot-value section 'type) 'multi-magit-status)))
      (setq section (slot-value section 'parent)))
    (if section (slot-value section 'value) default-directory)))

(defun agent-review--magit-section (pos)
  "Return the magit hunk section at POS, or nil."
  (let ((section (magit-section-at pos)))
    (and section (magit-section-match 'hunk section) section)))

(defun agent-review--magit-hunk (pos)
  "Return (START . END) of the magit hunk at POS, or nil."
  (when-let* ((hunk (agent-review--magit-section pos)))
    (agent-review-lines (slot-value hunk 'start) (slot-value hunk 'end))))

(defun agent-review--magit-location (pos)
  "Return (FILE LINE) of the magit diff line at POS.
LINE counts in the new file: removed lines are not counted, and the
header line stands for the line the hunk starts at."
  (when-let* ((hunk (agent-review--magit-section pos)))
    (list (expand-file-name (magit-section-parent-value hunk)
                            (agent-review--root pos))
          (let ((line (car (slot-value hunk 'to-range))))
            (if (or (null line) (slot-value hunk 'combined))
                line
              (save-excursion
                (let ((target (progn (goto-char pos)
                                     (line-beginning-position))))
                  (goto-char (slot-value hunk 'content))
                  (while (< (point) target)
                    (unless (eq (char-after) ?-)
                      (setq line (1+ line)))
                    (forward-line 1))
                  line)))))))

;;; Diff hunks

(defun agent-review--target ()
  "Return the diff text at point that a comment would refer to.
The result is a plist with :root, :file, :line and :text.  The text is
the hunk at point, or the whole lines of the active region when it lies
inside one hunk."
  (let* ((root (or (agent-review--root)
                   (user-error "Not in a diff buffer of agent-review")))
         (region (use-region-p))
         (lines (agent-review-lines (if region (region-beginning) (point))
                                    (if region (region-end) (point))))
         (hunk (or (agent-review--call :hunk (car lines))
                   (user-error "Not on a hunk")))
         (bounds (progn
                   (unless (equal hunk (agent-review--call :hunk (cdr lines)))
                     (user-error "The region spans more than one hunk"))
                   (let ((text (if region lines hunk)))
                     (cons (car text)
                           ;; Leave out blank lines separating hunks.
                           (save-excursion
                             (goto-char (cdr text))
                             (skip-chars-backward "\n" (car text))
                             (point))))))
         (location (agent-review--call :location (car bounds))))
    (list :root root :file (nth 0 location) :line (nth 1 location)
          :text (buffer-substring-no-properties (car bounds) (cdr bounds)))))

;;; Showing comments

(defun agent-review-show-comments ()
  "Show the current diff buffer's review comments under their diff text.
A comment whose text the buffer no longer holds stays queued unseen."
  (mapc #'delete-overlay agent-review--overlays)
  (setq agent-review--overlays nil)
  (when (and agent-review--comments (agent-review--backend))
    (save-excursion
      (save-restriction
        (widen)
        (dolist (comment agent-review--comments)
          (goto-char (point-min))
          (let ((text (plist-get comment :text))
                found)
            (while (and (not found) (search-forward text nil t))
              (let ((start (match-beginning 0)))
                (when (and (equal (agent-review--root start)
                                  (plist-get comment :root))
                           (equal (car (agent-review--call :location start))
                                  (plist-get comment :file)))
                  (setq found (point)))))
            (when found
              (let ((overlay (make-overlay found found nil t)))
                (overlay-put overlay 'after-string
                             (concat "\n" (propertize
                                           (concat "  » "
                                                   (plist-get comment
                                                              :comment))
                                           'face 'agent-review-comment)))
                (push overlay agent-review--overlays)))))))))

(defun agent-review-show-comments-later ()
  "Show the review comments once the current diff buffer is filled in.
For a mode hook of a diff mode that starts before its buffer is ready."
  (let ((buffer (current-buffer)))
    (run-at-time 0 nil
                 (lambda ()
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (agent-review-show-comments)))))))

(defun agent-review--show-everywhere ()
  "Show the review comments again in every diff buffer.
Not only in those of one repository: a multi-magit buffer shows
several."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (agent-review--backend)
        (agent-review-show-comments)))))

(add-hook 'magit-refresh-buffer-hook #'agent-review-show-comments)

;;; Commands

(defun agent-review--count (root)
  "Return the number of review comments queued for ROOT."
  (seq-count (lambda (comment) (equal (plist-get comment :root) root))
             agent-review--comments))

;;;###autoload
(defun agent-review-comment ()
  "Comment the diff hunk at point, or the lines of the region inside it.
On text that already has a comment, edit that comment; an empty comment
removes it.  \\[agent-review-send] sends the comments."
  (interactive)
  (let* ((target (agent-review--target))
         (old (seq-find (lambda (comment)
                          (cl-every (lambda (key)
                                      (equal (plist-get comment key)
                                             (plist-get target key)))
                                    '(:root :file :text)))
                        agent-review--comments))
         (comment (string-trim
                   (read-string (if old
                                    "Edit comment (empty removes it): "
                                  "Comment: ")
                                (plist-get old :comment)))))
    (cond ((and old (string-empty-p comment))
           (setq agent-review--comments (delq old agent-review--comments)))
          (old (plist-put old :comment comment))
          ((not (string-empty-p comment))
           (setq agent-review--comments
                 (append agent-review--comments
                         (list (append target (list :comment comment)))))))
    (deactivate-mark)
    (agent-review--show-everywhere)
    (let ((count (agent-review--count (plist-get target :root))))
      (message "%d review comment%s queued%s"
               count (if (= count 1) "" "s")
               (if (zerop count)
                   ""
                 (substitute-command-keys
                  ", \\[agent-review-send] sends them"))))))

(defun agent-review--fence (text)
  "Return a Markdown code fence longer than any backtick run in TEXT."
  (let ((longest 0) (start 0))
    (while (string-match "`+" text start)
      (setq longest (max longest (- (match-end 0) (match-beginning 0)))
            start (match-end 0)))
    (make-string (max 3 (1+ longest)) ?`)))

(defun agent-review--prompt (comments root)
  "Return the prompt carrying review COMMENTS on the repository at ROOT."
  (concat
   "Review comments on your changes. Address each one; its diff excerpt "
   "shows the code it refers to.\n"
   (let ((index 0))
     (mapconcat
      (lambda (comment)
        (let ((file (plist-get comment :file))
              (line (plist-get comment :line))
              (text (plist-get comment :text)))
          (setq index (1+ index))
          (format "\n%d. %s%s\n%sdiff\n%s\n%s\nComment: %s\n"
                  index
                  (if file (file-relative-name file root) "Change")
                  (if (and file line (> line 0)) (format ", line %d" line) "")
                  (agent-review--fence text) text
                  (agent-review--fence text)
                  (plist-get comment :comment))))
      comments ""))))

(defun agent-review--sessions (directory)
  "Return the agent sessions run for DIRECTORY itself.
Each is (LABEL SEND SESSION), LABEL naming the agent and the session."
  (mapcan (lambda (agent)
            (when (fboundp (nth 1 agent))
              (mapcar (lambda (session)
                        (let ((label (if (bufferp session)
                                         (buffer-name session)
                                       (car session))))
                          (list (format "%s: %s" (car agent) label)
                                (nth 2 agent)
                                (if (bufferp session) session (cdr session)))))
                      (funcall (nth 1 agent) directory))))
          agent-review-agents))

(defun agent-review--nearest-sessions (root)
  "Return (DIRECTORY . SESSIONS) of the agents nearest to ROOT, or nil.
DIRECTORY is ROOT when an agent session runs for it, else the closest
directory above ROOT that has one; SESSIONS are those of DIRECTORY, as
`agent-review--sessions' returns them."
  (let ((directory root) found)
    (while (and directory (not found))
      (if-let* ((sessions (agent-review--sessions directory)))
          (setq found (cons directory sessions))
        (let ((parent (file-name-directory (directory-file-name directory))))
          (setq directory (unless (or (null parent) (equal parent directory))
                            parent)))))
    found))

;;;###autoload
(defun agent-review-send (&optional choose)
  "Send this repository's review comments to a coding agent in one prompt.
The agent is the one with a session running for the repository, or
failing that for the closest directory above it; when several run
there, or when CHOOSE is non-nil, ask which one.  Sent comments leave
the queue.  \[agent-review-send-to] always asks."
  (interactive)
  (let* ((root (or (agent-review--root)
                   (user-error "Not in a diff buffer of agent-review")))
         (comments (seq-filter (lambda (comment)
                                 (equal (plist-get comment :root) root))
                               agent-review--comments))
         (nearest (progn
                    (unless comments
                      (user-error "No review comments for %s"
                                  (abbreviate-file-name root)))
                    (or (agent-review--nearest-sessions root)
                        (user-error "No %s session runs for %s or above"
                                    (mapconcat #'car agent-review-agents
                                               " or ")
                                    (abbreviate-file-name root)))))
         (directory (car nearest))
         (sessions (cdr nearest))
         (session (if (and (null (cdr sessions)) (not choose))
                      (car sessions)
                    (assoc (completing-read
                            (format "Send review to (%s): "
                                    (abbreviate-file-name directory))
                            (mapcar #'car sessions) nil t nil nil
                            (car (assoc agent-review--last-session
                                        sessions)))
                           sessions))))
    ;; File names relative to where the agent runs, which may be above
    ;; the repository.
    (funcall (nth 1 session) (nth 2 session)
             (agent-review--prompt comments directory))
    (setq agent-review--last-session (car session)
          agent-review--comments (seq-remove (lambda (comment)
                                               (memq comment comments))
                                             agent-review--comments))
    (agent-review--show-everywhere)
    (message "Sent %d review comment%s to %s"
             (length comments) (if (cdr comments) "s" "") (car session))))

;;;###autoload
(defun agent-review-send-to ()
  "Send this repository's review comments to an agent session asked for.
As `agent-review-send', but ask which session even when one runs."
  (interactive)
  (agent-review-send t))

;;;###autoload
(defun agent-review-discard ()
  "Discard the review comments of this repository, or all of them elsewhere."
  (interactive)
  (let* ((root (agent-review--root))
         (comments (if root
                       (seq-filter (lambda (comment)
                                     (equal (plist-get comment :root) root))
                                   agent-review--comments)
                     agent-review--comments)))
    (unless comments
      (user-error "No review comments to discard"))
    (when (y-or-n-p (format "Discard %d review comment%s? "
                            (length comments) (if (cdr comments) "s" "")))
      (setq agent-review--comments (seq-remove (lambda (comment)
                                                 (memq comment comments))
                                               agent-review--comments))
      (agent-review--show-everywhere))))

(provide 'agent-review)

;;; agent-review.el ends here
