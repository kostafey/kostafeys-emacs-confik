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
;; running agent, asking which one when several run.
;;
;; Diff buffers come from `agent-review-backends', which knows magit;
;; agents from `agent-review-agents', which knows `codex-ide' and
;; `pi-ide'.  Both lists take more entries.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defgroup agent-review nil
  "Review diffs for coding agents."
  :group 'tools :prefix "agent-review-")

(defcustom agent-review-agents
  '(("Codex" codex-ide-session-buffer codex-ide-send-string)
    ("Pi" pi-ide-session-buffer pi-ide-send-string))
  "Agents that review comments go to, as (NAME RUNNING-P SEND).
RUNNING-P is called in the diff buffer and returns non-nil when its
project has a live session of the agent; SEND submits a prompt string
to that session.  Agents whose RUNNING-P is not defined are skipped."
  :type '(repeat (list string function function)))

(defcustom agent-review-backends
  '((magit-mode :root agent-review--magit-root
                :hunk agent-review--magit-hunk
                :location agent-review--magit-location))
  "Diff buffers to review, as (MODE . PROPERTIES).
A buffer whose major mode derives from MODE uses the PROPERTIES, which
name functions called in that buffer:
 :root      () returns the repository root;
 :hunk      (POS) returns (START . END) of the whole lines of the hunk
            at POS, END without the last newline, or nil off a hunk;
 :location  (POS) returns (FILE LINE) of the diff line at POS: FILE
            absolute, LINE the line in FILE, either nil when unknown."
  :type '(alist :key-type symbol :value-type plist))

(defface agent-review-comment
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for review comments shown under the diff lines they refer to.")

(defvar agent-review--comments nil
  "Pending review comments, oldest first.
Each is a plist: :root of the repository, :file (absolute, or nil for a
change without one), :line in that file, :text of the diff excerpt and
the :comment itself.")

(defvar agent-review--last-agent nil
  "Name of the agent the last review went to.")

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

(defun agent-review--root ()
  "Return the repository root of the current diff buffer, or nil."
  (when (agent-review--backend)
    (when-let* ((root (agent-review--call :root)))
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

(defun agent-review--magit-root ()
  "Return the repository of a magit buffer, its `default-directory'."
  default-directory)

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
                            default-directory)
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
  (when-let* ((root (and agent-review--comments (agent-review--root))))
    (save-excursion
      (save-restriction
        (widen)
        (dolist (comment agent-review--comments)
          (when (equal (plist-get comment :root) root)
            (goto-char (point-min))
            (let ((text (plist-get comment :text))
                  found)
              (while (and (not found) (search-forward text nil t))
                (when (equal (car (agent-review--call
                                   :location (match-beginning 0)))
                             (plist-get comment :file))
                  (setq found (point))))
              (when found
                (let ((overlay (make-overlay found found nil t)))
                  (overlay-put overlay 'after-string
                               (concat "\n" (propertize
                                             (concat "  » "
                                                     (plist-get comment
                                                                :comment))
                                             'face 'agent-review-comment)))
                  (push overlay agent-review--overlays))))))))))

(defun agent-review-show-comments-later ()
  "Show the review comments once the current diff buffer is filled in.
For a mode hook of a diff mode that starts before its buffer is ready."
  (let ((buffer (current-buffer)))
    (run-at-time 0 nil
                 (lambda ()
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (agent-review-show-comments)))))))

(defun agent-review--show-everywhere (root)
  "Show the review comments again in every diff buffer of ROOT."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (agent-review--backend)
                 (or agent-review--overlays
                     (equal (agent-review--root) root)))
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
    (agent-review--show-everywhere (plist-get target :root))
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

;;;###autoload
(defun agent-review-send (&optional choose)
  "Send this repository's review comments to a coding agent in one prompt.
The agent is the one with a session running for the repository; when
several run, or with a prefix argument CHOOSE, ask which one.  Sent
comments leave the queue."
  (interactive "P")
  (let* ((root (or (agent-review--root)
                   (user-error "Not in a diff buffer of agent-review")))
         (comments (seq-filter (lambda (comment)
                                 (equal (plist-get comment :root) root))
                               agent-review--comments))
         (running (progn
                    (unless comments
                      (user-error "No review comments for %s"
                                  (abbreviate-file-name root)))
                    (seq-filter (lambda (agent)
                                  (and (fboundp (nth 1 agent))
                                       (funcall (nth 1 agent))))
                                agent-review-agents)))
         (agent (cond ((null running)
                       (user-error "No %s session runs for %s"
                                   (mapconcat #'car agent-review-agents
                                              " or ")
                                   (abbreviate-file-name root)))
                      ((and (null (cdr running)) (not choose))
                       (car running))
                      (t
                       (assoc (completing-read
                               "Send review to: " (mapcar #'car running)
                               nil t nil nil
                               (car (assoc agent-review--last-agent
                                           running)))
                              running)))))
    (funcall (nth 2 agent) (agent-review--prompt comments root))
    (setq agent-review--last-agent (car agent)
          agent-review--comments (seq-remove (lambda (comment)
                                               (memq comment comments))
                                             agent-review--comments))
    (agent-review--show-everywhere root)
    (message "Sent %d review comment%s to %s"
             (length comments) (if (cdr comments) "s" "") (car agent))))

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
      (dolist (root (delete-dups (mapcar (lambda (comment)
                                           (plist-get comment :root))
                                         comments)))
        (agent-review--show-everywhere root)))))

(provide 'agent-review)

;;; agent-review.el ends here
