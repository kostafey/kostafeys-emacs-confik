;;; magit-git2-measure.el --- Count the git calls of a Magit refresh  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Kostafey <kostafey@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Measures what a Magit status refresh costs in git subprocesses, and
;; writes the result to measure/report-<host>-<date>-<setup>.org next
;; to this file, <setup> being `git' when the magit-git2 module is not
;; loaded and `magit-git2-mode' otherwise.  Load it into a running,
;; fully configured Emacs and call it from a buffer inside the
;; repository to measure:
;;
;;   emacs -l .../measure/magit-git2-measure.el
;;   M-x magit-git2-measure
;;
;; or, without a display:
;;
;;   emacs --batch -l <your init> -l .../magit-git2-measure.el \
;;         -f magit-git2-measure-batch REPO-DIRECTORY
;;
;; The run records (`magit-process-record-invocations') the initial
;; `magit-status' and two explicit `magit-refresh' calls, each with
;; `magit-refresh-verbose' on, then times three more refreshes with
;; both off.  Every invocation recorded is a subprocess; the calls that
;; went through `magit-process-git' are counted separately, being the
;; ones `magit-git2' can answer in-process.  When `magit-git2' is
;; loaded the report also states its mode and statistics, so the same
;; script measures before and after.

;;; Code:

(require 'subr-x)
(require 'shell)                        ; `split-string-shell-command'

(declare-function magit-status "magit-status")
(declare-function magit-refresh "magit-mode")
(declare-function magit-version "magit")
(declare-function magit-git-version "magit-git")
(declare-function magit-toplevel "magit-git")
(declare-function magit-git2-stats-alist "magit-git2")
(declare-function magit-git2-core-available-p "magit-git2-core")
(defvar magit-process-record-invocations)
(defvar magit-process-record-buffer-name)
(defvar magit-process-record-entry-format)
(defvar magit-refresh-verbose)
(defvar magit-git-executable)
(defvar magit-git-global-arguments)
(defvar magit-git2-mode)

(defconst magit-git2-measure-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory the reports are written to.")

(defconst magit-git2-measure--separator "\x1e"
  "Separator put in front of every recorded invocation.
Arguments may contain newlines, so the record is split on this
instead of on lines.")

(defvar magit-git2-measure--funnel-calls nil
  "Calls of `magit-process-git' seen during the current phase.
Each element is (DESTINATION . LOCAL-ARGS), newest first.")

(defun magit-git2-measure--capture (orig destination &rest args)
  "Record DESTINATION and ARGS, then call ORIG with them."
  (push (cons destination (flatten-tree args)) magit-git2-measure--funnel-calls)
  (apply orig destination args))

(defun magit-git2-measure--parse-record (text)
  "Parse the invocation record TEXT into a list of argument lists.
A git invocation yields its local arguments only (Magit replaces
the global ones with an ellipsis); any other program yields the
program name followed by its arguments."
  (let (result)
    (dolist (entry (split-string text magit-git2-measure--separator t))
      (when (string-match "\\` *\\(.*?\\) \\$ \\([^ ]+\\) ?" entry)
        (let* ((program (match-string 2 entry))
               (rest (string-trim-right (substring entry (match-end 0))))
               (global (and (string-match "\\`\\(…\\|\\.\\.\\.\\) ?" rest)
                            (match-string 1 rest))))
          (push (if global
                    (split-string-shell-command
                     (substring rest (match-end 0)))
                  (cons program (split-string-shell-command rest)))
                result))))
    (nreverse result)))

(defun magit-git2-measure--command (args)
  "Return the git subcommand in ARGS.
Leading options and \"-c KEY=VALUE\" pairs are skipped: callers
such as `magit-git-string-ng' pass the global arguments again."
  (while (and args (string-prefix-p "-" (car args)))
    (setq args (if (equal (car args) "-c") (cddr args) (cdr args))))
  (or (car args) "(none)"))

(defun magit-git2-measure--histogram (items)
  "Return ((COUNT . ITEM) ...) for ITEMS, most frequent first."
  (let ((table (make-hash-table :test #'equal))
        result)
    (dolist (item items)
      (puthash item (1+ (gethash item table 0)) table))
    (maphash (lambda (k v) (push (cons v k) result)) table)
    (sort result (lambda (a b)
                   (or (> (car a) (car b))
                       (and (= (car a) (car b))
                            (string< (format "%S" (cdr a))
                                     (format "%S" (cdr b)))))))))

(defun magit-git2-measure--argv-string (args)
  "Return ARGS as one shell-quoted string."
  (mapconcat #'shell-quote-argument args " "))

(defun magit-git2-measure--messages-since (start)
  "Return the text of *Messages* from position START on."
  (with-current-buffer (messages-buffer)
    (buffer-substring-no-properties (min start (point-max)) (point-max))))

(defun magit-git2-measure--run-phase (name thunk)
  "Run THUNK with invocation recording and verbose refresh on.
Return a plist describing phase NAME."
  (let ((record (get-buffer-create magit-process-record-buffer-name))
        (messages-start (with-current-buffer (messages-buffer) (point-max)))
        (start (current-time)))
    (with-current-buffer record
      (let ((inhibit-read-only t)) (erase-buffer)))
    (setq magit-git2-measure--funnel-calls nil)
    (let ((magit-process-record-invocations t)
          (magit-process-record-entry-format
           (concat magit-git2-measure--separator "%T %%d $ %%a"))
          (magit-refresh-verbose t))
      (funcall thunk))
    (list :name name
          :seconds (float-time (time-since start))
          :recorded (magit-git2-measure--parse-record
                     (with-current-buffer record
                       (buffer-substring-no-properties
                        (point-min) (point-max))))
          :funnel (reverse magit-git2-measure--funnel-calls)
          :verbose (magit-git2-measure--messages-since messages-start))))

(defun magit-git2-measure--environment ()
  "Return the environment facts as ((LABEL . VALUE) ...)."
  `(("host" . ,(system-name))
    ("emacs-version" . ,emacs-version)
    ("system-type" . ,system-type)
    ;; There is no `dynamic-modules' feature: an Emacs built with
    ;; module support has `module-load' and a `module-file-suffix'.
    ("dynamic modules" . ,(and (fboundp 'module-load)
                               module-file-suffix
                               t))
    ("module-file-suffix" . ,module-file-suffix)
    ("magit-version" . ,(ignore-errors (magit-version)))
    ("magit-git-executable" . ,magit-git-executable)
    ("magit-git-version" . ,(ignore-errors (magit-git-version)))
    ("magit-git-global-arguments"
     . ,(magit-git2-measure--argv-string magit-git-global-arguments))
    ("magit-git2-mode" . ,(if (featurep 'magit-git2)
                              magit-git2-mode
                            "not loaded"))))

(defun magit-git2-measure--insert-histogram (title items)
  "Insert an org section TITLE with the histogram of ITEMS."
  (insert (format "*** %s\n\n#+begin_example\n" title))
  (dolist (entry (magit-git2-measure--histogram items))
    (insert (format "%5d  %s\n" (car entry) (cdr entry))))
  (insert "#+end_example\n\n"))

(defun magit-git2-measure--insert-phase (phase)
  "Insert the report section for PHASE."
  (let ((recorded (plist-get phase :recorded))
        (funnel (plist-get phase :funnel)))
    (insert (format "** %s\n\n" (plist-get phase :name)))
    (insert (format "- wall time: %.3f s\n" (plist-get phase :seconds)))
    (insert (format "- subprocesses recorded: %d\n" (length recorded)))
    (insert (format "- calls through =magit-process-git=: %d\n\n"
                    (length funnel)))
    (magit-git2-measure--insert-histogram
     "Recorded invocations per command"
     (mapcar #'magit-git2-measure--command recorded))
    (magit-git2-measure--insert-histogram
     "Recorded invocations (local arguments)"
     (mapcar #'magit-git2-measure--argv-string recorded))
    (magit-git2-measure--insert-histogram
     "Calls through magit-process-git (destination, local arguments)"
     (mapcar (lambda (call)
               (format "%-10S %s" (car call)
                       (magit-git2-measure--argv-string (cdr call))))
             funnel))
    (insert "*** Verbose refresh\n\n#+begin_example\n"
            (plist-get phase :verbose)
            "#+end_example\n\n")))

(defun magit-git2-measure--setup ()
  "Name what answered Magit: \"git\" without the module, else the mode."
  (if (and (featurep 'magit-git2) (magit-git2-core-available-p))
      (symbol-name magit-git2-mode)
    "git"))

(defun magit-git2-measure--write-report (directory environment phases bench)
  "Write the report for DIRECTORY and return its file name.
ENVIRONMENT is from `magit-git2-measure--environment', PHASES from
`magit-git2-measure--run-phase' and BENCH from `benchmark-run'."
  (let ((file (expand-file-name
               (format "report-%s-%s-%s.org"
                       (car (split-string (system-name) "\\."))
                       (format-time-string "%Y-%m-%d")
                       (magit-git2-measure--setup))
               magit-git2-measure-directory)))
    (with-temp-file file
      (insert (format "#+title: Magit refresh cost: %s\n"
                      (abbreviate-file-name directory)))
      (insert (format "#+date: %s\n\n"
                      (format-time-string "%Y-%m-%d %H:%M")))
      (insert "* Environment\n\n")
      (dolist (entry environment)
        (insert (format "- %s :: %s\n" (car entry) (cdr entry))))
      (insert "\n* Summary\n\n")
      (insert "| phase | wall s | subprocesses | magit-process-git |\n")
      (insert "|-------+--------+--------------+-------------------|\n")
      (dolist (phase phases)
        (insert (format "| %s | %.3f | %d | %d |\n"
                        (plist-get phase :name)
                        (plist-get phase :seconds)
                        (length (plist-get phase :recorded))
                        (length (plist-get phase :funnel)))))
      (insert (format "\n=(benchmark-run 3 (magit-refresh))= => =%S=\n"
                      bench))
      (insert (format "- wall time per refresh: %.3f s\n" (/ (car bench) 3)))
      (insert (format "- garbage collections: %d (%.3f s)\n\n"
                      (nth 1 bench) (nth 2 bench)))
      (when (fboundp 'magit-git2-stats-alist)
        (insert "* magit-git2 statistics (whole session)\n\n#+begin_example\n"
                (pp-to-string (magit-git2-stats-alist))
                "#+end_example\n\n"))
      (insert "* Phases\n\n")
      (mapc #'magit-git2-measure--insert-phase phases))
    file))

;;;###autoload
(defun magit-git2-measure (directory)
  "Measure the git subprocesses of a Magit refresh in DIRECTORY.
Write the report to `magit-git2-measure-directory' and return its
file name."
  (interactive
   (list (or (ignore-errors (require 'magit) (magit-toplevel))
             (read-directory-name "Repository: "))))
  (require 'magit)
  (let* ((default-directory (file-name-as-directory
                             (expand-file-name directory)))
         (default-directory (or (magit-toplevel) default-directory))
         (environment (magit-git2-measure--environment))
         phases bench file)
    (advice-add 'magit-process-git :around #'magit-git2-measure--capture
                '((depth . -100)))
    (unwind-protect
        (let (status-buffer)
          (push (magit-git2-measure--run-phase
                 "magit-status"
                 (lambda ()
                   (magit-status default-directory)
                   (setq status-buffer (current-buffer))))
                phases)
          (dolist (name '("magit-refresh 1" "magit-refresh 2"))
            (push (magit-git2-measure--run-phase
                   name
                   (lambda ()
                     (with-current-buffer status-buffer (magit-refresh))))
                  phases))
          (setq bench (with-current-buffer status-buffer
                        (let ((magit-refresh-verbose nil))
                          (benchmark-run 3 (magit-refresh))))))
      (advice-remove 'magit-process-git #'magit-git2-measure--capture))
    (setq file (magit-git2-measure--write-report
                default-directory environment (nreverse phases) bench))
    (message "magit-git2-measure: %.3f s per refresh, report in %s"
             (/ (car bench) 3) file)
    file))

(defun magit-git2-measure-batch ()
  "Run `magit-git2-measure' on the directory given on the command line."
  (let ((directory (or (pop command-line-args-left) default-directory)))
    (magit-git2-measure directory)))

(provide 'magit-git2-measure)

;;; magit-git2-measure.el ends here
