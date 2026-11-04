;;; magit-git2-test.el --- Differential tests of magit-git2  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Kostafey <kostafey@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Run with test/run-tests.sh.  For every registered shape, every
;; example argument list of it, and every repository state -- the
;; fixture states of test/fixture.el plus the repositories listed in
;; MAGIT_GIT2_TEST_REPOS -- the emulation's (EXIT STDOUT STDERR) must
;; equal real git's.  A shape that refuses falls through to git, which
;; is always correct; the registry test makes sure each shape answers
;; at least once.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit)
(require 'magit-git2)
(require 'fixture)

(declare-function magit-git2--release "magit-git2-core")

;; Without the module every call falls through to git, and the tests
;; would compare git with itself.
(unless (magit-git2-core-available-p)
  (error "magit-git2: %s" magit-git2-core-error))

(setq ert-batch-print-length 3
      ert-batch-print-level 3
      ert-batch-backtrace-right-margin 60)

(defvar magit-git2-test-report
  (or (getenv "MAGIT_GIT2_TEST_REPORT")
      (expand-file-name "mismatches.txt" (magit-git2-fixture-root)))
  "File the mismatches are written to.")

(defun magit-git2-test--check (what reports)
  "Fail when REPORTS (strings) is not empty, writing them to a file.
WHAT names the test in the file."
  (when reports
    (with-temp-buffer
      (insert "## " what "\n\n" (string-join reports "\n\n") "\n\n")
      (write-region nil nil magit-git2-test-report t 'silent))
    (ert-fail (format "%s: %d mismatches, see %s"
                      what (length reports) magit-git2-test-report))))

(defvar magit-git2-test--dirs nil
  "The repository states: ((NAME . DIRECTORY) ...).")

(defun magit-git2-test--dirs ()
  "The fixture states (built on first use) and the extra repositories."
  (or magit-git2-test--dirs
      (setq magit-git2-test--dirs
            (append (if (file-exists-p (expand-file-name
                                        "main" (magit-git2-fixture-root)))
                        (magit-git2-fixture-dirs)
                      (magit-git2-fixture-build))
                    (mapcar (lambda (dir)
                              (cons (abbreviate-file-name dir)
                                    (file-name-as-directory dir)))
                            (split-string
                             (or (getenv "MAGIT_GIT2_TEST_REPOS") "")
                             path-separator t))))))

(defun magit-git2-test--git (args)
  "Run git with local ARGS the way `magit-process-git' does."
  (let ((log (make-temp-file "magit-git2-test-stderr"))
        (magit-git2-mode 'off))
    (unwind-protect
        (with-temp-buffer
          (let* ((exit (magit-process-git (list t log) args))
                 (stdout (buffer-string)))
            (list exit stdout
                  (with-temp-buffer
                    (let ((coding-system-for-read 'utf-8-unix))
                      (insert-file-contents log))
                    (buffer-string)))))
      (delete-file log))))

(defun magit-git2-test--compare (args)
  "Compare the emulation with git for ARGS in `default-directory'.
Return nil when they agree or the emulation refused, else a report."
  (let ((mine (condition-case nil
                  (magit-git2--dispatch args)
                ;; In use such a call goes to git, as on a refusal.
                (magit-git2-error nil)))
        (shape magit-git2--shape))
    (magit-git2--release)
    (when mine
      (let* ((theirs (magit-git2-test--git args))
             (mine* (if (eq (nth 2 mine) 'unknown)
                        (list (nth 0 mine) (nth 1 mine) (nth 2 theirs))
                      mine)))
        (unless (equal mine* theirs)
          (format "%s $ git %s\n  shape:    %s\n  git:      %S\n  emulated: %S"
                  (abbreviate-file-name default-directory)
                  (mapconcat #'shell-quote-argument args " ")
                  shape theirs mine))))))

(defvar magit-git2-test--tally nil
  "Per shape: (NAME ANSWERED REFUSED), from the last run.")

(defun magit-git2-test--run (shapes)
  "Compare SHAPES (names) in every state.
Return (MISMATCHES . ANSWERED), ANSWERED the shapes that answered."
  (setq magit-git2-test--tally (mapcar (lambda (n) (list n 0 0)) shapes))
  (let (mismatches answered)
    (pcase-dolist (`(,state . ,dir) (magit-git2-test--dirs))
      (let ((default-directory dir))
        (dolist (name shapes)
          (pcase-let ((`(,_ ,_ ,examples) (alist-get name magit-git2--shapes)))
            (dolist (args (funcall examples state))
              (let ((report (magit-git2-test--compare args))
                    (tally (assq name magit-git2-test--tally)))
                (if magit-git2--shape (cl-incf (nth 1 tally))
                  (cl-incf (nth 2 tally)))
                (when (and magit-git2--shape (not report))
                  (cl-pushnew magit-git2--shape answered))
                (when report (push report mismatches))))))))
    (cons (nreverse mismatches) answered)))

(ert-deftest magit-git2-test-differential ()
  "The emulation agrees with git for every example of every shape."
  :tags '(git2)
  (let ((result (magit-git2-test--run (mapcar #'car magit-git2--shapes))))
    (message "%-32s %8s %8s" "shape" "answered" "refused")
    (pcase-dolist (`(,name ,yes ,no) magit-git2-test--tally)
      (message "%-32s %8d %8d" name yes no))
    (magit-git2-test--check "differential" (car result))))

(ert-deftest magit-git2-test-every-shape-answers ()
  "Every registered shape has examples and answers some of them."
  :tags '(git2)
  (let* ((names (mapcar #'car magit-git2--shapes))
         (answered (cdr (magit-git2-test--run names))))
    (magit-git2-test--check
     "every-shape-answers"
     (mapcar (lambda (n) (format "%s never answered" n))
             (cl-remove-if (lambda (n) (memq n answered)) names)))))

(ert-deftest magit-git2-test-delivery ()
  "Magit's own helpers return the same with the emulation on and off."
  :tags '(git2)
  (let (reports)
   (pcase-dolist (`(,_ . ,dir)
                  (append (magit-git2-test--dirs)
                          ;; Magit passes abbreviated directories too;
                          ;; HOME is the fixture's home, next to main.
                          (list (cons "~" "~/../main/"))))
    (let ((default-directory dir))
      (dolist (call '((magit-git-string "rev-parse" "--show-toplevel")
                      (magit-git-string "rev-parse" "--verify" "HEAD")
                      (magit-git-str "symbolic-ref" "--short" "HEAD")
                      (magit-git-lines "log" "--format=%h %s" "-n5" "--")
                      (magit-git-items "config" "--list" "-z")
                      (magit-git-success "rev-parse" "@{upstream}")
                      (magit-git-exit-code "merge-base" "--is-ancestor"
                                           "HEAD" "HEAD")
                      (magit-git-config-p "magitgit2.global")
                      (magit-git-string-ng "rev-parse" "--git-dir")))
        (let* ((magit--refresh-cache nil)
               (off (let ((magit-git2-mode 'off)) (eval call t)))
               (on (let ((magit-git2-mode 'on)) (eval call t))))
          (unless (equal on off)
            (push (format "%s %S\n  off: %S\n  on:  %S" dir call off on)
                  reports))))))
   (magit-git2-test--check "delivery" (nreverse reports))))

(ert-deftest magit-git2-test-status-shadow ()
  "Refreshing a status buffer in shadow mode records no mismatch."
  :tags '(git2)
  (when-let* ((buffer (get-buffer magit-git2-mismatches-buffer)))
    (kill-buffer buffer))
  (let ((magit-git2-mode 'shadow))
    (pcase-dolist (`(,state . ,dir) (magit-git2-test--dirs))
      (unless (member state '("main/sub" "origin.git"))
        (let ((default-directory dir))
          (magit-status-setup-buffer dir)
          (magit-refresh)))))
  (magit-git2-test--check
   "status-shadow"
   (when-let* ((b (get-buffer magit-git2-mismatches-buffer)))
     (with-current-buffer b (split-string (buffer-string) "^\\* " t)))))

(provide 'magit-git2-test)

;;; magit-git2-test.el ends here
