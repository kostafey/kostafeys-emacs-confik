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

(defmacro magit-git2-test--in-refresh (&rest body)
  "Run BODY the way a section runs in a buffer refresh."
  (declare (indent 0))
  `(let ((magit--refreshing-buffer-p t)
         (magit--refresh-start-time (current-time))
         (magit--refresh-cache (list (cons 0 0))))
     ,@body))

(ert-deftest magit-git2-test-refresh-memo ()
  "What a buffer refresh has read lasts for that refresh alone."
  :tags '(git2)
  (let* ((dirs (magit-git2-test--dirs))
         (main (cdr (assoc "main" dirs)))
         (sub (cdr (assoc "main/sub" dirs)))
         (ref "refs/heads/memo-test")
         (args (list "rev-parse" "--verify" "--quiet" ref))
         (default-directory main)
         (exit (lambda ()
                 (prog1 (car (magit-git2--dispatch args))
                   (magit-git2--release))))
         (cdup (lambda (dir)
                 (let ((default-directory dir))
                   (cadr (magit-git2--dispatch
                          '("rev-parse" "--show-cdup")))))))
    (unwind-protect
        (progn
          (magit-git2-test--in-refresh
            (should (equal (funcall exit) 1))
            (magit-git2-fixture--git main "update-ref" ref "HEAD")
            (should (equal (funcall exit) 1))
            ;; The memos are per directory.
            (should (equal (funcall cdup main) "\n"))
            (should (equal (funcall cdup sub) "../\n")))
          (magit-git2-test--in-refresh
            (should (equal (funcall exit) 0)))
          ;; `magit-run-git' binds the cache around a command that writes.
          (let ((magit--refresh-cache (list (cons 0 0))))
            (should (equal (funcall exit) 0))
            (magit-git2-fixture--git main "update-ref" "-d" ref)
            (should (equal (funcall exit) 1))))
      (magit-git2-fixture--git main "update-ref" "-d" ref))))

(defun magit-git2-test--clean-repo ()
  "A new repository with one committed file and no changes."
  (magit-git2-test--dirs)               ; the global config names the author
  (let ((dir (file-name-as-directory
              (make-temp-file (expand-file-name
                               "clean-" (magit-git2-fixture-root))
                              t))))
    (magit-git2-fixture--git dir "init" "-q")
    (magit-git2-fixture--write (expand-file-name "file" dir) "text\n")
    (magit-git2-fixture--git dir "add" "file")
    (magit-git2-fixture--commit dir "first")
    dir))

(ert-deftest magit-git2-test-update-index ()
  "A status refresh does without `update-index --refresh' where it can."
  :tags '(git2)
  (let* ((default-directory (magit-git2-test--clean-repo))
         (dir default-directory)
         (magit-git2-mode 'on)
         ;; Started ahead, they would refresh the index themselves.
         (magit-git2--ahead-commands nil)
         ;; git compares whole seconds of the modification time.
         (shift 0)
         (touch (lambda ()
                  (setq shift (+ shift 10))
                  (set-file-times "file" (time-add (current-time) shift))))
         (stale-p (lambda ()
                    (/= 0 (call-process "git" nil nil nil
                                        "diff-files" "--quiet"))))
         (run (lambda ()
                (magit-process-git nil '("update-index" "--refresh"))))
         (run-in-refresh (lambda ()
                           (let ((magit--refreshing-buffer-p t))
                             (funcall run)))))
    ;; Outside a refresh, git runs.
    (funcall touch)
    (should (funcall stale-p))
    (funcall run)
    (should-not (funcall stale-p))
    ;; In one it does not, and Magit gets a success.
    (funcall touch)
    (should (equal (funcall run-in-refresh) 0))
    (should (funcall stale-p))
    ;; Without a `git diff' that writes the index back, git runs.
    (let ((magit-git-global-arguments
           (cons "--no-optional-locks" magit-git-global-arguments)))
      (funcall run-in-refresh))
    (should-not (funcall stale-p))
    (funcall touch)
    (magit-git2-fixture--git dir "config" "diff.autoRefreshIndex" "false")
    (funcall run-in-refresh)
    (magit-git2-fixture--git dir "config" "--unset" "diff.autoRefreshIndex")
    (should-not (funcall stale-p))
    ;; Shadow mode leaves all to git.
    (funcall touch)
    (let ((magit-git2-mode 'shadow))
      (funcall run-in-refresh))
    (should-not (funcall stale-p))
    ;; A real refresh shows nothing false and leaves the index fresh.
    (funcall touch)
    (with-current-buffer (magit-status-setup-buffer dir)
      (should-not (magit-get-section '((unstaged) (status))))
      (should-not (funcall stale-p)))))

(defun magit-git2-test--ahead-count ()
  "How many calls git commands started ahead have answered."
  (cl-loop for (_ . counters) in (alist-get 'commands (magit-git2-stats-alist))
           sum (alist-get 'ahead counters)))

(defun magit-git2-test--leftovers ()
  "The processes and buffers of commands started ahead that remain."
  (append (cl-remove-if-not
           (lambda (p) (string-prefix-p "magit-git2" (process-name p)))
           (process-list))
          (cl-remove-if-not
           (lambda (b) (string-prefix-p " *magit-git2 " (buffer-name b)))
           (buffer-list))))

(defun magit-git2-test--settle ()
  "Wait until the commands started ahead have been freed."
  (with-timeout (10 (ert-fail "commands started ahead were not freed"))
    (while (magit-git2-test--leftovers)
      (accept-process-output nil 0.01))))

(ert-deftest magit-git2-test-ahead ()
  "Commands started ahead give what they give one after the other."
  :tags '(git2)
  (let* ((dir (magit-git2-test--clean-repo))
         (default-directory dir)
         (refresh (lambda ()
                    (with-current-buffer (magit-status-setup-buffer dir)
                      (magit-refresh)
                      (buffer-substring-no-properties
                       (point-min) (point-max)))))
         (coding-system-for-write 'latin-1-unix))
    ;; Staged, unstaged, not valid UTF-8, untracked.
    (write-region "staged\n" nil "staged" nil 'silent)
    (magit-git2-fixture--git dir "add" "staged")
    (write-region "caf\xe9\n" nil "file" nil 'silent)
    (write-region "new\n" nil "untracked" nil 'silent)
    (let* ((serial (let ((magit-git2-mode 'off)) (funcall refresh)))
           (magit-git2-mode 'on)
           (before (magit-git2-test--ahead-count))
           ;; It redisplays, showing the half built buffer.
           (deleted nil)
           (note (lambda (&rest _)
                   (when magit--refreshing-buffer-p
                     (setq deleted t))))
           (ahead (unwind-protect
                      (progn
                        (advice-add 'delete-process :before note)
                        (funcall refresh))
                    (advice-remove 'delete-process note))))
      (should (> (magit-git2-test--ahead-count) before))
      (should (equal ahead serial))
      (should-not deleted)
      (magit-git2-test--settle))))

(ert-deftest magit-git2-test-ahead-output ()
  "A command started ahead keeps its exit code, stdout and stderr apart."
  :tags '(git2)
  (let ((default-directory (magit-git2-test--clean-repo))
        (finish (lambda (&rest args)
                  (magit-git2--finish-process
                   (magit-git2--start-process
                    (cons magit-git-global-arguments args))))))
    (should (equal (funcall finish "rev-parse" "--is-bare-repository")
                   '(0 "false\n" "")))
    (should (equal (funcall finish "rev-parse" "--verify" "no-such-ref")
                   '(128 "" "fatal: Needed a single revision\n")))
    ;; Long over by the time Magit asks.
    (let ((process (magit-git2--start-process
                    (cons magit-git-global-arguments
                          '("rev-parse" "--is-bare-repository")))))
      (sleep-for 0.2)
      (should (equal (magit-git2--finish-process process)
                     '(0 "false\n" ""))))
    (magit-git2-test--settle)))

(ert-deftest magit-git2-test-ahead-unused ()
  "A command nothing asks for ends by itself and is not started again."
  :tags '(git2)
  (let* ((dir (magit-git2-test--clean-repo))
         (default-directory dir)
         (magit-git2-mode 'on)
         (command (cons magit-git-global-arguments '("diff" "--stat")))
         (magit-git2--ahead-commands (list command))
         (magit-git2--ahead-processes nil)
         (magit--refreshing-buffer-p t)
         (magit--refresh-start-time (current-time)))
    (should (equal (magit-process-git nil '("update-index" "--refresh")) 0))
    (should (= (length magit-git2--ahead-processes) 1))
    ;; Its output belongs to this refresh and no other.
    (let ((magit--refresh-start-time (current-time)))
      (should-not (magit-git2--take-ahead '("diff" "--stat"))))
    (magit-git2--drop-ahead)
    (should-not magit-git2--ahead-processes)
    (should-not magit-git2--ahead-commands)
    (magit-git2-test--settle)
    (should-not (file-exists-p ".git/index.lock"))))

(ert-deftest magit-git2-test-lisp-error ()
  "A Lisp error turns the emulation off and git answers."
  :tags '(git2)
  (let* ((default-directory (magit-git2-test--clean-repo))
         (check
          (lambda (mode function args)
            (let* ((magit-git2-mode mode)
                   (magit-git2--auto-disables 0)
                   (magit--refreshing-buffer-p t)
                   (magit--refresh-start-time (current-time))
                   (magit-git2--status-refresh magit--refresh-start-time)
                   (theirs (magit-git2-test--git args))
                   (mine (cl-letf (((symbol-function function)
                                    (lambda (&rest _) (error "Failure"))))
                           (with-temp-buffer
                             (let ((exit (magit-process-git '(t nil) args)))
                               (list exit (buffer-string)))))))
              (should (equal mine (list (car theirs) (cadr theirs))))
              (should (eq magit-git2-mode 'off))
              (should (= magit-git2--auto-disables 1))))))
    (dolist (function '(magit-git2--usable-p magit-git2--redundant-p
                        magit-git2--emulate))
      (funcall check 'on function '("rev-parse" "--show-cdup")))
    (funcall check 'on 'magit-git2--deliver '("rev-parse" "--show-cdup"))
    (funcall check 'on 'magit-git2--take-ahead '("status" "--porcelain"))
    (funcall check 'shadow 'magit-git2--usable-p '("rev-parse" "--show-cdup"))
    (funcall check 'shadow 'magit-git2--count '("rev-parse" "--show-cdup"))))

(provide 'magit-git2-test)

;;; magit-git2-test.el ends here
