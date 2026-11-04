;;; fixture.el --- Fixture repositories for the magit-git2 tests  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Kostafey <kostafey@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Builds, with real git, the repositories the differential tests run
;; against.  Everything lives under the directory named by the
;; environment variable MAGIT_GIT2_FIXTURE_ROOT, and HOME and
;; GIT_CONFIG_SYSTEM must point inside it: the builder writes the
;; global and the system config, so it refuses to run against the
;; real ones.  test/run-tests.sh sets all three up.
;;
;; Layout under the root:
;;
;;   home/.gitconfig   global config       etc/gitconfig  system config
;;   origin.git        bare repository
;;   main              branch main, upstream origin/main ahead 2 behind 1,
;;                     tags, two stashes, staged/unstaged/untracked/
;;                     renamed/ignored files, UTF-8 names and messages;
;;                     branch cp1251, a commit message in windows-1251
;;   wt                linked worktree of main on branch feature
;;   detached          clone of main, HEAD detached at tag v1.0
;;   unborn            fresh repository, no commits

;;; Code:

(require 'subr-x)

(defvar magit-git2-fixture--clock 0
  "Minutes since the fixture epoch of the last commit made.")

(defun magit-git2-fixture-root ()
  "Return the fixture root, or signal an error if it is unsafe."
  (let ((root (getenv "MAGIT_GIT2_FIXTURE_ROOT")))
    (unless root
      (error "MAGIT_GIT2_FIXTURE_ROOT is not set; use test/run-tests.sh"))
    (setq root (file-name-as-directory (expand-file-name root)))
    (dolist (var '("HOME" "GIT_CONFIG_SYSTEM"))
      (let ((value (getenv var)))
        (unless (and value
                     (string-prefix-p root (expand-file-name value)))
          (error "%s must point inside %s, not at %s" var root value))))
    root))

(defun magit-git2-fixture--git (dir &rest args)
  "Run git with ARGS in DIR; return its stdout, signal on failure."
  (let* ((default-directory (file-name-as-directory dir))
         (stamp (format-time-string
                 "%Y-%m-%dT%H:%M:%S+0000"
                 (+ 1767225600 (* 60 magit-git2-fixture--clock)) t))
         (process-environment
          (append (list (concat "GIT_AUTHOR_DATE=" stamp)
                        (concat "GIT_COMMITTER_DATE=" stamp))
                  process-environment)))
    (with-temp-buffer
      (let ((exit (apply #'call-process "git" nil t nil args)))
        (unless (zerop exit)
          (error "git %s in %s failed (%s):\n%s"
                 (string-join args " ") dir exit (buffer-string)))
        (string-trim-right (buffer-string))))))

(defun magit-git2-fixture--commit (dir message &rest args)
  "Advance the clock and commit in DIR with MESSAGE and extra ARGS."
  (setq magit-git2-fixture--clock (1+ magit-git2-fixture--clock))
  (apply #'magit-git2-fixture--git dir "commit" "-q" "-m" message args))

(defun magit-git2-fixture--write (file content)
  "Write CONTENT to FILE, creating its directory."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region content nil file nil 'silent)))

(defun magit-git2-fixture--configs (root)
  "Write the global and the system config under ROOT."
  (magit-git2-fixture--write
   (expand-file-name ".gitconfig" (getenv "HOME"))
   (concat "[user]\n\tname = Fixture User\n\temail = fixture@example.com\n"
           "[init]\n\tdefaultBranch = main\n"
           "[advice]\n\tdetachedHead = false\n"
           "[magitgit2]\n\tlevel = global\n\tglobal = true\n"))
  (magit-git2-fixture--write
   (getenv "GIT_CONFIG_SYSTEM")
   "[magitgit2]\n\tlevel = system\n\tsystem = yes\n\tmulti = one\n")
  (ignore root))

(defun magit-git2-fixture--main (root)
  "Build the repository ROOT/main and return its directory."
  (let* ((dir (expand-file-name "main" root))
         (git (lambda (&rest args)
                (apply #'magit-git2-fixture--git dir args)))
         (write (lambda (file content)
                  (magit-git2-fixture--write
                   (expand-file-name file dir) content))))
    (make-directory dir t)
    (funcall git "init" "-q" "-b" "main")
    (funcall git "config" "magitgit2.level" "local")
    (funcall git "config" "--add" "magitgit2.multi" "two")
    (funcall git "config" "--add" "magitgit2.multi" "three")
    (funcall git "config" "include.path" "extra.inc")
    (magit-git2-fixture--write (expand-file-name ".git/extra.inc" dir)
                               "[magitgit2]\n\tincluded = yes\n\tnovalue\n")
    ;; A: the root commit, by an author the mailmap renames.
    (funcall write "README" "Fixture\n")
    (funcall write ".gitignore" "*.log\n")
    (funcall write ".mailmap"
             "Proper Name <proper@example.com> <old@example.com>\n")
    (funcall write "old.txt" "to be renamed\n")
    (funcall write "a.txt" "a\n")
    (funcall write "b.txt" "b\n")
    (funcall write "sub/inner.txt" "inner\n")
    (funcall git "add" ".")
    (magit-git2-fixture--commit dir "Initial commit"
                                "--author=Old Name <old@example.com>")
    (funcall git "tag" "light")
    ;; O1: a commit only upstream has.
    (setq magit-git2-fixture--clock (1+ magit-git2-fixture--clock))
    (let ((o1 (funcall git "commit-tree" "HEAD^{tree}" "-p" "HEAD"
                       "-m" "Upstream only")))
      (funcall git "update-ref" "refs/remotes/origin/main" o1))
    (funcall git "symbolic-ref" "refs/remotes/origin/HEAD"
             "refs/remotes/origin/main")
    (funcall git "config" "remote.origin.url"
             (expand-file-name "origin.git" root))
    (funcall git "config" "remote.origin.fetch"
             "+refs/heads/*:refs/remotes/origin/*")
    ;; A second remote, defined after origin but sorting before it.
    (funcall git "config" "remote.aardvark.url"
             (expand-file-name "origin.git" root))
    (funcall git "config" "branch.main.remote" "origin")
    (funcall git "config" "branch.main.merge" "refs/heads/main")
    ;; M1, tagged v1.0 (annotated), with a UTF-8 file and message.
    (funcall write "файл.txt" "юникод\n")
    (funcall git "add" "файл.txt")
    (magit-git2-fixture--commit dir "Привет: юникод в сообщении")
    (setq magit-git2-fixture--clock (1+ magit-git2-fixture--clock))
    (funcall git "tag" "-a" "v1.0" "-m" "Release 1.0")
    ;; F1 on branch feature, which pushes to origin.
    (setq magit-git2-fixture--clock (1+ magit-git2-fixture--clock))
    (let ((f1 (funcall git "commit-tree" "HEAD^{tree}" "-p" "HEAD"
                       "-m" "Feature work")))
      (funcall git "update-ref" "refs/heads/feature" f1))
    (funcall git "config" "branch.feature.pushRemote" "origin")
    ;; M2: HEAD, a message with a body.
    (funcall write "a.txt" "a\nmore\n")
    (funcall git "add" "a.txt")
    (magit-git2-fixture--commit
     dir (concat "Second change on main\n\nA body paragraph\n"
                 "spanning two lines.\n\nAnd a second paragraph."))
    ;; C1 on branch cp1251: a windows-1251 message, stored as such under
    ;; i18n.commitEncoding.  The module cannot read it, so the log goes
    ;; to git, which re-encodes it.
    (setq magit-git2-fixture--clock (1+ magit-git2-fixture--clock))
    (let ((file (expand-file-name "cp1251-message" root)))
      (let ((coding-system-for-write 'windows-1251-unix))
        (write-region "Сообщение в cp1251\n" nil file nil 'silent))
      (funcall git "update-ref" "refs/heads/cp1251"
               (funcall git "-c" "i18n.commitEncoding=windows-1251"
                        "commit-tree" "HEAD^{tree}" "-p" "HEAD" "-F" file)))
    ;; Two stashes.
    (funcall write "README" "Fixture, stashed once\n")
    (setq magit-git2-fixture--clock (1+ magit-git2-fixture--clock))
    (funcall git "stash" "push" "-q" "-m" "first stash")
    (funcall write "b.txt" "b, stashed twice\n")
    (setq magit-git2-fixture--clock (1+ magit-git2-fixture--clock))
    (funcall git "stash" "push" "-q")
    ;; Working tree: staged, unstaged, renamed, untracked, ignored.
    (funcall write "a.txt" "a\nmore\nstaged\n")
    (funcall git "add" "a.txt")
    (funcall write "b.txt" "b\nunstaged\n")
    (funcall git "mv" "old.txt" "renamed.txt")
    (funcall write "new.txt" "untracked\n")
    (funcall write "новый.txt" "untracked too\n")
    (funcall write "debug.log" "ignored\n")
    dir))

(defun magit-git2-fixture-build ()
  "Build all fixture repositories; return a plist of their directories."
  (let ((root (magit-git2-fixture-root)))
    (setq magit-git2-fixture--clock 0)
    (when (file-exists-p (expand-file-name "main" root))
      (error "Fixture already built in %s" root))
    (magit-git2-fixture--configs root)
    (magit-git2-fixture--git root "init" "-q" "--bare" "origin.git")
    (let ((main (magit-git2-fixture--main root)))
      (magit-git2-fixture--git main "worktree" "add" "-q" "../wt" "feature")
      (magit-git2-fixture--git root "clone" "-q" main "detached")
      (magit-git2-fixture--git (expand-file-name "detached" root)
                               "checkout" "-q" "--detach" "v1.0")
      (magit-git2-fixture--git root "init" "-q" "-b" "trunk" "unborn")
      (magit-git2-fixture-dirs))))

(defun magit-git2-fixture-dirs ()
  "Return the fixture states as ((NAME . DIRECTORY) ...)."
  (let ((root (magit-git2-fixture-root)))
    (mapcar (lambda (name)
              (cons name (file-name-as-directory
                          (expand-file-name name root))))
            '("main" "main/sub" "wt" "detached" "unborn" "origin.git"))))

(defun magit-git2-fixture-build-batch ()
  "Build the fixture and print its directories (for scripts)."
  (dolist (entry (magit-git2-fixture-build))
    (princ (format "%-10s %s\n" (car entry) (cdr entry)))))

(provide 'fixture)

;;; fixture.el ends here
