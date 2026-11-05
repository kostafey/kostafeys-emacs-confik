;;; magit-git2.el --- Answer Magit's read-only git queries in-process  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Kostafey <kostafey@gmail.com>

;; Author: Kostafey <kostafey@gmail.com>
;; Keywords: vc, tools
;; Package-Requires: ((emacs "29.1") (magit "4.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Every synchronous git call of Magit goes through `magit-process-git'.
;; An :around advice on that function looks at the local arguments; when
;; they have a shape registered here (with `magit-git2-define-shape'),
;; the shape's formatter produces git's exit code, stdout and stderr
;; from data the magit-git2-core module reads with libgit2, and no
;; process is started.  Every other call goes to git exactly as before.
;;
;; The formatters' spec is real git's output on the same repository;
;; test/magit-git2-test.el compares the two for every registered shape.
;; A formatter that cannot be sure to match git refuses, and the call
;; falls through to git.
;;
;; `magit-git2-mode' selects off, shadow (run git and the emulation and
;; record differences; Magit gets git's result) or on.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magit-git2-core)

(declare-function magit-git2--release "magit-git2-core")
(declare-function magit-git2--set-search-path "magit-git2-core")
(declare-function magit-git2--set-system-config-path "magit-git2-core")
(declare-function magit-git2--repo-info "magit-git2-core")
(declare-function magit-git2--revparse "magit-git2-core")
(declare-function magit-git2--short-id "magit-git2-core")
(declare-function magit-git2--ref "magit-git2-core")
(declare-function magit-git2--refs "magit-git2-core")
(declare-function magit-git2--head "magit-git2-core")
(declare-function magit-git2--config "magit-git2-core")
(declare-function magit-git2--upstream "magit-git2-core")
(declare-function magit-git2--ahead-behind "magit-git2-core")
(declare-function magit-git2--merge-base "magit-git2-core")
(declare-function magit-git2--descendant-p "magit-git2-core")
(declare-function magit-git2--reachable-from-any "magit-git2-core")
(declare-function magit-git2--describe "magit-git2-core")
(declare-function magit-git2--tips-reaching "magit-git2-core")
(declare-function magit-git2--log "magit-git2-core")
(declare-function magit-git2--commits "magit-git2-core")
(declare-function magit-git2--reflog "magit-git2-core")
(declare-function magit-add-section-hook "magit-section")
(declare-function magit-git-executable "magit-git")
(declare-function magit-process-git-arguments "magit-git")
(declare-function magit-process-environment "magit-process")
(declare-function magit--process-coding-system "magit-process")
(defvar magit-git-global-arguments)
(defvar magit-git-environment)
(defvar magit-git-executable)
(defvar magit--refresh-cache)
(defvar magit-status-sections-hook)
(defvar magit-status-headers-hook)

;;; Options

(defcustom magit-git2-mode
  (cond ((not (magit-git2-core-available-p)) 'off)
        ((eq system-type 'windows-nt) 'on)
        (t 'shadow))
  "How the calls of `magit-process-git' are answered.

off     Always run git.
shadow  Run git, and also the emulation; record every difference in
        `magit-git2-mismatches-buffer'.  Magit always gets git's result.
on      Answer the calls the emulation understands in-process, run git
        for the rest.

The default is on for Windows, where a process costs most, and shadow
elsewhere; off when the module did not load.  An unexpected error in
the emulation turns it off for the session."
  :group 'magit-git2
  :type '(choice (const off) (const shadow) (const on)))

(defvar magit-git2-mismatches-buffer "*magit-git2-mismatches*"
  "Buffer collecting the differences found in shadow mode.")

(defvar magit-git2-log-buffer "*magit-git2-log*"
  "Buffer logging unexpected errors of the emulation.")

;;; Statistics

(defvar magit-git2--stats (make-hash-table :test #'equal)
  "Per-command counters: COMMAND -> [EMULATED FELL-THROUGH MISMATCHED
LIBGIT2-ERRORS SKIPPED AHEAD].")

(defvar magit-git2--auto-disables 0
  "How often an unexpected error turned the emulation off.")

(defvar magit-git2--mismatch-reported nil
  "Whether this session already announced a mismatch.")

(defconst magit-git2--stat-slots
  '((emulated . 0) (fell-through . 1) (mismatched . 2) (libgit2-error . 3)
    (skipped . 4) (ahead . 5)))

(defun magit-git2--count (command what)
  "Count one WHAT (a key of `magit-git2--stat-slots') for COMMAND."
  (let ((v (or (gethash command magit-git2--stats)
               (puthash command (make-vector 6 0) magit-git2--stats)))
        (i (alist-get what magit-git2--stat-slots)))
    (aset v i (1+ (aref v i)))))

(defun magit-git2-stats-alist ()
  "Return the session statistics as an alist."
  (let (commands)
    (maphash (lambda (k v)
               (push (cons k (cl-loop for (slot . i) in magit-git2--stat-slots
                                      collect (cons slot (aref v i))))
                     commands))
             magit-git2--stats)
    `((mode . ,magit-git2-mode)
      (auto-disables . ,magit-git2--auto-disables)
      (commands . ,(sort commands (lambda (a b) (string< (car a) (car b))))))))

;;; Shapes

(defvar magit-git2--shapes nil
  "Emulated argument shapes: ((NAME DOC MATCHER EXAMPLES) ...).
MATCHER takes the local arguments and returns (EXIT STDOUT STDERR) or
nil; EXAMPLES takes a fixture state name and returns argument lists
for the differential test.")

(defun magit-git2--register (name doc matcher examples)
  "Register shape NAME with DOC, MATCHER and EXAMPLES."
  (if-let* ((cell (assq name magit-git2--shapes)))
      (setcdr cell (list doc matcher examples))
    (setq magit-git2--shapes
          (nconc magit-git2--shapes (list (list name doc matcher examples))))))

(defmacro magit-git2-define-shape (name pattern docstring &rest body)
  "Define the emulated argument shape NAME.
PATTERN is a `pcase' pattern for the local git arguments; patterns
must not overlap, as a refusal ends the dispatch.  BODY
returns (EXIT STDOUT STDERR), where STDERR is `unknown' when only the
exit code is certain; or nil to let git answer.  BODY starts with
`:examples FORM': FORM, with `fixture' bound to the name of a fixture
state, returns the argument lists the differential test compares."
  (declare (indent 2) (doc-string 3))
  (unless (eq (car body) :examples)
    (error "Shape %s has no :examples" name))
  (let ((examples (cadr body))
        (body (cddr body)))
    `(magit-git2--register
      ',name ,docstring
      (lambda (args) (pcase args (,pattern ,@body)))
      (lambda (fixture) (ignore fixture) ,examples))))

(defvar magit-git2--shape nil
  "The shape that answered the last dispatch.")

(defvar magit-git2--memo nil
  "Values computed for the current answer: ((KEY . VALUE) ...).")

(defvar magit-git2--refresh-memos nil
  "The memos of one buffer refresh: (START (SCOPE . MEMO) ...).
START is its `magit--refresh-start-time', a list made anew for each
refresh.  SCOPE is a directory and the global arguments.")

(defun magit-git2--current-memos ()
  "The memos of the buffer refresh under way, or nil outside one.
A buffer refresh only reads.  Magit's own cache is no measure of that:
`magit-run-git' also binds it around a command that writes.  Like that
cache these memos do not apply where Magit binds it to nil, or in a
recursive edit."
  (when-let* ((start (bound-and-true-p magit--refresh-start-time))
              ((bound-and-true-p magit--refreshing-buffer-p))
              ((bound-and-true-p magit--refresh-cache))
              ((zerop (recursion-depth))))
    (unless (eq (car magit-git2--refresh-memos) start)
      (setq magit-git2--refresh-memos (list start)))
    magit-git2--refresh-memos))

(defmacro magit-git2--memoize (key &rest body)
  "Return the value of BODY, computed once per memo for KEY."
  (declare (indent 1))
  (let ((k (make-symbol "key"))
        (cell (make-symbol "cell")))
    `(let* ((,k ,key)
            (,cell (assoc ,k magit-git2--memo)))
       (if ,cell
           (cdr ,cell)
         (let ((value (progn ,@body)))
           (push (cons ,k value) magit-git2--memo)
           value)))))

(defun magit-git2--refuse ()
  "Give up on the current call: git answers it."
  (throw 'magit-git2--refuse nil))

(defun magit-git2--out (stdout)
  "A successful result printing STDOUT."
  (list 0 stdout ""))

(defun magit-git2--die (exit &optional stderr)
  "A failed result: EXIT, nothing on stdout, STDERR (default unknown)."
  (list exit "" (or stderr 'unknown)))

(defun magit-git2--with-memo (function)
  "Call FUNCTION in `default-directory', made absolute, with its memo."
  ;; libgit2 knows nothing of "~".
  (let* ((default-directory (file-name-as-directory
                             (expand-file-name default-directory)))
         (memos (magit-git2--current-memos))
         (scope (cons default-directory magit-git-global-arguments))
         (magit-git2--memo (cdr (assoc scope (cdr memos)))))
    (unwind-protect (funcall function)
      (when memos
        (setf (alist-get scope (cdr memos) nil nil #'equal)
              magit-git2--memo)))))

(defun magit-git2--dispatch (args)
  "Return (EXIT STDOUT STDERR) for the local git ARGS, or nil."
  (magit-git2--with-memo
   (lambda ()
     (setq magit-git2--shape nil)
     (catch 'magit-git2--refuse
       ;; Global arguments other than -c would change any answer.
       (magit-git2--global-config)
       (cl-loop for (name _doc matcher _examples) in magit-git2--shapes
                for result = (funcall matcher args)
                when result
                return (progn (setq magit-git2--shape name) result))))))

;;; Repository

(defun magit-git2--info ()
  "Plist describing the repository of `default-directory', or nil."
  (magit-git2--memoize 'info
    (magit-git2--repo-info default-directory)))

(defun magit-git2--repo ()
  "Like `magit-git2--info', but refuse outside a repository."
  (or (magit-git2--info) (magit-git2--refuse)))

(defun magit-git2--cwd ()
  "The current directory, absolute, with a final slash."
  (file-name-as-directory (expand-file-name default-directory)))

(defun magit-git2--same-file-p (a b)
  "Whether the file names A and B are the same, ignoring case on Windows."
  (eq t (compare-strings a nil nil b nil nil
                         (memq system-type '(windows-nt ms-dos)))))

(defun magit-git2--under-p (dir file)
  "Whether FILE is DIR (with a final slash) or below it."
  (string-prefix-p dir (file-name-as-directory file)
                   (memq system-type '(windows-nt ms-dos))))

(defun magit-git2--location ()
  "Where the current directory is: one of `outside', `gitdir' (inside
the git directory), `bare' or (worktree TOP DEPTH)."
  (magit-git2--memoize 'location
    (let ((info (magit-git2--info))
          (cwd (magit-git2--cwd)))
      (cond ((null info) 'outside)
            ((magit-git2--under-p (plist-get info :gitdir) cwd) 'gitdir)
            ((plist-get info :bare) 'bare)
            (t (let ((top (plist-get info :workdir)))
                 (unless (magit-git2--under-p top cwd)
                   (magit-git2--refuse))
                 (list 'worktree top
                       (length (split-string (substring cwd (length top))
                                             "/" t)))))))))

(defun magit-git2--head-info ()
  "HEAD as (:symref NAME :hash HASH)."
  (magit-git2--memoize 'head
    (magit-git2--head default-directory)))

(defun magit-git2--refs-list ()
  "All refs but HEAD, sorted: ((NAME HASH PEELED SYMREF) ...)."
  (magit-git2--memoize 'refs
    (sort (magit-git2--refs default-directory)
          (lambda (a b) (string< (car a) (car b))))))

(defun magit-git2--ref-exists-p (name)
  "Whether NAME is a ref that resolves (git's refs_ref_exists)."
  (magit-git2--memoize (cons 'exists name)
    (let ((depth 0) ref)
      (while (and (setq ref (magit-git2--ref default-directory name))
                  (eq (car ref) 'symbolic)
                  (< (cl-incf depth) 5))
        (setq name (cadr ref)))
      (eq (car ref) 'direct))))

(defun magit-git2--replace-refs-p ()
  "Whether replace refs exist (libgit2 does not honour them)."
  (cl-some (lambda (r) (string-prefix-p "refs/replace/" (car r)))
           (magit-git2--refs-list)))

;;; Config

(defun magit-git2--canonical-key (key)
  "KEY with section and name downcased, or refuse."
  (if (string-match (rx bos (group (+ (any alnum "-"))) "."
                         (? (group (+ anything)) ".")
                         (group alpha (* (any alnum "-"))) eos)
                     key)
      (concat (downcase (match-string 1 key)) "."
              (if (match-string 2 key) (concat (match-string 2 key) ".") "")
              (downcase (match-string 3 key)))
    (magit-git2--refuse)))

(defun magit-git2--global-config ()
  "The -c pairs of `magit-git-global-arguments': ((KEY . VALUE) ...).
Refuse when the global arguments have anything else."
  (magit-git2--memoize 'global-config
    (let ((args magit-git-global-arguments)
          pairs)
      (while args
        (pcase (pop args)
          ((or "--no-pager" "--literal-pathspecs" "--no-optional-locks"))
          ("-c" (let ((kv (or (pop args) "")))
                  (if (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" kv)
                      (let ((key (match-string 1 kv))
                            (value (match-string 2 kv)))
                        (push (cons (magit-git2--canonical-key key) value)
                              pairs))
                    (magit-git2--refuse))))
          (_ (magit-git2--refuse))))
      (nreverse pairs))))

(defconst magit-git2--level-order
  '(programdata system xdg global local worktree app))

(defun magit-git2--config-entries (&optional level)
  "Config entries ((NAME VALUE LEVEL) ...) in git's order.
LEVEL is nil for all levels, else one of `magit-git2--level-order'."
  (magit-git2--memoize (cons 'config level)
    (let ((entries (magit-git2--config default-directory level)))
      (if level
          entries
        (sort entries
              (lambda (a b)
                (< (cl-position (nth 2 a) magit-git2--level-order)
                   (cl-position (nth 2 b) magit-git2--level-order))))))))

(defun magit-git2--config-values (key)
  "The values of KEY in git's order, the -c overrides last.
A value is nil for a variable written without \"=\"."
  (let ((key (magit-git2--canonical-key key)))
    (append (cl-loop for (name value) in (magit-git2--config-entries)
                     when (equal name key) collect value)
            (cl-loop for (name . value) in (magit-git2--global-config)
                     when (equal name key) collect value))))

(defun magit-git2--parse-bool (value)
  "Git's boolean VALUE: t, nil, or `invalid'."
  (cond ((null value) t)
        ((member (downcase value) '("true" "yes" "on")) t)
        ((member (downcase value) '("false" "no" "off" "")) nil)
        ((string-match-p "\\`-?[0-9]+\\'" value)
         (/= 0 (string-to-number value)))
        (t 'invalid)))

(defun magit-git2--config-bool (key default)
  "The boolean value of KEY, DEFAULT when unset; refuse when invalid."
  (let ((values (magit-git2--config-values key)))
    (if (null values)
        default
      (let ((b (magit-git2--parse-bool (car (last values)))))
        (if (eq b 'invalid) (magit-git2--refuse) b)))))

;;; Environment

(defconst magit-git2--refused-environment
  '("GIT_DIR" "GIT_WORK_TREE" "GIT_INDEX_FILE" "GIT_OBJECT_DIRECTORY"
    "GIT_ALTERNATE_OBJECT_DIRECTORIES" "GIT_COMMON_DIR" "GIT_NAMESPACE"
    "GIT_CEILING_DIRECTORIES" "GIT_DISCOVERY_ACROSS_FILESYSTEM"
    "GIT_CONFIG" "GIT_CONFIG_PARAMETERS" "GIT_CONFIG_COUNT"
    "GIT_CONFIG_NOSYSTEM" "GIT_CONFIG_GLOBAL" "GIT_REPLACE_REF_BASE"
    "GIT_NO_REPLACE_OBJECTS" "GIT_GRAFT_FILE" "GIT_SHALLOW_FILE"
    "GIT_DEFAULT_HASH" "GIT_ICASE_PATHSPECS" "GIT_GLOB_PATHSPECS"
    "GIT_NOGLOB_PATHSPECS")
  "Variables that change what git reads; set, they disable emulation.")

(defconst magit-git2--matched-environment
  '("HOME" "XDG_CONFIG_HOME" "USERPROFILE" "HOMEDRIVE" "HOMEPATH"
    "PROGRAMDATA" "GIT_CONFIG_SYSTEM")
  "Variables libgit2 read from the Emacs process at start-up.
When git would see other values, emulation is off.")

(defvar magit-git2--initial-environment
  (mapcar (lambda (var)
            (cons var (let ((process-environment initial-environment))
                        (getenv var))))
          magit-git2--matched-environment)
  "The values of `magit-git2--matched-environment' libgit2 knows.")

(defun magit-git2--environment-ok-p ()
  "Whether git would run in the environment libgit2 assumes."
  (let ((process-environment (append (bound-and-true-p magit-git-environment)
                                     process-environment)))
    (and (cl-notany #'getenv magit-git2--refused-environment)
         (cl-every (lambda (entry)
                     (equal (getenv (car entry)) (cdr entry)))
                   magit-git2--initial-environment))))

(defvar magit-git2--system-config-ok
  (let ((file (cdr (assoc "GIT_CONFIG_SYSTEM"
                          magit-git2--initial-environment))))
    (or (null file)
        (and (equal (file-name-nondirectory file) "gitconfig")
             (magit-git2-core-available-p)
             (progn (magit-git2--set-system-config-path
                     (file-name-directory (expand-file-name file)))
                    t))))
  "Whether libgit2 reads the system config git reads.
GIT_CONFIG_SYSTEM names a file; libgit2 searches directories for
\"gitconfig\", so other file names cannot be matched.")

(defun magit-git2-set-system-config-from-git (&optional executable)
  "Make libgit2 read the system config of Git for Windows EXECUTABLE.
EXECUTABLE defaults to `magit-git-executable'.  Git for Windows keeps
the system config under its install root, where libgit2 does not
look by default.  Return the directory used, or nil."
  (when-let* ((exe (executable-find (or executable magit-git-executable)))
              (root (locate-dominating-file
                     exe (lambda (dir)
                           (or (file-exists-p
                                (expand-file-name "etc/gitconfig" dir))
                               (file-exists-p
                                (expand-file-name "mingw64/etc/gitconfig"
                                                  dir))))))
              (dir (if (file-exists-p (expand-file-name "etc/gitconfig" root))
                       (expand-file-name "etc" root)
                     (expand-file-name "mingw64/etc" root))))
    (magit-git2--set-system-config-path dir)
    dir))

;;; Object names

(defconst magit-git2--rev-parse-rules
  '("%s" "refs/%s" "refs/tags/%s" "refs/heads/%s" "refs/remotes/%s"
    "refs/remotes/%s/HEAD")
  "Where git looks a ref name up, in order (gitrevisions(7)).")

(defun magit-git2--match-rule (refname rule)
  "The part of REFNAME that matches the %s of RULE, or nil."
  (let* ((pos (string-search "%s" rule))
         (prefix (substring rule 0 pos))
         (suffix (substring rule (+ pos 2))))
    (and (string-prefix-p prefix refname)
         (>= (length refname) (+ (length prefix) (length suffix)))
         (string-suffix-p suffix (substring refname (length prefix)))
         (substring refname (length prefix)
                    (- (length refname) (length suffix))))))

(defun magit-git2--shorten-ref (refname strict)
  "REFNAME as git's --abbrev-ref and %(refname:short) shorten it.
The most specific lookup rule that matches REFNAME, the bare one aside,
gives the short name, unless another ref answers to that name under a
rule looked up before it, or under any other rule when STRICT; then
the next matching rule is tried, and REFNAME stays whole when none is
left."
  (let ((rules magit-git2--rev-parse-rules))
    (or (seq-some
         (lambda (rule)
           (when-let* ((short (magit-git2--match-rule refname rule)))
             (and (not (seq-some
                        (lambda (other)
                          (and (not (equal other rule))
                               (magit-git2--ref-exists-p
                                (format other short))))
                        (if strict
                            rules
                          (seq-take rules (seq-position rules rule)))))
                  short)))
         (reverse (cdr rules)))
        refname)))

(defun magit-git2--dwim-count (name)
  "How many refs the bare NAME could mean."
  (cl-count-if (lambda (rule)
                 (magit-git2--ref-exists-p (format rule name)))
               magit-git2--rev-parse-rules))

(defconst magit-git2--rev-regexp
  (rx bos
      (group (? (any "A-Za-z0-9_./+") (* (any "A-Za-z0-9_./+-"))))
      (? (group "@{" (or (any "uU") (regexp "[uU][pP][sS][tT][rR][eE][aA][mM]"))
                "}"))
      (group (* (or (seq "~" (* digit)) (seq "^" (* digit))
                    "^{commit}" "^{tree}" "^{}")))
      eos)
  "The revisions understood here: NAME[@{upstream}][~N|^N|^{TYPE}]...")

(cl-defun magit-git2--upstream-error (base)
  "Check the upstream of BASE (\"\" for HEAD); return a git error or nil.
Refuse what is not certain."
  (let* ((branch
          (if (member base '("" "HEAD" "@"))
              (let ((symref (plist-get (magit-git2--head-info) :symref)))
                (if (and symref (string-prefix-p "refs/heads/" symref))
                    (substring symref 11)
                  (cl-return-from magit-git2--upstream-error
                    "fatal: HEAD does not point to a branch\n")))
            base))
         (full (concat "refs/heads/" branch)))
    (cond ((string-prefix-p "refs/" branch) (magit-git2--refuse))
          ((not (magit-git2--ref-exists-p full))
           (format "fatal: no such branch: '%s'\n" branch))
          ((not (magit-git2--upstream default-directory full))
           (format "fatal: no upstream configured for branch '%s'\n"
                   branch))
          ((not (magit-git2--ref-exists-p
                 (magit-git2--upstream default-directory full)))
           (magit-git2--refuse)))))

(defun magit-git2--resolve (rev)
  "Resolve the revision REV: (HASH TYPE REFNAME), nil, or (die MESSAGE).
Refuse syntax outside `magit-git2--rev-regexp' and ambiguous names."
  (magit-git2--memoize (cons 'resolve rev)
    (unless (string-match magit-git2--rev-regexp rev)
      (magit-git2--refuse))
    (let ((base (match-string 1 rev))
          (upstream (match-string 2 rev)))
      (cond ((string-match-p "\\.\\.\\|\\.lock\\'\\|//\\|/\\'" base)
             (magit-git2--refuse))
            ((and (string-match-p "\\`[0-9a-f]+\\'" base)
                  (/= (length base) 40))
             (magit-git2--refuse))
            ((and (equal base "") (not upstream))
             (magit-git2--refuse))
            ((and (not (member base '("" "@")))
                  (> (magit-git2--dwim-count base) 1))
             (magit-git2--refuse)))
      (or (and upstream
               (when-let* ((err (magit-git2--upstream-error base)))
                 (list 'die err)))
          (magit-git2--revparse default-directory rev)))))

(defun magit-git2--commit (rev)
  "The hash of the commit REV names, nil if none; refuse non-commits."
  (pcase (magit-git2--resolve rev)
    ('nil nil)
    (`(die ,_) (magit-git2--refuse))
    (`(,hash commit . ,_) hash)
    (`(,_ tag . ,_)
     (pcase (magit-git2--resolve (concat rev "^{commit}"))
       (`(,hash commit . ,_) hash)
       (_ (magit-git2--refuse))))
    (_ (magit-git2--refuse))))

;;; Abbreviation

(defun magit-git2--read-bytes (file offset length)
  "LENGTH bytes of FILE from OFFSET, as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil offset (+ offset length))
    (buffer-string)))

(defun magit-git2--u32 (s i)
  "The big-endian 32-bit number at byte I of S."
  (logior (ash (aref s i) 24) (ash (aref s (+ i 1)) 16)
          (ash (aref s (+ i 2)) 8) (aref s (+ i 3))))

(defun magit-git2--idx-count (file)
  "The number of objects in the pack index FILE."
  (let ((magic (magit-git2--read-bytes file 0 4)))
    (magit-git2--u32 (magit-git2--read-bytes
                      file (if (equal magic "\377tOc") (+ 8 1020) 1020) 4)
                     0)))

(defun magit-git2--midx (file)
  "(COUNT . PACK-INDEX-NAMES) of the multi-pack-index FILE; refuse what
is not a single version 1 or 2 SHA-1 index."
  (let* ((header (magit-git2--read-bytes file 0 12))
         (chunks (aref header 6))
         (table (magit-git2--read-bytes file 12 (* 12 (1+ chunks))))
         offsets count names)
    (unless (and (equal (substring header 0 4) "MIDX")
                 (memq (aref header 4) '(1 2))
                 (= (aref header 5) 1)
                 (= (aref header 7) 0))
      (magit-git2--refuse))
    (dotimes (i (1+ chunks))
      (push (cons (substring table (* 12 i) (+ 4 (* 12 i)))
                  (+ (* (magit-git2--u32 table (+ 4 (* 12 i))) 4294967296)
                     (magit-git2--u32 table (+ 8 (* 12 i)))))
            offsets))
    (setq offsets (nreverse offsets))
    (cl-loop for ((id . start) (_ . end)) on offsets
             while end
             do (pcase id
                  ("OIDF" (setq count (magit-git2--u32
                                       (magit-git2--read-bytes
                                        file (+ start 1020) 4)
                                       0)))
                  ("PNAM" (setq names
                                (split-string
                                 (magit-git2--read-bytes
                                  file start (- end start))
                                 "\0" t)))))
    (unless count (magit-git2--refuse))
    (cons count names)))

(defvar magit-git2--object-count-cache nil
  "((PACK-DIRECTORY STAMP COUNT) ...).")

(defun magit-git2--object-count ()
  "Git's approximate object count: the objects in packs only."
  (let* ((objects (expand-file-name
                   "objects/" (plist-get (magit-git2--repo) :commondir)))
         (pack (expand-file-name "pack/" objects))
         (midx (expand-file-name "multi-pack-index" pack))
         (use-midx (and (file-exists-p midx)
                        (magit-git2--config-bool "core.multiPackIndex" t)))
         (stamp (list (file-attribute-modification-time
                       (file-attributes pack))
                      (and use-midx (file-attribute-modification-time
                                     (file-attributes midx)))))
         (cached (assoc pack magit-git2--object-count-cache)))
    (when (or (file-exists-p (expand-file-name "info/alternates" objects))
              (file-exists-p (expand-file-name "multi-pack-index.d" pack)))
      (magit-git2--refuse))
    (if (and cached (equal (nth 1 cached) stamp))
        (nth 2 cached)
      (let* ((m (and use-midx (magit-git2--midx midx)))
             (count (or (car m) 0)))
        (when (file-directory-p pack)
          (dolist (idx (directory-files pack nil "\\.idx\\'"))
            (when (and (not (member idx (cdr m)))
                       (file-exists-p
                        (expand-file-name
                         (concat (file-name-sans-extension idx) ".pack")
                         pack)))
              (cl-incf count (magit-git2--idx-count
                              (expand-file-name idx pack))))))
        (setf (alist-get pack magit-git2--object-count-cache nil nil #'equal)
              (list stamp count))
        count))))

(defun magit-git2--abbrev ()
  "The abbreviation length git uses here."
  (magit-git2--memoize 'abbrev
    (let ((values (magit-git2--config-values "core.abbrev")))
      (if (or (null values) (equal (downcase (or (car (last values)) ""))
                                   "auto"))
          (let ((count (magit-git2--object-count)))
            ;; odb_find_abbrev_len(): msb(count) + 1 bits, half of
            ;; that in hex digits, at least 7.
            (max 7 (/ (1+ (1+ (logb (max count 1)))) 2)))
        (let ((v (car (last values))))
          (cond ((null v) (magit-git2--refuse))
                ((string-match-p "\\`[0-9]+\\'" v)
                 (let ((n (string-to-number v)))
                   (if (<= 4 n 40) n (magit-git2--refuse))))
                ((null (magit-git2--parse-bool v)) 40)
                (t (magit-git2--refuse))))))))

(defun magit-git2--short (hash)
  "HASH abbreviated like git abbreviates it."
  (magit-git2--short-id default-directory hash (magit-git2--abbrev)))

;;; rev-parse

(defconst magit-git2--worktree-only
  "fatal: this operation must be run in a work tree\n")

(magit-git2-define-shape rev-parse-show-toplevel
    `("rev-parse" "--show-toplevel")
  "The top of the working tree."
  :examples '(("rev-parse" "--show-toplevel"))
  (pcase (magit-git2--location)
    ('outside (magit-git2--die 128))
    ((or 'gitdir 'bare) (magit-git2--die 128 magit-git2--worktree-only))
    (`(worktree ,top ,_)
     (magit-git2--out (concat (directory-file-name top) "\n")))))

(magit-git2-define-shape rev-parse-show-cdup
    `("rev-parse" "--show-cdup")
  "The way up to the top of the working tree."
  :examples '(("rev-parse" "--show-cdup"))
  (pcase (magit-git2--location)
    ('outside (magit-git2--die 128))
    ((or 'gitdir 'bare) (magit-git2--out ""))
    (`(worktree ,_ ,depth)
     (magit-git2--out
      (concat (apply #'concat (make-list depth "../")) "\n")))))

(magit-git2-define-shape rev-parse-git-dir
    `("rev-parse" "--git-dir")
  "The git directory, relative when it is ./.git or ./."
  :examples '(("rev-parse" "--git-dir"))
  (let ((loc (magit-git2--location)))
    (if (eq loc 'outside)
        (magit-git2--die 128)
      (let ((gitdir (plist-get (magit-git2--info) :gitdir))
            (cwd (magit-git2--cwd)))
        (magit-git2--out
         (concat (cond ((magit-git2--same-file-p gitdir cwd) ".")
                       ((eq loc 'gitdir) (magit-git2--refuse))
                       ((magit-git2--same-file-p gitdir (concat cwd ".git/"))
                        ".git")
                       (t (directory-file-name gitdir)))
                 "\n"))))))

(magit-git2-define-shape rev-parse-is-bare-repository
    `("rev-parse" "--is-bare-repository")
  "Whether the repository is bare."
  :examples '(("rev-parse" "--is-bare-repository"))
  (if (eq (magit-git2--location) 'outside)
      (magit-git2--die 128)
    (magit-git2--out (if (plist-get (magit-git2--info) :bare)
                         "true\n" "false\n"))))

(magit-git2-define-shape rev-parse-is-inside-work-tree
    `("rev-parse" "--is-inside-work-tree")
  "Whether the current directory is in the working tree."
  :examples '(("rev-parse" "--is-inside-work-tree"))
  (pcase (magit-git2--location)
    ('outside (magit-git2--die 128))
    (`(worktree . ,_) (magit-git2--out "true\n"))
    (_ (magit-git2--out "false\n"))))

(defconst magit-git2--example-revs
  '("HEAD" "HEAD~" "HEAD~10" "HEAD^{commit}" "@{upstream}" "main"
    "main@{upstream}" "feature@{upstream}" "trunk@{upstream}" "v1.0"
    "v1.0^{commit}" "light" "origin/main" "origin/feature" "refs/stash"
    "refs/tags/main" "refs/heads/main" "nope")
  "Revisions the differential test tries in every fixture state.")

(magit-git2-define-shape rev-parse-verify
    `("rev-parse" "--verify" . ,(or `("--quiet" ,rev) `(,rev)))
  "The object a revision names; exit 128 (1 with --quiet) for none."
  :examples (cl-loop for rev in magit-git2--example-revs
                     collect `("rev-parse" "--verify" ,rev)
                     collect `("rev-parse" "--verify" "--quiet" ,rev))
  (let ((quiet (equal (nth 2 args) "--quiet")))
    (pcase (magit-git2--resolve rev)
      (`(die ,msg) (if quiet (magit-git2--refuse) (magit-git2--die 128 msg)))
      ('nil (if quiet
                (magit-git2--die 1 "")
              (magit-git2--die 128 "fatal: Needed a single revision\n")))
      (`(,hash . ,_) (magit-git2--out (concat hash "\n"))))))

(magit-git2-define-shape rev-parse-short
    `("rev-parse" "--short" ,rev)
  "The abbreviated object id a revision names."
  :examples (cl-loop for rev in magit-git2--example-revs
                     collect `("rev-parse" "--short" ,rev))
  (pcase (magit-git2--resolve rev)
    (`(die ,msg) (magit-git2--die 128 msg))
    ('nil (magit-git2--die 128 "fatal: Needed a single revision\n"))
    (`(,hash . ,_) (magit-git2--out (concat (magit-git2--short hash) "\n")))))

(magit-git2-define-shape rev-parse-verify-abbrev-ref
    `("rev-parse" "--verify" "--abbrev-ref"
      ,(and rev (pred (string-match-p "@{\\(?:u\\|upstream\\)}\\'"))))
  "The short name of an upstream branch."
  :examples (cl-loop for rev in magit-git2--example-revs
                     when (string-suffix-p "@{upstream}" rev)
                     collect `("rev-parse" "--verify" "--abbrev-ref" ,rev))
  (pcase (magit-git2--resolve rev)
    (`(die ,msg) (magit-git2--die 128 msg))
    (`(,_ ,_ ,(and refname (pred stringp)))
     ;; --abbrev-ref is strict when core.warnAmbiguousRefs is on.
     (magit-git2--out
      (concat (magit-git2--shorten-ref
               refname (magit-git2--config-bool "core.warnAmbiguousRefs" t))
              "\n")))))

(magit-git2-define-shape rev-parse-upstream
    `("rev-parse"
      ,(and rev (pred (string-match-p "\\`@{\\(?:u\\|upstream\\)}\\'"))))
  "The object the upstream of HEAD points to."
  :examples '(("rev-parse" "@{upstream}") ("rev-parse" "@{u}"))
  (pcase (magit-git2--resolve rev)
    (`(die ,msg) (magit-git2--die 128 msg))
    (`(,hash . ,_) (magit-git2--out (concat hash "\n")))))

;;; symbolic-ref

(magit-git2-define-shape symbolic-ref
    `("symbolic-ref" . ,(or `("--short" ,name) `(,name)))
  "The target of a symbolic ref."
  :examples (cl-loop for name in '("HEAD" "refs/remotes/origin/HEAD"
                                   "refs/remotes/nope/HEAD" "refs/heads/main")
                     collect `("symbolic-ref" ,name)
                     collect `("symbolic-ref" "--short" ,name))
  (unless (or (equal name "HEAD") (string-prefix-p "refs/" name))
    (magit-git2--refuse))
  (magit-git2--repo)
  (pcase (magit-git2--ref default-directory name)
    (`(symbolic ,target)
     (magit-git2--out
      (concat (if (equal (nth 1 args) "--short")
                  (magit-git2--shorten-ref target nil)
                target)
              "\n")))
    (_ (magit-git2--die
        128 (format "fatal: ref %s is not a symbolic ref\n" name)))))

;;; config

(defun magit-git2--config-scope (option)
  "The levels a config OPTION reads, or `all'; refuse unknown ones."
  (pcase option
    ('nil 'all)
    ("--local" '(local))
    ("--global" '(xdg global))
    ("--system" '(system))
    ("--worktree" '(worktree))
    (_ (magit-git2--refuse))))

(defun magit-git2--scoped-values (key scope)
  "The values of KEY in the levels SCOPE (see `magit-git2--config-scope')."
  (if (eq scope 'all)
      (magit-git2--config-values key)
    (let ((key (magit-git2--canonical-key key)))
      (cl-loop for (name value level) in (magit-git2--config-entries)
               when (and (equal name key) (memq level scope))
               collect value))))

(defun magit-git2--config-line (name value z)
  "One entry of `git config --list', NUL-terminated if Z."
  (concat name (and value (concat (if z "\n" "=") value)) (if z "\0" "\n")))

(magit-git2-define-shape config-list
    `("config" . ,(or `("--list" "-z") `("--list") `("-z" "--list")))
  "All config entries, the -c overrides last."
  :examples '(("config" "--list" "-z") ("config" "--list"))
  (magit-git2--repo)
  (let ((z (member "-z" args)))
    (magit-git2--out
     (concat (mapconcat (pcase-lambda (`(,name ,value ,_))
                          (magit-git2--config-line name value z))
                        (magit-git2--config-entries))
             (mapconcat (pcase-lambda (`(,name . ,value))
                          (magit-git2--config-line name value z))
                        (magit-git2--global-config))))))

(magit-git2-define-shape config-get
    `("config" . ,(and opts
                       (guard (and (or (member "--get" opts)
                                       (member "--get-all" opts))
                                   (not (string-prefix-p
                                         "-" (car (last opts))))))))
  "The value(s) of one key: [--local|...] [-z] --get|--get-all [--include]."
  :examples (cl-loop for key in '("magitgit2.level" "magitgit2.multi"
                                  "magitgit2.included" "magitgit2.novalue"
                                  "magitgit2.missing" "core.preloadIndex"
                                  "branch.main.remote" "MagitGit2.Level"
                                  "status.showUntrackedFiles")
                     collect `("config" "--get" ,key)
                     collect `("config" "--get-all" ,key)
                     collect `("config" "-z" "--get-all" ,key)
                     collect `("config" "--local" "-z" "--get-all"
                               "--include" ,key)
                     collect `("config" "--global" "--get" ,key)
                     collect `("config" "--system" "--get-all" ,key))
  (let* ((key (car (last opts)))
         (flags (butlast opts))
         (scope nil) (z nil) (mode nil))
    (dolist (f flags)
      (pcase f
        ((or "--local" "--global" "--system" "--worktree")
         (when scope (magit-git2--refuse))
         (setq scope f))
        ((or "-z" "--null") (setq z t))
        ("--include")
        ((or "--get" "--get-all")
         (when mode (magit-git2--refuse))
         (setq mode f))
        (_ (magit-git2--refuse))))
    (unless mode (magit-git2--refuse))
    (magit-git2--repo)
    (let ((values (magit-git2--scoped-values
                   key (magit-git2--config-scope scope)))
          (end (if z "\0" "\n")))
      (cond ((null values) (magit-git2--die 1 ""))
            ((equal mode "--get")
             (magit-git2--out (concat (car (last values)) end)))
            (t (magit-git2--out
                (mapconcat (lambda (v) (concat v end)) values)))))))

(magit-git2-define-shape config-bool
    `("config" "--bool" . ,(or `("--default" ,default ,key) `(,key)))
  "A boolean value, normalized to true or false."
  :examples (cl-loop for key in '("magitgit2.global" "magitgit2.system"
                                  "magitgit2.novalue" "magitgit2.missing"
                                  "magitgit2.level")
                     collect `("config" "--bool" ,key)
                     collect `("config" "--bool" "--default" "false" ,key)
                     collect `("config" "--bool" "--default" "true" ,key))
  (magit-git2--repo)
  ;; git parses every value in turn and dies at the first bad one.
  (let* ((values (or (magit-git2--config-values key)
                     (and default (list default))))
         (bad (cl-find-if (lambda (v) (eq (magit-git2--parse-bool v) 'invalid))
                          values)))
    (cond ((null values) (magit-git2--die 1 ""))
          (bad
           (magit-git2--die
            128 (format "fatal: bad boolean config value '%s' for '%s'\n"
                        bad key)))
          (t (magit-git2--out (if (magit-git2--parse-bool (car (last values)))
                                  "true\n" "false\n"))))))

;;; remote

(magit-git2-define-shape remote
    `("remote")
  "The names of the remotes."
  :examples '(("remote"))
  (let ((gitdir (plist-get (magit-git2--repo) :commondir)))
    ;; Remotes may also be defined by files there.
    (dolist (legacy '("remotes" "branches"))
      (let ((dir (expand-file-name legacy gitdir)))
        (when (and (file-directory-p dir)
                   (directory-files dir nil "\\`[^.]"))
          (magit-git2--refuse))))
    (let (names)
      (dolist (entry (append (magit-git2--config-entries)
                             (mapcar (lambda (p) (list (car p) (cdr p)))
                                     (magit-git2--global-config))))
        (when (string-match "\\`remote\\.\\(.+\\)\\.[^.]+\\'" (car entry))
          (cl-pushnew (match-string 1 (car entry)) names :test #'equal)))
      (magit-git2--out
       (mapconcat (lambda (n) (concat n "\n"))
                  (sort names #'string<))))))

;;; describe

(defun magit-git2--tags ()
  "The tags as ((NAME . COMMIT) ...); COMMIT is nil for non-commits."
  (cl-loop for (name hash peeled) in (magit-git2--refs-list)
           when (string-prefix-p "refs/tags/" name)
           collect (cons name (or peeled hash))))

(magit-git2-define-shape describe-long-tags
    `("describe" "--long" "--tags" . ,(or `(,rev) '()))
  "The nearest tag, its distance and the abbreviated commit."
  :examples '(("describe" "--long" "--tags")
              ("describe" "--long" "--tags" "HEAD~")
              ("describe" "--long" "--tags" "feature"))
  (magit-git2--repo)
  (let ((rev (or rev "HEAD")))
    (cond ((null (magit-git2--tags))
           (magit-git2--die
            128 "fatal: No names found, cannot describe anything.\n"))
          ((null (magit-git2--resolve rev))
           (magit-git2--die
            128 (format "fatal: Not a valid object name %s\n" rev)))
          (t (let ((hash (or (magit-git2--commit rev) (magit-git2--refuse))))
               (if-let* ((d (magit-git2--describe default-directory hash
                                                 (magit-git2--abbrev))))
                   (magit-git2--out (concat d "\n"))
                 (magit-git2--die
                  128 (format "fatal: No tags can describe '%s'.\n%s" hash
                              "Try --always, or create some tags.\n"))))))))

(defun magit-git2--hex-to-raw (hash)
  "The 20 bytes of the hex object id HASH, as a unibyte string."
  (apply #'unibyte-string
         (cl-loop for i from 0 below 40 by 2
                  collect (string-to-number (substring hash i (+ i 2)) 16))))

(defun magit-git2--graph-contains-p (file raw)
  "Whether the commit-graph FILE lists the object id RAW (20 bytes)."
  (let* ((header (magit-git2--read-bytes file 0 8))
         (chunks (aref header 6))
         (table (magit-git2--read-bytes file 8 (* 12 (1+ chunks))))
         oidf oidl)
    (unless (and (equal (substring header 0 4) "CGPH") (= (aref header 5) 1))
      (magit-git2--refuse))
    (dotimes (i chunks)
      (let ((id (substring table (* 12 i) (+ 4 (* 12 i))))
            (offset (+ (* (magit-git2--u32 table (+ 4 (* 12 i))) 4294967296)
                       (magit-git2--u32 table (+ 8 (* 12 i))))))
        (pcase id
          ("OIDF" (setq oidf offset))
          ("OIDL" (setq oidl offset)))))
    (unless (and oidf oidl) (magit-git2--refuse))
    (let* ((b (aref raw 0))
           (lo (if (= b 0) 0
                 (magit-git2--u32 (magit-git2--read-bytes
                                   file (+ oidf (* 4 (1- b))) 4)
                                  0)))
           (hi (magit-git2--u32 (magit-git2--read-bytes file (+ oidf (* 4 b)) 4)
                                0))
           found)
      (while (and (< lo hi) (not found))
        (let* ((mid (/ (+ lo hi) 2))
               (oid (magit-git2--read-bytes file (+ oidl (* 20 mid)) 20)))
          (cond ((string= oid raw) (setq found t))
                ((string< oid raw) (setq lo (1+ mid)))
                (t (setq hi mid)))))
      found)))

(defun magit-git2--in-commit-graph-p (hash)
  "Whether a commit-graph git would read has the commit HASH."
  (and (magit-git2--config-bool "core.commitGraph" t)
       (let* ((info (expand-file-name
                     "objects/info/" (plist-get (magit-git2--repo) :commondir)))
              (single (expand-file-name "commit-graph" info))
              (chain (expand-file-name "commit-graphs/commit-graph-chain" info))
              (raw (magit-git2--hex-to-raw hash)))
         (or (and (file-exists-p single)
                  (magit-git2--graph-contains-p single raw))
             (and (file-exists-p chain)
                  (cl-some (lambda (h)
                             (magit-git2--graph-contains-p
                              (expand-file-name
                               (format "commit-graphs/graph-%s.graph" h) info)
                              raw))
                           (with-temp-buffer
                             (insert-file-contents-literally chain)
                             (split-string (buffer-string) "\n" t))))))))

(magit-git2-define-shape describe-contains
    `("describe" "--contains" ,rev)
  "The tag that contains a commit, when that is certain: no tag, or
exactly one tag, pointing at the commit itself."
  :examples '(("describe" "--contains" "HEAD")
              ("describe" "--contains" "HEAD~")
              ("describe" "--contains" "v1.0"))
  (magit-git2--repo)
  (pcase (magit-git2--resolve rev)
    ('nil (list 0 "" (format "Could not get sha1 for %s. Skipping.\n" rev)))
    (_
     (let* ((hash (or (magit-git2--commit rev) (magit-git2--refuse)))
            (tags (magit-git2--tags))
            (time (plist-get (car (magit-git2--commits
                                   default-directory (list hash) 7 nil))
                             :committer-time)))
       ;; name-rev walks down from the tags, but not to commits older
       ;; than a day before HASH -- unless HASH is in a commit-graph, in
       ;; which case it cuts off by generation number instead.
       (when (magit-git2--in-commit-graph-p hash)
         (magit-git2--refuse))
       (let* ((tips (magit-git2--tips-reaching
                     default-directory hash
                     (delete-dups (mapcar #'cdr tags)) (- time 86400)))
              (containing (cl-remove-if-not
                           (lambda (tag) (member (cdr tag) tips)) tags)))
         (pcase containing
           ('nil
            (magit-git2--die 128 (format "fatal: cannot describe '%s'\n" hash)))
           ;; name-rev prefers the oldest tag among those containing the
           ;; commit, so only a single containing tag is certain.
           (`((,name . ,(pred (equal hash))))
            (let ((annotated (nth 2 (assoc name (magit-git2--refs-list)))))
              (magit-git2--out
               (concat (substring name 10) (and annotated "^0") "\n"))))
           (_ (magit-git2--refuse))))))))

;;; merge-base, rev-list

(magit-git2-define-shape merge-base-is-ancestor
    `("merge-base" "--is-ancestor" ,a ,b)
  "Exit 0 when the first commit is an ancestor of the second, else 1."
  :examples '(("merge-base" "--is-ancestor" "HEAD" "origin/main")
              ("merge-base" "--is-ancestor" "HEAD~" "HEAD")
              ("merge-base" "--is-ancestor" "HEAD" "HEAD")
              ("merge-base" "--is-ancestor" "v1.0" "main"))
  (magit-git2--repo)
  (let ((ha (magit-git2--commit a))
        (hb (magit-git2--commit b)))
    (cond ((null ha)
           (if (magit-git2--resolve a)
               (magit-git2--refuse)
             (magit-git2--die
              128 (format "fatal: Not a valid object name %s\n" a))))
          ((null hb)
           (if (magit-git2--resolve b)
               (magit-git2--refuse)
             (magit-git2--die
              128 (format "fatal: Not a valid object name %s\n" b))))
          ((or (equal ha hb)
               (magit-git2--descendant-p default-directory hb ha))
           (list 0 "" ""))
          (t (list 1 "" "")))))

(magit-git2-define-shape merge-base
    `("merge-base" ,(and a (pred (string-match-p "\\`[^-]"))) ,b)
  "A best common ancestor of two commits."
  :examples '(("merge-base" "HEAD" "origin/main")
              ("merge-base" "feature" "main"))
  (magit-git2--repo)
  (let ((ha (or (magit-git2--commit a) (magit-git2--refuse)))
        (hb (or (magit-git2--commit b) (magit-git2--refuse))))
    ;; With several best ancestors git and libgit2 may pick another.
    (if-let* ((base (magit-git2--merge-base default-directory ha hb)))
        (magit-git2--out (concat base "\n"))
      (magit-git2--die 1 ""))))

(magit-git2-define-shape rev-list-count-left-right
    `("rev-list" "--count" "--left-right"
      ,(and range
            (pred (string-match "\\`\\([^.]+\\)\\.\\.\\.\\([^.]+\\)\\'"))))
  "The commits only on the left and only on the right of A...B."
  :examples '(("rev-list" "--count" "--left-right" "HEAD...origin/main")
              ("rev-list" "--count" "--left-right" "main...feature")
              ("rev-list" "--count" "--left-right" "HEAD...HEAD"))
  (string-match "\\`\\([^.]+\\)\\.\\.\\.\\([^.]+\\)\\'" range)
  (let ((a (match-string 1 range))
        (b (match-string 2 range)))
    (magit-git2--repo)
    (let ((ha (or (magit-git2--commit a) (magit-git2--refuse)))
          (hb (or (magit-git2--commit b) (magit-git2--refuse))))
      (pcase (magit-git2--ahead-behind default-directory ha hb)
        (`(,ahead . ,behind)
         (magit-git2--out (format "%d\t%d\n" ahead behind)))))))

;;; for-each-ref

(defun magit-git2--parse-ref-format (format)
  "Parse the for-each-ref FORMAT into strings and field symbols, or refuse."
  (let ((i 0) (n (length format)) (literal "") tokens)
    (while (< i n)
      (let ((rest (substring format i)))
        (cond
         ((string-match "\\`%%" rest)
          (setq literal (concat literal "%") i (+ i 2)))
         ((string-match "\\`%\\([0-7][[:xdigit:]]\\)" rest)
          (setq literal (concat literal (string (string-to-number
                                                (match-string 1 rest) 16)))
                i (+ i 3)))
         ((string-match "\\`%(\\([a-zA-Z:]+\\))" rest)
          (let ((field (match-string 1 rest)))
            (unless (member field '("refname" "refname:short" "symref"
                                    "symref:short" "objectname"
                                    "objectname:short" "upstream"
                                    "upstream:short" "HEAD"))
              (magit-git2--refuse))
            (unless (string-empty-p literal) (push literal tokens))
            (setq literal "")
            (push (intern field) tokens)
            (setq i (+ i (length (match-string 0 rest))))))
         ((eq (aref rest 0) ?%) (magit-git2--refuse))
         (t (setq literal (concat literal (substring rest 0 1)) i (1+ i))))))
    (unless (string-empty-p literal) (push literal tokens))
    (nreverse tokens)))

(defun magit-git2--ref-matches-p (refname patterns)
  "Whether REFNAME matches one of the for-each-ref PATTERNS (no globs)."
  (or (null patterns)
      (cl-some (lambda (p)
                 (and (string-prefix-p p refname)
                      (or (= (length p) (length refname))
                          (eq (aref refname (length p)) ?/)
                          (string-suffix-p "/" p))))
               patterns)))

(magit-git2-define-shape for-each-ref
    `("for-each-ref" . ,args)
  "Refs in a --format of the fields Magit uses, sorted by name."
  :examples
  (cl-loop for format in '("%(refname)" "%(refname:short)\t%(upstream:short)"
                           "%(symref)\f%(refname)"
                           "%(HEAD)%00%(refname:short)%00%(refname)%00\
%(upstream:short)%00%(upstream)%00"
                           "%(objectname) %(objectname:short) %(symref:short)"
                           "%%%(refname)%25")
           append `(("for-each-ref" ,(concat "--format=" format))
                    ("for-each-ref" ,(concat "--format=" format)
                     "refs/heads" "refs/remotes")
                    ("for-each-ref" ,(concat "--format=" format)
                     "--sort=-refname" "refs/tags")
                    ("for-each-ref" "refs/heads"
                     ,(concat "--format=" format))))
  (let (format patterns (reverse nil))
    (dolist (a args)
      (cond ((string-prefix-p "--format=" a) (setq format (substring a 9)))
            ((equal a "--sort=refname"))
            ((equal a "--sort=-refname") (setq reverse t))
            ((string-prefix-p "-" a) (magit-git2--refuse))
            ((string-match-p "[*?[\\]" a) (magit-git2--refuse))
            (t (push a patterns))))
    (unless format (magit-git2--refuse))
    (let* ((tokens (magit-git2--parse-ref-format format))
           (strict (magit-git2--config-bool "core.warnAmbiguousRefs" t))
           (symref (plist-get (magit-git2--head-info) :symref))
           (refs (cl-remove-if-not
                  (lambda (r) (magit-git2--ref-matches-p (car r) patterns))
                  (magit-git2--refs-list))))
      (magit-git2--repo)
      (when reverse (setq refs (reverse refs)))
      (magit-git2--out
       (mapconcat
        (pcase-lambda (`(,name ,hash ,_ ,target))
          (concat
           (mapconcat
            (lambda (tok)
              (pcase tok
                ((pred stringp) tok)
                ('refname name)
                ('refname:short (magit-git2--shorten-ref name strict))
                ('symref (or target ""))
                ('symref:short
                 (if target (magit-git2--shorten-ref target strict) ""))
                ('objectname hash)
                ('objectname:short (magit-git2--short hash))
                ('HEAD (if (equal symref name) "*" " "))
                (_ (let ((up (and (string-prefix-p "refs/heads/" name)
                                  (magit-git2--upstream
                                   default-directory name))))
                     (cond ((null up) "")
                           ((eq tok 'upstream) up)
                           (t (magit-git2--shorten-ref up strict)))))))
            tokens)
           "\n"))
        refs)))))

;;; Pretty formats

(defvar magit-git2--format-cache (make-hash-table :test #'equal)
  "FORMAT -> parsed format, or `unsupported'.")

(defun magit-git2--parse-format (format)
  "Parse the pretty FORMAT into strings and placeholder keywords."
  (with-memoization (gethash format magit-git2--format-cache)
    (let ((i 0) (n (length format)) (literal "") tokens)
      (catch 'unsupported
        (while (< i n)
          (let ((ch (aref format i)))
            (if (/= ch ?%)
                (setq literal (concat literal (string ch)) i (1+ i))
              (let* ((rest (substring format (1+ i)))
                     (token
                      (cond
                       ((string-match "\\`x\\([0-7][[:xdigit:]]\\)" rest)
                        (cons (string (string-to-number (match-string 1 rest)
                                                        16))
                              3))
                       ((string-match "\\`[n%]" rest)
                        (cons (if (eq (aref rest 0) ?n) "\n" "%") 1))
                       ((string-match "\\`[HhPpsbdD]" rest)
                        (cons (intern (concat ":" (match-string 0 rest))) 1))
                       ((string-match "\\`[ac][nNeEt]\\|\\`g[dDsnNeE]" rest)
                        (cons (intern (concat ":" (match-string 0 rest))) 2))
                       (t (throw 'unsupported 'unsupported)))))
                (if (stringp (car token))
                    (setq literal (concat literal (car token)))
                  (unless (string-empty-p literal) (push literal tokens))
                  (setq literal "")
                  (push (car token) tokens))
                (setq i (+ i 1 (cdr token)))))))
        (unless (string-empty-p literal) (push literal tokens))
        (nreverse tokens)))))

(defun magit-git2--blank-line-end (message pos)
  "If the line of MESSAGE at POS is blank, its end (after the newline)."
  (let ((eol (string-search "\n" message pos)))
    (and (< pos (length message))
         (string-match-p "\\`[ \t\r]*\\'"
                         (substring message pos (or eol (length message))))
         (if eol (1+ eol) (length message)))))

(defun magit-git2--message-parts (message)
  "(SUBJECT . BODY) of MESSAGE, as git's %s and %b."
  (let ((pos 0) (len (length message)) lines next)
    (while (setq next (magit-git2--blank-line-end message pos))
      (setq pos next))
    (while (and (< pos len) (not (magit-git2--blank-line-end message pos)))
      (let ((eol (or (string-search "\n" message pos) len)))
        (push (string-trim-right (substring message pos eol) "[ \t\r]+")
              lines)
        (setq pos (min len (1+ eol)))))
    (while (setq next (magit-git2--blank-line-end message pos))
      (setq pos next))
    (cons (string-join (nreverse lines) " ") (substring message pos))))

(defconst magit-git2--decoration-namespaces
  '(("HEAD" . head) ("refs/heads/" . local) ("refs/tags/" . tag)
    ("refs/remotes/" . remote) ("refs/stash" . stash))
  "The refs git decorates by default and their decoration types.")

(defun magit-git2--decoration-type (refname)
  "The decoration type of REFNAME, or nil when git does not decorate it."
  (cl-loop for (prefix . type) in magit-git2--decoration-namespaces
           when (if (string-suffix-p "/" prefix)
                    (string-prefix-p prefix refname)
                  (or (equal refname prefix)
                      (string-prefix-p (concat prefix "/") refname)))
           return type))

(defun magit-git2--decorations ()
  "HASH -> ((TYPE . REFNAME) ...) in git's order (load_ref_decorations)."
  (magit-git2--memoize 'decorations
    (when (or (magit-git2--config-values "log.excludeDecoration")
              (magit-git2--config-values "log.initialDecorationSet"))
      (magit-git2--refuse))
    (let ((table (make-hash-table :test #'equal))
          (head (magit-git2--head-info)))
      (cl-flet ((add (hash type name)
                  (push (cons type name) (gethash hash table))))
        (pcase-dolist (`(,name ,hash ,peeled ,_) (magit-git2--refs-list))
          (when-let* ((type (magit-git2--decoration-type name)))
            (add hash type name)
            (when peeled (add peeled 'tag name))))
        (when-let* ((hash (plist-get head :hash)))
          (add hash 'head "HEAD")))
      table)))

(defun magit-git2--format-decorations (hash full prefix suffix)
  "The decorations of HASH as git's format_decorations prints them."
  (let* ((decos (gethash hash (magit-git2--decorations)))
         (symref (plist-get (magit-git2--head-info) :symref))
         (current (and (assq 'head decos) symref
                       (string-prefix-p "refs/" symref)
                       (cl-find-if (lambda (d) (and (eq (car d) 'local)
                                                    (equal (cdr d) symref)))
                                   decos)))
         (show (lambda (d)
                 (let ((name (cdr d)))
                   (if full
                       name
                     (replace-regexp-in-string
                      "\\`refs/\\(?:heads\\|tags\\|remotes\\)/" "" name)))))
         (parts (cl-loop for d in decos
                         unless (eq d current)
                         collect (concat
                                  (and (eq (car d) 'tag) "tag: ")
                                  (funcall show d)
                                  (and current (eq (car d) 'head)
                                       (concat " -> "
                                               (funcall show current)))))))
    (if parts (concat prefix (string-join parts ", ") suffix) "")))

(defun magit-git2--format-commit (tokens commit ctx)
  "Expand the parsed format TOKENS for COMMIT (a module plist).
CTX is a plist: :decorate (full, short, no or nil), :reflog (NEW
OLD NAME EMAIL TIME OFFSET MESSAGE SELECTOR SHORT-SELECTOR)."
  (let (parts)
    (cl-flet ((person (role mapped field)
                (plist-get commit
                           (intern (format ":%s%s-%s"
                                           (if mapped "mailmap-" "")
                                           role field))))
              (refuse () (magit-git2--refuse)))
      (dolist (tok tokens)
        (push
         (if (stringp tok)
             tok
           (pcase tok
             (:H (plist-get commit :hash))
             (:h (plist-get commit :short))
             (:P (string-join (plist-get commit :parents) " "))
             (:p (mapconcat #'magit-git2--short (plist-get commit :parents)
                            " "))
             (:s (car (magit-git2--message-parts
                       (plist-get commit :message))))
             (:b (cdr (magit-git2--message-parts
                       (plist-get commit :message))))
             ((or :d :D)
              (let ((decorate (plist-get ctx :decorate)))
                (cond ((null decorate) (refuse))
                      ((eq decorate 'no) "")
                      (t (magit-git2--format-decorations
                          (plist-get commit :hash) (eq decorate 'full)
                          (if (eq tok :d) " (" "")
                          (if (eq tok :d) ")" ""))))))
             ;; The mailmap applies to %aN and %aE only, --use-mailmap
             ;; or not.
             (:an (person "author" nil "name"))
             (:ae (person "author" nil "email"))
             (:aN (person "author" t "name"))
             (:aE (person "author" t "email"))
             (:at (number-to-string (plist-get commit :author-time)))
             (:cn (person "committer" nil "name"))
             (:ce (person "committer" nil "email"))
             (:cN (person "committer" t "name"))
             (:cE (person "committer" t "email"))
             (:ct (number-to-string (plist-get commit :committer-time)))
             (_ (let ((reflog (or (plist-get ctx :reflog) (refuse))))
                  (pcase tok
                    (:gd (nth 8 reflog))
                    (:gD (nth 7 reflog))
                    (:gs (or (nth 6 reflog) ""))
                    (:gn (nth 2 reflog))
                    (:ge (nth 3 reflog))
                    (_ (refuse)))))))
         parts))
      (apply #'concat (nreverse parts)))))

(defun magit-git2--check-encodings (commits)
  "Refuse when git would re-encode the messages of COMMITS."
  (let ((out (car (last (magit-git2--config-values
                         "i18n.logOutputEncoding")))))
    (unless (or (null out) (string-match-p "\\`utf-?8\\'" (downcase out)))
      (magit-git2--refuse)))
  (dolist (c commits)
    (when-let* ((enc (plist-get c :encoding)))
      (unless (string-match-p "\\`utf-?8\\'" (downcase enc))
        (magit-git2--refuse)))))

(defun magit-git2--needs-mailmap (tokens)
  "Whether TOKENS need mailmapped identities."
  (cl-some (lambda (tok) (memq tok '(:aN :aE :cN :cE))) tokens))

(defun magit-git2--history-ok ()
  "Refuse when libgit2 would see another history than git."
  (magit-git2--memoize 'history-ok
    (let ((info (magit-git2--repo)))
      (when (or (plist-get info :shallow)
                (file-exists-p (expand-file-name
                                "info/grafts" (plist-get info :commondir)))
                (magit-git2--replace-refs-p))
        (magit-git2--refuse)))
    t))

;;; log

(defun magit-git2--log-starts (revs)
  "The walk start points ((HASH . UNINTERESTING) ...) for REVS."
  (let (starts)
    (dolist (rev revs)
      (cond
       ((string-match-p "\\.\\.\\." rev) (magit-git2--refuse))
       ((string-match "\\`\\(.*\\)\\.\\.\\(.*\\)\\'" rev)
        (let ((a (match-string 1 rev))
              (b (match-string 2 rev)))
          (push (cons (or (magit-git2--commit (if (equal a "") "HEAD" a))
                          (magit-git2--refuse))
                      t)
                starts)
          (push (cons (or (magit-git2--commit (if (equal b "") "HEAD" b))
                          (magit-git2--refuse))
                      nil)
                starts)))
       ((string-prefix-p "^" rev)
        (push (cons (or (magit-git2--commit (substring rev 1))
                        (magit-git2--refuse))
                    t)
              starts))
       (t (push (cons (or (magit-git2--commit rev) (magit-git2--refuse)) nil)
                starts))))
    (nreverse starts)))

(cl-defun magit-git2--log-command (args)
  "Emulate `git log ARGS' with an explicit format, or refuse."
  (let (format max no-walk decorate revs dashdash)
    (while args
      (let ((a (pop args)))
        (cond
         (dashdash (magit-git2--refuse))
         ((string-match "\\`--\\(?:format=\\|pretty=tformat:\\)\\(.*\\)\\'" a)
          (setq format (match-string 1 a)))
         ((string-match "\\`\\(?:-n\\|--max-count=\\)\\([0-9]+\\)\\'" a)
          (setq max (string-to-number (match-string 1 a))))
         ((equal a "--no-walk") (setq no-walk t))
         ((string-match "\\`--decorate=\\(full\\|short\\|no\\)\\'" a)
          (setq decorate (intern (match-string 1 a))))
         ((member a '("--use-mailmap" "--no-use-mailmap")))
         ((equal a "--no-prefix"))
         ((equal a "--") (setq dashdash t))
         ((string-prefix-p "-" a) (magit-git2--refuse))
         (t (push a revs)))))
    (when (or (not dashdash) (member format '(nil "")))
      (magit-git2--refuse))
    (let ((tokens (magit-git2--parse-format format)))
      (when (eq tokens 'unsupported) (magit-git2--refuse))
      (magit-git2--repo)
      (magit-git2--history-ok)
      (let* ((head (magit-git2--head-info))
             (starts
              (if revs
                  (magit-git2--log-starts (nreverse revs))
                (if-let* ((hash (plist-get head :hash)))
                    (list (cons hash nil))
                  (let ((symref (plist-get head :symref)))
                    (if (and symref (string-prefix-p "refs/heads/" symref))
                        (cl-return-from magit-git2--log-command
                          (magit-git2--die
                           128 (format "fatal: your current branch '%s' %s\n"
                                       (substring symref 11)
                                       "does not have any commits yet")))
                      (magit-git2--refuse))))))
             (commits (magit-git2--log default-directory starts max no-walk
                                       (magit-git2--abbrev)
                                       (magit-git2--needs-mailmap tokens)))
             (ctx (list :decorate decorate)))
        (magit-git2--check-encodings commits)
        (magit-git2--out
         (mapconcat (lambda (c)
                      (concat (magit-git2--format-commit tokens c ctx) "\n"))
                    commits))))))

(defconst magit-git2--example-formats
  '("%h%x0c%D%x0c%x0c%aN%x0c%at%x0c%x0c%s" "%h %s" "%s" "%H%x00%h%x00%s"
    "%h%x00%D%x00%s" "%aN%x00%at%x00%s" "%aN <%ae>" "%H %P" "%b"
    "%h%d %an %ae %cn %ce %ct" "%x25%n%%")
  "Pretty formats the differential test tries.")

(magit-git2-define-shape log
    `("log" . ,args)
  "`git log' with an explicit --format, revisions and `--', no paths."
  :examples
  (append
   (cl-loop for format in magit-git2--example-formats
            collect `("log" ,(concat "--format=" format) "--decorate=full"
                      "-n10" "--use-mailmap" "--no-prefix" "--"))
   '(("log" "--format=%h%x0c%D%x0c%x0c%aN%x0c%at%x0c%x0c%s" "--decorate=full"
      "-n256" "--use-mailmap" "--no-prefix" "..@{upstream}" "--")
     ("log" "--format=%h%x0c%D%x0c%x0c%aN%x0c%at%x0c%x0c%s" "--decorate=full"
      "-n256" "--use-mailmap" "--no-prefix" "@{upstream}.." "--")
     ("log" "--format=%h%x0c%D%x0c%x0c%aN%x0c%at%x0c%x0c%s" "--decorate=full"
      "-n256" "--use-mailmap" "--no-prefix" "origin/feature.." "--")
     ("log" "--format=%h %D" "--decorate=short" "--all-match" "--")
     ("log" "--format=%h%d" "--decorate=short" "main" "feature" "--")
     ("log" "--format=%h%d" "--decorate=full" "feature" "^v1.0" "--")
     ("log" "--format=%h %s" "-n3" "cp1251" "--")
     ("log" "--format=%an %aN" "--no-use-mailmap" "-n3" "--")
     ("log" "--format=%an %aN" "-n3" "--")
     ("log" "--no-walk" "--format=%h %s" "HEAD^{commit}" "--")
     ("log" "--no-walk" "--format=%s" "origin/main^{commit}" "--")
     ("log" "--no-walk" "--format=%s" "nope^{commit}" "--")
     ("log" "--format=%h %p %ct" "-n2000" "--")
     ("log" "--format=%h%d" "--decorate=full" "-n300" "HEAD~50..HEAD" "--")
     ("log" "--format=%h" "-n100" "HEAD~30" "^HEAD~60" "--")
     ("log" "--format=%H" "-n0" "--")
     ("log" "--format=%H" "--max-count=2" "v1.0" "--")))
  (magit-git2--log-command args))

;;; reflog

(magit-git2-define-shape reflog
    `("reflog" ,(and f (pred (string-prefix-p "--format="))) ,ref)
  "A reflog in a --format, of a ref given by its full name."
  :examples '(("reflog" "--format=%gd%x00%aN%x00%at%x00%gs" "refs/stash")
              ("reflog" "--format=%h%x00%aN%x00%gd%x00%gs" "refs/stash")
              ("reflog" "--format=%gD %gs %gn" "refs/heads/main"))
  (unless (string-prefix-p "refs/" ref) (magit-git2--refuse))
  (let ((tokens (magit-git2--parse-format (substring f 9))))
    (when (eq tokens 'unsupported) (magit-git2--refuse))
    (magit-git2--repo)
    (magit-git2--history-ok)
    (let ((entries (magit-git2--reflog default-directory ref)))
      (cond
       ((and (null entries) (not (magit-git2--ref-exists-p ref)))
        (magit-git2--die
         128 (format "fatal: ambiguous argument '%s': %s\n%s\n%s\n" ref
                     "unknown revision or path not in the working tree."
                     "Use '--' to separate paths from revisions, like this:"
                     "'git <command> [<revision>...] -- [<file>...]'")))
       ((null entries) (magit-git2--refuse))
       (t
        (let* ((short (magit-git2--shorten-ref ref nil))
               (commits (magit-git2--commits
                         default-directory (mapcar #'car entries)
                         (magit-git2--abbrev)
                         (magit-git2--needs-mailmap tokens))))
          (when (cl-some (lambda (tok) (memq tok '(:d :D :gN :gE))) tokens)
            (magit-git2--refuse))
          (magit-git2--check-encodings commits)
          (magit-git2--out
           (cl-loop for entry in entries
                    for commit in commits
                    for i from 0
                    concat (concat
                            (magit-git2--format-commit
                             tokens commit
                             (list :reflog (append
                                            entry
                                            (list (format "%s@{%d}" ref i)
                                                  (format "%s@{%d}" short i)))))
                            "\n")))))))))

;;; Dispatch from Magit

(defun magit-git2--strip-global-arguments (args)
  "ARGS without a leading copy of `magit-git-global-arguments'.
`magit-git-string-ng' passes the global arguments again."
  (let ((global magit-git-global-arguments))
    (if (and global (equal (seq-take args (length global)) global))
        (nthcdr (length global) args)
      args)))

(defun magit-git2--destination-ok-p (destination)
  "Whether DESTINATION is one of the `call-process' shapes Magit uses."
  (pcase destination
    ((or 'nil 't `(t nil) `(t ,(pred stringp))) t)))

(defun magit-git2--usable-p (destination)
  "Whether a call with DESTINATION may be emulated at all."
  (and (magit-git2-core-available-p)
       magit-git2--system-config-ok
       (not (file-remote-p default-directory))
       (file-directory-p default-directory)
       (magit-git2--destination-ok-p destination)
       (magit-git2--environment-ok-p)))

(defun magit-git2--log-error (args err)
  "Log the unexpected error ERR for ARGS and turn the emulation off."
  (cl-incf magit-git2--auto-disables)
  (setq magit-git2-mode 'off)
  (with-current-buffer (get-buffer-create magit-git2-log-buffer)
    (goto-char (point-max))
    (insert (format-time-string "%F %T ") (abbreviate-file-name
                                          default-directory)
            "\n  " (mapconcat #'shell-quote-argument args " ")
            "\n  " (error-message-string err) "\n"))
  (message "magit-git2: error in the emulation, turned it off (see %s)"
           magit-git2-log-buffer))

(defun magit-git2--emulate (args destination)
  "The emulated (EXIT STDOUT STDERR) for local ARGS, or nil.
Nil also when DESTINATION needs a stderr the emulation cannot give."
  (let* ((args (magit-git2--strip-global-arguments args))
         (command (or (car args) ""))
         (result
          (condition-case err
              (prog1 (magit-git2--dispatch args)
                (unless (bound-and-true-p magit--refresh-cache)
                  (magit-git2--release)))
            (magit-git2-error
             (magit-git2--count command 'libgit2-error)
             nil)
            (error (magit-git2--log-error args err) nil))))
    (when (and result
               (eq (nth 2 result) 'unknown)
               (or (eq destination t)
                   (and (consp destination) (stringp (cadr destination)))))
      (setq result nil))
    (when (and result (eq destination t)
               (not (string-empty-p (nth 1 result)))
               (not (string-empty-p (nth 2 result))))
      ;; Where git's stderr lands in its stdout is not certain.
      (setq result nil))
    result))

(defun magit-git2--redundant-p (args destination)
  "Whether git need not run for the local ARGS and DESTINATION at all.
That is the `update-index --refresh' a status refresh starts with: Magit
ignores its exit code, and the `git diff' of that refresh looks past
stale stat data and writes the index back.  It does not with
`--no-optional-locks' or with `diff.autoRefreshIndex' off."
  (and (null destination)
       (equal args '("update-index" "--refresh"))
       (bound-and-true-p magit--refreshing-buffer-p)
       (not (member "--no-optional-locks" magit-git-global-arguments))
       (condition-case nil
           (magit-git2--with-memo
            (lambda ()
              (catch 'magit-git2--refuse
                (magit-git2--repo)
                (magit-git2--config-bool "diff.autoRefreshIndex" t))))
         (magit-git2-error nil))))

(defun magit-git2--deliver (result destination)
  "Put RESULT where `call-process' DESTINATION says; return the exit code."
  (pcase-let ((`(,exit ,out ,err) result))
    (pcase destination
      ('t (insert out (if (stringp err) err "")))
      (`(t nil) (insert out))
      (`(t ,(and (pred stringp) file))
       (let ((coding-system-for-write 'utf-8-unix))
         (write-region (if (stringp err) err "") nil file nil 'silent))
       (insert out)))
    exit))

(defun magit-git2--record-mismatch (args expected actual)
  "Record that the emulation gave ACTUAL where git gave EXPECTED."
  (with-current-buffer (get-buffer-create magit-git2-mismatches-buffer)
    (goto-char (point-max))
    (insert (format-time-string "* %F %T ")
            (abbreviate-file-name default-directory) " $ git "
            (mapconcat #'shell-quote-argument args " ") "\n"
            (format "  shape: %s\n" magit-git2--shape)
            (format "  git:      %S\n" expected)
            (format "  emulated: %S\n" actual))
    (let ((a (split-string (nth 1 expected) "\n"))
          (b (split-string (nth 1 actual) "\n"))
          (line 1))
      (while (and a b (equal (car a) (car b)))
        (setq a (cdr a) b (cdr b) line (1+ line)))
      (when (or a b)
        (insert (format "  first difference at line %d:\n  - %S\n  + %S\n"
                        line (car a) (car b))))))
  (unless magit-git2--mismatch-reported
    (setq magit-git2--mismatch-reported t)
    (message "magit-git2: the emulation differs from git, see %s"
             magit-git2-mismatches-buffer)))

(defun magit-git2--shadow (orig destination args)
  "Call ORIG with DESTINATION and ARGS; compare with the emulation."
  (let* ((flat (flatten-tree args))
         (emulated (condition-case err
                       (and (magit-git2--usable-p destination)
                            (magit-git2--emulate flat destination))
                     (error (magit-git2--log-error flat err) nil)))
         (start (and (not (null destination)) (point)))
         (file (and (consp destination) (stringp (cadr destination))
                    (cadr destination)))
         (exit (apply orig destination args)))
    (unless emulated
      (magit-git2--count (or (car flat) "") 'fell-through))
    (when emulated
      (condition-case err
          (let* ((stdout (if start
                             (buffer-substring-no-properties start (point))
                           ""))
                 (stderr (and file (file-exists-p file)
                              (with-temp-buffer
                                (let ((coding-system-for-read 'utf-8-unix))
                                  (insert-file-contents file))
                                (buffer-string))))
                 (mine (pcase destination
                         ('nil (list (nth 0 emulated) "" nil))
                         ('t (list (nth 0 emulated)
                                   (concat (nth 1 emulated)
                                           (if (stringp (nth 2 emulated))
                                               (nth 2 emulated) ""))
                                   nil))
                         (`(t nil)
                          (list (nth 0 emulated) (nth 1 emulated) nil))
                         (_ (list (nth 0 emulated) (nth 1 emulated)
                                  (nth 2 emulated)))))
                 (theirs (list exit stdout stderr)))
            (if (equal mine theirs)
                (magit-git2--count (car flat) 'emulated)
              (magit-git2--count (car flat) 'mismatched)
              (magit-git2--record-mismatch flat theirs mine)))
        (error (magit-git2--log-error flat err))))
    exit))

;;; Commands started ahead

;; What a status refresh still needs of git takes three processes, nearly
;; all of it their start-up, and Magit waits for one after the other.
;; The commands rarely change, so a refresh starts those of the last one
;; all at once and takes their output when Magit asks for it.

(defvar magit-git2--ahead-commands nil
  "The `status' and `diff' commands of the last status refresh.
A list of (GLOBAL-ARGS . LOCAL-ARGS).")

(defvar magit-git2--ahead-processes nil
  "The commands running ahead of Magit: ((KEY . PROCESS) ...).
KEY is (DIRECTORY GLOBAL-ARGS . LOCAL-ARGS).")

(defvar magit-git2--status-refresh nil
  "The `magit--refresh-start-time' of the latest status refresh.")

(defvar magit-git2--finished-processes nil
  "The processes `magit-git2--free-finished' is to free.")

(defun magit-git2--start-process (command)
  "Start COMMAND, a (GLOBAL-ARGS . LOCAL-ARGS); return the process."
  (let* ((magit-git-global-arguments (car command))
         (process-environment (magit-process-environment))
         (coding (magit--process-coding-system))
         (errors (make-pipe-process
                  :name "magit-git2 stderr"
                  :buffer (generate-new-buffer " *magit-git2 stderr*" t)
                  :coding coding
                  :noquery t
                  :sentinel #'ignore))
         (process (make-process
                   :name "magit-git2"
                   :buffer (generate-new-buffer " *magit-git2 stdout*" t)
                   :stderr errors
                   :command (cons (magit-git-executable)
                                  (magit-process-git-arguments (cdr command)))
                   :coding coding
                   :connection-type 'pipe
                   :noquery t
                   :sentinel #'ignore)))
    (process-put process 'magit-git2-errors errors)
    process))

(defun magit-git2--free-process (process)
  "Free what the finished PROCESS leaves behind."
  (let* ((errors (process-get process 'magit-git2-errors))
         (buffers (list (process-buffer process) (process-buffer errors))))
    (delete-process errors)
    (delete-process process)
    (mapc #'kill-buffer buffers)))

(defun magit-git2--free-finished ()
  "Free the finished processes."
  (mapc #'magit-git2--free-process
        (prog1 magit-git2--finished-processes
          (setq magit-git2--finished-processes nil))))

(defun magit-git2--free-later (process)
  "Free the finished PROCESS once the refresh is over.
`delete-process' redisplays, which would show the half built buffer."
  (unless magit-git2--finished-processes
    (run-with-timer 0 nil #'magit-git2--free-finished))
  (push process magit-git2--finished-processes))

(defun magit-git2--finish-process (process)
  "Wait for PROCESS and free it later; return (EXIT STDOUT STDERR).
Nil when it did not end by itself."
  (let ((errors (process-get process 'magit-git2-errors)))
    ;; Neither pipe may fill up, and no timer or other process runs
    ;; here, as none does while `call-process' waits.  A process that
    ;; ended long ago still has its output to read.
    (while (or (accept-process-output process 0.005 nil 0)
               (process-live-p process))
      (accept-process-output errors 0 nil 0))
    (while (accept-process-output errors 0 nil 0))
    (prog1 (and (eq (process-status process) 'exit)
                (list (process-exit-status process)
                      (with-current-buffer (process-buffer process)
                        (buffer-string))
                      (with-current-buffer (process-buffer errors)
                        (buffer-string))))
      (magit-git2--free-later process))))

(defun magit-git2--reap (process _event)
  "Have PROCESS freed once it has finished."
  (unless (process-live-p process)
    (magit-git2--free-later process)))

(defun magit-git2--drop-ahead ()
  "Forget the commands started ahead that nothing asked for.
They run to their end: killed, git could leave `index.lock' behind."
  (pcase-dolist (`(,_ . ,process) magit-git2--ahead-processes)
    (if (process-live-p process)
        (set-process-sentinel process #'magit-git2--reap)
      (magit-git2--free-later process)))
  (setq magit-git2--ahead-processes nil))

(defun magit-git2--start-ahead ()
  "Start the commands of the last status refresh."
  (magit-git2--drop-ahead)
  (setq magit-git2--status-refresh
        (bound-and-true-p magit--refresh-start-time))
  (let ((cwd (magit-git2--cwd)))
    (dolist (command (prog1 magit-git2--ahead-commands
                       (setq magit-git2--ahead-commands nil)))
      (push (cons (cons cwd command) (magit-git2--start-process command))
            magit-git2--ahead-processes))))

(defun magit-git2--take-ahead (args)
  "The (EXIT STDOUT STDERR) of the local ARGS started ahead, or nil.
In a status refresh, also remember ARGS for the next one."
  (when (and magit-git2--status-refresh
             (eq magit-git2--status-refresh
                 (bound-and-true-p magit--refresh-start-time))
             (member (car args) '("status" "diff")))
    (let* ((command (cons magit-git-global-arguments args))
           (entry (assoc (cons (magit-git2--cwd) command)
                         magit-git2--ahead-processes)))
      (cl-pushnew command magit-git2--ahead-commands :test #'equal)
      (when entry
        (setq magit-git2--ahead-processes
              (delq entry magit-git2--ahead-processes))
        (magit-git2--finish-process (cdr entry))))))

(defun magit-git2--answer (args destination)
  "Answer the local git ARGS for DESTINATION; return the exit code.
Nil when git has to run."
  (when (magit-git2--usable-p destination)
    (cond ((magit-git2--redundant-p args destination)
           (magit-git2--count (car args) 'skipped)
           (magit-git2--start-ahead)
           0)
          ((when-let* ((result (magit-git2--emulate args destination)))
             (magit-git2--count (car args) 'emulated)
             (magit-git2--deliver result destination)))
          ((when-let* ((result (magit-git2--take-ahead args)))
             (magit-git2--count (car args) 'ahead)
             (magit-git2--deliver result destination))))))

(defun magit-git2--around (orig destination &rest args)
  "Answer `magit-process-git' (ORIG) with DESTINATION and ARGS."
  (pcase magit-git2-mode
    ('on
     (let ((flat (flatten-tree args)))
       (or (condition-case err
               (magit-git2--answer flat destination)
             (error (magit-git2--log-error flat err) nil))
           (progn
             (magit-git2--count (or (car flat) "") 'fell-through)
             (apply orig destination args)))))
    ('shadow (magit-git2--shadow orig destination args))
    (_ (apply orig destination args))))

(advice-add 'magit-process-git :around #'magit-git2--around
            '((name . magit-git2)))

(with-eval-after-load 'magit-mode
  (add-hook 'magit-unwind-refresh-hook #'magit-git2--release-if-loaded)
  (add-hook 'magit-unwind-refresh-hook #'magit-git2--drop-ahead))

(defun magit-git2--release-if-loaded ()
  "Free the module's repository handles, if the module is loaded."
  (when (magit-git2-core-available-p)
    (magit-git2--release)))

;;; Commands

(defun magit-git2-mode (&optional mode)
  "Cycle `magit-git2-mode' through off, shadow and on, or set it to MODE."
  (interactive)
  (setq magit-git2-mode
        (or mode (pcase magit-git2-mode
                   ('off 'shadow)
                   ('shadow 'on)
                   (_ 'off))))
  (when (and (not (eq magit-git2-mode 'off))
             (not (magit-git2-core-available-p)))
    (setq magit-git2-mode 'off)
    (user-error "magit-git2: the module is not loaded: %s"
                magit-git2-core-error))
  (message "magit-git2: %s" magit-git2-mode)
  magit-git2-mode)

(defun magit-git2-show-mismatches ()
  "Show the differences shadow mode found."
  (interactive)
  (display-buffer (get-buffer-create magit-git2-mismatches-buffer)))

(defun magit-git2-stats ()
  "Show how many git calls were emulated, by command."
  (interactive)
  (let* ((alist (magit-git2-stats-alist))
         (total (lambda (slot)
                  (cl-loop for (_ . counters) in (alist-get 'commands alist)
                           sum (alist-get slot counters)))))
    (with-current-buffer (get-buffer-create "*magit-git2-stats*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "mode: %s   auto-disables: %d\n\n"
                        (alist-get 'mode alist)
                        (alist-get 'auto-disables alist))
                (format "%-16s %9s %12s %10s %8s %8s %6s\n" "command" "emulated"
                        "fell-through" "mismatched" "libgit2" "skipped"
                        "ahead"))
        (pcase-dolist (`(,command . ,c) (alist-get 'commands alist))
          (insert (format "%-16s %9d %12d %10d %8d %8d %6d\n" command
                          (alist-get 'emulated c) (alist-get 'fell-through c)
                          (alist-get 'mismatched c)
                          (alist-get 'libgit2-error c)
                          (alist-get 'skipped c)
                          (alist-get 'ahead c)))))
      (display-buffer (current-buffer)))
    (message "magit-git2: %d emulated, %d fell through, %d mismatched"
             (funcall total 'emulated) (funcall total 'fell-through)
             (funcall total 'mismatched))))

(defun magit-git2-reload-module ()
  "Load the module again, after rebuilding it."
  (interactive)
  (if (magit-git2-core-reload)
      (message "magit-git2: loaded %s" magit-git2-core--loaded-file)
    (user-error "magit-git2: %s" magit-git2-core-error)))

(defconst magit-git2--restored-sections
  '((magit-status-headers-hook magit-insert-tags-header)
    (magit-status-sections-hook
     magit-insert-unpushed-to-pushremote
     magit-insert-unpushed-to-upstream-or-recent
     magit-insert-unpulled-from-pushremote
     magit-insert-unpulled-from-upstream))
  "Sections too costly in git processes on Windows, cheap with the module.")

(defun magit-git2-restore-sections ()
  "Put the sections of `magit-git2--restored-sections' back.
Each goes after the section that precedes it in the standard value
of its hook."
  (require 'magit-status)
  (pcase-dolist (`(,hook . ,sections) magit-git2--restored-sections)
    (let ((standard (eval (car (get hook 'standard-value)) t)))
      (dolist (section sections)
        (unless (memq section (symbol-value hook))
          ;; The nearest section before it that the hook still has.
          (let ((after (cl-loop for s in (cdr (memq section (reverse standard)))
                                when (memq s (symbol-value hook)) return s)))
            (if after
                (magit-add-section-hook hook section after t)
              (magit-add-section-hook hook section nil t))))))))

(provide 'magit-git2)

;;; magit-git2.el ends here
