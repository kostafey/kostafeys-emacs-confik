;;; eshell-conf.el --- Eshell configuration.  -*- lexical-binding: t -*-

(require 'subr-x)

;;------------------------------------------------------------
;; eshell

;; git without a git process
;;
;; A git process costs ~220 ms on Windows, and the prompt asked `git branch'
;; for one after every command, in a repository or not.  magit-git2's module
;; (see `version-control.el') reads the repository in-process in ~1 ms.  It
;; is loaded with Magit; here it is loaded on first use instead, from the
;; file Magit would load it from -- no download, that stays with Magit.
;; Without the module, and in remote directories, git runs as before.

(defvar k/eshell-detached-head-char "D:"
  "Prefix of the commit shown in the prompt for a detached HEAD.")

(defvar k/git2-load-tried nil
  "Non-nil once loading magit-git2's module was attempted.")

(declare-function magit-git2-core-load "magit-git2-core" (&optional file))
(declare-function magit-git2--head "magit-git2-core-module" (dir))
(declare-function magit-git2--refs "magit-git2-core-module" (dir))
(declare-function magit-git2--repo-info "magit-git2-core-module" (dir))
(declare-function magit-git2--short-id "magit-git2-core-module"
                  (dir hash minlen))

(defun k/git2-usable-p ()
  "Non-nil when magit-git2's module can answer for `default-directory'."
  (and (not (file-remote-p default-directory))
       (or (featurep 'magit-git2-core-module)
           (unless k/git2-load-tried
             (setq k/git2-load-tried t)
             (and (require 'magit-git2-core nil t)
                  (magit-git2-core-load))))))

(defun k/git-branch-by-git ()
  "Return the git branch name, as `git branch' tells it."
  (let ((branch (car (vc-git-branches))))
    (cond
     ((null branch) nil)
     ((string-match "^(HEAD detached at \\(.+\\))$" branch)
      (concat k/eshell-detached-head-char (match-string 1 branch)))
     (t branch))))

(defun k/git-branch ()
  "Return the git branch name of `default-directory', or nil outside git.
A detached HEAD shows as its abbreviated commit.  An unborn branch, which
`git branch' does not list, shows by its name."
  (if (not (k/git2-usable-p))
      (k/git-branch-by-git)
    (pcase (condition-case nil
               (magit-git2--head default-directory)
             (error 'failed))
      ('failed (k/git-branch-by-git))
      ((and `(:symref ,symref . ,_) (guard symref))
       (string-remove-prefix "refs/heads/" symref))
      (`(:symref nil :hash ,(and hash (pred stringp)))
       (concat k/eshell-detached-head-char
               (or (ignore-errors
                     (magit-git2--short-id default-directory hash 7))
                   (substring hash 0 7)))))))

(defvar k/eshell-lambda-color "#5544EE"
  "Color of the λ prompt mark in eshell.")

(defvar k/eshell-emacs-lambda-color "#ADC6FF"
  "Color of the λ prompt mark while `k/eshell-emacs-mode' is on.")

(defun k/eshell-lambda-face ()
  "Return the face of the λ prompt mark for the mode of the buffer."
  `(:foreground ,(if (bound-and-true-p k/eshell-emacs-mode)
                     k/eshell-emacs-lambda-color
                   k/eshell-lambda-color)
    :weight bold))

(setq eshell-prompt-regexp "^λ "
      eshell-prompt-function
      (lambda ()
        (concat
         (propertize (eshell/pwd) 'face 'font-lock-function-name-face)
         (if-let* ((branch (k/git-branch)))
             (propertize (concat "\n" branch) 'face 'font-lock-builtin-face))
         (propertize "\nλ" 'face (k/eshell-lambda-face))
         (propertize " " 'face 'default))))

(defun eshell-maybe-bol ()
  "Goto end of prompt or beginning of line."
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))

(defalias 'eshell/ff 'find-file)
(defalias 'eshell/fw 'find-file-other-window)

(defun eshell/cls ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/cd.. ()
  (eshell/cd ".."))

(defun k/eshell-handle-cd-args (args)
  "If `cd' argument is filepath, use it's directory path."
  (list (let ((path (car args)))
          (if (file-directory-p path)
              path
            (if (file-exists-p path)
                (file-name-directory path)
              path)))))

(advice-add 'eshell/cd :filter-args #'k/eshell-handle-cd-args)

(defun k/eshell-parse-backslash ()
  "Replace a single backslash (\\) character with slash (/).
Handle space before backslash ( \\) for multiline commands case."
  (when (eq (char-after) ?\\)
    (when (eshell-looking-at-backslash-return (point))
      (if (equal " " (string (char-before)))
	      (throw 'eshell-incomplete ?\\)
        ""))
    (forward-char 1)
    "/"))

(defun k/eshell-load-hook ()
  (add-hook 'eshell-parse-argument-hook 'k/eshell-parse-backslash))

(add-hook 'eshell-prompt-load-hook 'k/eshell-load-hook)

;;------------------------------------------------------------
;; git output in color
;;
;; git colors its output only for a terminal (color.ui=auto), and eshell
;; hands it a pipe, so `git branch' shows no current branch in green and
;; `git diff' no red and green lines -- though eshell renders ANSI colors
;; (`eshell-handle-ansi-color').  A git command typed in eshell is run
;; with -c color.ui=always when its output goes to the eshell buffer --
;; what color.ui=auto does in a terminal.  Piped or redirected, as in
;; `git log | grep ^7087' or `git log > file', it stays plain.  Only the
;; commands typed: an environment variable, such as GIT_CONFIG_COUNT,
;; would reach every git run from the buffer too, those of vc-git,
;; pcmpl-git and Magit, which parse the output (and magit-git2 turns its
;; emulation off under it).  `--no-color' after the subcommand still
;; turns the colors off.

(defvar eshell-in-pipeline-p)
(declare-function eshell-interactive-output-p "esh-io" (&optional index handles))

(defun k/eshell-output-to-buffer-p ()
  "Non-nil when the command being run writes to the eshell buffer.
Not when it is piped to a next command or its output is redirected."
  (and (not (memq eshell-in-pipeline-p '(first t)))
       (eshell-interactive-output-p)))

(defun k/eshell-git-color-args (args)
  "Add -c color.ui=always to ARGS of `eshell-external-command' for git.
Only when git writes to the eshell buffer, see `k/eshell-output-to-buffer-p'."
  (pcase-let ((`(,command ,command-args) args))
    (if (and (string-equal-ignore-case
              (file-name-sans-extension (file-name-nondirectory command)) "git")
             (k/eshell-output-to-buffer-p))
        (list command (append '("-c" "color.ui=always") command-args))
      args)))

(advice-add 'eshell-external-command :filter-args #'k/eshell-git-color-args)

;;------------------------------------------------------------
;; grep, egrep and fgrep: the external grep, as typed
;;
;; em-unix.el defines them as Lisp commands, `eshell/grep' and its kin.
;; Alone, they send the matches to a *grep* buffer; piped into or
;; redirected they run the external command -- but with the -H and -n
;; meant for that buffer, so `git log | grep 1234' prefixes each line
;; with "(standard input):9876:".  Without `eshell/grep' eshell runs
;; grep.exe as it is, everywhere.  egrep and fgrep of Git for Windows
;; are "#!/bin/sh" scripts, which eshell cannot start on Windows, so they
;; become what those scripts do: grep -E and grep -F.  rgrep has no
;; external command and stays.

(declare-function eshell-parse-command "esh-cmd" (command &optional args toplevel))

(with-eval-after-load 'em-unix
  (fmakunbound 'eshell/grep)

  (defun eshell/egrep (&rest args)
    "Run the external grep -E with ARGS."
    (throw 'eshell-replace-command (eshell-parse-command "*grep" (cons "-E" args))))

  (defun eshell/fgrep (&rest args)
    "Run the external grep -F with ARGS."
    (throw 'eshell-replace-command (eshell-parse-command "*grep" (cons "-F" args)))))

;;------------------------------------------------------------
;; Emacs mode: arrows move by lines, as `ghostel-emacs-mode' does
;;
;; `eshell-hist-mode' binds <up> and <down> to the input history, and a
;; minor mode keymap wins over `eshell-mode-map', so the global line moves
;; never reach an eshell buffer.  `k/eshell-emacs-mode' puts its own map in
;; place of the history one for the buffer while it is on: the arrows move
;; by lines again (S-arrows select, as everywhere), and q or C-c C-e goes
;; back to the prompt.  The λ of the last prompt takes the color of the
;; mode, `k/eshell-emacs-lambda-color' or `k/eshell-lambda-color'; the
;; prompt is text already in the buffer, so it is recolored in place.

(defun k/eshell-recolor-lambda ()
  "Give the λ of the last eshell prompt the color of the current mode."
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward eshell-prompt-regexp nil t)
      (with-silent-modifications
        (put-text-property (point) (1+ (point)) 'face (k/eshell-lambda-face))))))

(defvar-keymap k/eshell-emacs-mode-map
  "<up>"     #'k/line-previous
  "<down>"   #'k/line-next
  "<escape>" #'k/eshell-emacs-mode
  "C-g"      #'k/eshell-emacs-mode
  "q"        #'k/eshell-emacs-mode
  "C-c C-e"  #'k/eshell-emacs-mode)

(define-minor-mode k/eshell-emacs-mode
  "Move through an eshell buffer by lines instead of the input history.
Leaving the mode returns point to the end of the buffer, the prompt."
  :lighter " Emacs"
  (setq-local minor-mode-overriding-map-alist
              (delq (assq 'eshell-hist-mode minor-mode-overriding-map-alist)
                    (copy-sequence minor-mode-overriding-map-alist)))
  (k/eshell-recolor-lambda)
  (if k/eshell-emacs-mode
      (push (cons 'eshell-hist-mode k/eshell-emacs-mode-map)
            minor-mode-overriding-map-alist)
    (deactivate-mark)
    (goto-char (point-max))))

(with-eval-after-load 'esh-mode
  (define-key eshell-mode-map (kbd "<home>") #'eshell-maybe-bol)
  (define-key eshell-mode-map (kbd "C-c C-e") #'k/eshell-emacs-mode))

;;------------------------------------------------------------
;; git options completion without the browser
;;
;; `pcomplete/git' (pcmpl-git.el) reads the options of a subcommand from
;; `git help SUBCMD'.  Git for Windows has no man pages, so that opens
;; git-SUBCMD.html in the browser and blocks typing until git returns -
;; corfu triggers it on its own as soon as `git log --' is typed.  Here the
;; options are read from that HTML file instead: each <dt> of it is an
;; option (or an option value), and those matching the ARGUMENT regexp of
;; `pcomplete-from-help' are the candidates.  The page is parsed here and
;; not fed to the original function, which reads its text through
;; `call-process': pcomplete.el is natively compiled, so a temporary
;; redefinition of `call-process' would never reach it.  A subcommand
;; without a page (an alias, say) falls back to the short `git SUBCMD -h'
;; usage.

(defvar k/git-html-path nil
  "Directory of the git HTML pages, as told by `git --html-path'.")

(defun k/git-help-html-options (file argument)
  "Return the options of the git HTML page FILE matching ARGUMENT.
Each <dt> term of the page is matched at its start; the first match
group of ARGUMENT, failing that the entire match, is the option."
  (with-temp-buffer
    (insert-file-contents file)
    (let (options)
      (while (re-search-forward "<dt class=\"hdlist1\">\\(.*\\)</dt>" nil t)
        (let ((term (string-replace
                     "&lt;" "<"
                     (string-replace
                      "&gt;" ">"
                      (string-replace
                       "&amp;" "&"
                       (replace-regexp-in-string
                        "<[^>]*>" "" (match-string 1)))))))
          (when (string-match (concat "\\`\\(?:" argument "\\)") term)
            (push (or (match-string 1 term) (match-string 0 term)) options))))
      (delete-dups (nreverse options)))))

(defun k/pcomplete-from-help-no-browser (orig command &rest args)
  "Call ORIG with COMMAND and ARGS, never letting `git help' open a browser."
  (if-let* (((eq system-type 'windows-nt))
            ((consp command))
            ((equal (cdr command) (list "help" (nth 2 command))))
            (subcmd (nth 2 command))
            ((not (string-prefix-p "-" subcmd))))
      ;; Cached under the key of the `git help' call it replaces.
      (with-memoization (gethash (cons command args) pcomplete-from-help)
        (let ((html (expand-file-name
                     (format "git-%s.html" subcmd)
                     (with-memoization k/git-html-path
                       (or (ignore-errors
                             (car (process-lines (car command) "--html-path")))
                           "")))))
          (if (file-readable-p html)
              (k/git-help-html-options
               html (or (plist-get args :argument)
                        (rx "-" (+ (any "-" alnum)) (? "="))))
            (apply orig (list (car command) subcmd "-h") args))))
    (apply orig command args)))

(advice-add 'pcomplete-from-help :around #'k/pcomplete-from-help-no-browser)

;;------------------------------------------------------------
;; Command name completion from a cache of PATH
;;
;; corfu asks for completions after each pause in typing, several times
;; per pause, and `eshell--complete-commands-list' answers every time by
;; listing all of PATH and testing each file found with `file-directory-p'
;; and `file-executable-p' (`file-readable-p' with `eshell-force-execution'),
;; then dropping duplicates with `member'.  On Windows that is ~50 ms for a
;; prefix such as "gi", and ~2 s for the empty prefix, which the substring
;; and fussy styles ask for when no command starts with what was typed.
;; Here what each directory holds is kept until its modification time
;; changes, that is until a file is added, removed or renamed there.  So a
;; file made executable in place with chmod shows up only once something
;; else changes in its directory.  The rest of the function, and the case
;; of a command typed with a directory, are as in em-cmpl.el.

(defvar eshell-force-execution)

(defvar k/eshell-path-commands-cache (make-hash-table :test #'equal)
  "PATH directory -> (MTIME FORCE COMMANDS SUBDIRECTORIES).
FORCE is `eshell-force-execution' at the time of the listing.")

(defun k/eshell-path-commands (dir cwd-p)
  "Return the commands in the directory DIR, a name with a final slash.
These are its executable files, readable ones with
`eshell-force-execution', and when CWD-P its subdirectories too."
  (when (file-accessible-directory-p dir)
    (let ((mtime (file-attribute-modification-time (file-attributes dir)))
          (entry (gethash dir k/eshell-path-commands-cache)))
      (unless (and entry
                   (equal (nth 0 entry) mtime)
                   (eq (nth 1 entry) eshell-force-execution))
        (let (commands subdirectories)
          (dolist (name (file-name-all-completions "" dir))
            (cond ((string-suffix-p "/" name) (push name subdirectories))
                  ((if eshell-force-execution
                       (file-readable-p (concat dir name))
                     (file-executable-p (concat dir name)))
                   (push name commands))))
          (setq entry (list mtime eshell-force-execution
                            commands subdirectories))
          (puthash dir entry k/eshell-path-commands-cache)))
      (if cwd-p
          (append (nth 3 entry) (nth 2 entry))
        (nth 2 entry)))))

(defvar eshell-explicit-command-char)
(defvar pcomplete-stub)
(defvar eshell-show-lisp-completions)
(defvar eshell-show-lisp-alternatives)
(declare-function eshell-get-path "esh-ext" (&optional literal-p))
(declare-function eshell-alias-completions "em-alias" (name))
(declare-function eshell-winnow-list "esh-util" (entries exclude &optional predicates))
(declare-function eshell-find-alias-function "esh-cmd" (name))

(defvar k/eshell-functions-cache 'unknown
  "Names of the `eshell/' functions without the prefix, or `unknown'.")

(defun k/eshell-functions ()
  "Return the commands defined as `eshell/' functions, but not as aliases.
They are found anew after each command run in eshell: a function
defined meanwhile elsewhere waits for it."
  (when (eq k/eshell-functions-cache 'unknown)
    (setq k/eshell-functions-cache
          (eshell-winnow-list
           (mapcar (lambda (name) (substring name 7))
                   (all-completions "eshell/" obarray #'functionp))
           nil '(eshell-find-alias-function))))
  k/eshell-functions-cache)

(defun k/eshell-functions-cache-clear ()
  "Have `k/eshell-functions' look for the `eshell/' functions again."
  (setq k/eshell-functions-cache 'unknown))

(add-hook 'eshell-pre-command-hook #'k/eshell-functions-cache-clear)

(defun k/eshell-complete-commands-cached (orig)
  "Like ORIG, `eshell--complete-commands-list', over the cached PATH.
The table answers each FILENAME once: corfu keeps it while the command
is typed, and its completion styles ask it the same thing many times.
The `eshell/' functions, a walk of the whole `obarray', are kept in
`k/eshell-functions-cache'."
  (if (file-name-directory (pcomplete-arg))
      (funcall orig)
    (let ((glob-name
           (if (and (> (length pcomplete-stub) 0)
                    (eq (aref pcomplete-stub 0) eshell-explicit-command-char))
               (setq pcomplete-stub (substring pcomplete-stub 1))))
          (answers (make-hash-table :test #'equal)))
      (completion-table-dynamic
       (lambda (filename)
         (with-memoization (gethash filename answers)
           (let* ((cwd (file-name-as-directory
                        (expand-file-name default-directory)))
                  (completions
                   ;; `file-name-all-completions' called with FILENAME, as
                   ;; em-cmpl.el does, ignores case the same way.
                   (let ((completion-ignore-case
                          read-file-name-completion-ignore-case))
                     (all-completions
                      filename
                      (mapcan (lambda (path)
                                (setq path (file-name-as-directory
                                            (expand-file-name (or path "."))))
                                (copy-sequence
                                 (k/eshell-path-commands
                                  path (string-equal path cwd))))
                              (eshell-get-path))))))
             (unless glob-name
               (setq completions
                     (append (if (fboundp 'eshell-alias-completions)
                                 (eshell-alias-completions filename))
                             (all-completions filename (k/eshell-functions))
                             completions))
               (when (or eshell-show-lisp-completions
                         (and eshell-show-lisp-alternatives
                              (null completions)))
                 (setq completions
                       (append (all-completions filename obarray #'functionp)
                               completions))))
             (pcomplete-uniquify-list completions))))))))

(advice-add 'eshell--complete-commands-list
            :around #'k/eshell-complete-commands-cached)

;;------------------------------------------------------------
;; git arguments completion with fewer git processes
;;
;; `pcomplete/git' starts git on each completion request: `git for-each-ref'
;; for the revisions after checkout, log, diff, switch..., and `git
;; ls-files' for the tracked files after add, commit, diff, log...  With
;; corfu that is ~0.5 s per pause in typing `git checkout ma'.  The refs
;; come from magit-git2's module (see `k/git2-usable-p'); the list of
;; tracked files is kept until the next command runs, ten seconds at most.

(defvar vc-git-revision-complete-only-branches)

(defun k/vc-git-revision-table-git2 (orig files)
  "Call ORIG with FILES, `vc-git-revision-table', unless magit-git2 can answer."
  (if-let* (((k/git2-usable-p))
            (refs (ignore-errors
                    (and (magit-git2--repo-info default-directory)
                         (magit-git2--refs default-directory)))))
      (let ((regexp (if vc-git-revision-complete-only-branches
                        "^refs/\\(heads\\|remotes\\)/\\(.*\\)$"
                      "^refs/\\(heads\\|tags\\|remotes\\)/\\(.*\\)$")))
        (cons "HEAD"
              (seq-keep (lambda (refname)
                          (and (string-match regexp refname)
                               (match-string 2 refname)))
                        (sort (mapcar #'car refs) #'string<))))
    (funcall orig files)))

(advice-add 'vc-git-revision-table :around #'k/vc-git-revision-table-git2)

(defvar k/pcmpl-git-tracked-cache nil
  "Alist of ((DIRECTORY . ARGS) TIME . PREDICATE) of `git ls-files' calls.")

(defun k/pcmpl-git-tracked-cached (orig &rest args)
  "Call ORIG with ARGS, `pcmpl-git--tracked-file-predicate', or reuse its answer."
  (let* ((key (cons (expand-file-name default-directory) args))
         (hit (assoc key k/pcmpl-git-tracked-cache)))
    (if (and hit (< (- (float-time) (cadr hit)) 10))
        (cddr hit)
      (let ((predicate (apply orig args)))
        (setf (alist-get key k/pcmpl-git-tracked-cache nil nil #'equal)
              (cons (float-time) predicate))
        predicate))))

(defun k/pcmpl-git-tracked-cache-clear ()
  "Forget the `git ls-files' answers: a command may change the files."
  (setq k/pcmpl-git-tracked-cache nil))

(advice-add 'pcmpl-git--tracked-file-predicate
            :around #'k/pcmpl-git-tracked-cached)
(add-hook 'eshell-pre-command-hook #'k/pcmpl-git-tracked-cache-clear)

(provide 'eshell-conf)

;;; eshell-conf.el ends here
