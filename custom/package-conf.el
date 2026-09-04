;;; package-conf.el --- package.el + package-vc bootstrap -*- lexical-binding: t -*-

;;; Commentary:

;; Replaces `straight-conf'.  Packages are installed by the built-in
;; `package.el' and fetched from git by `package-vc': every `use-package'
;; declaration in this configuration carries a `:vc' recipe naming the
;; upstream repository and branch, the way its `straight-use-package' recipe
;; did.
;;
;; A `:vc' checkout under `package-user-dir' (`~/.emacs.d/elpa/<pkg>-<ver>/')
;; is an ordinary git working tree -- what `straight/repos/<pkg>/' was, minus
;; straight's separate build directory, so there is nothing to relink after
;; an edit:
;;
;;   M-x package-vc-rebuild   regenerate autoloads and `.elc' after a local edit
;;   M-x package-vc-upgrade   `git pull' one package
;;   M-x package-vc-checkout  clone elsewhere, then
;;   M-x package-vc-install-from-checkout   symlink that clone in as the package
;;
;; `package-vc-register-as-project' additionally puts every checkout into
;; `C-x p p'.

;;; Code:

(require 'package)
(require 'package-vc)
(require 'use-package)
(require 'lisp-mnt)

;; No package archives.  Everything this configuration installs comes from a
;; git repository named by a `:vc' recipe or by the closure at the end of this
;; file, and an archive is only ever reached for something no recipe claims --
;; silently, as a tarball, with a date-based version that outranks whatever the
;; git checkout reports.  Leaving the list empty makes that impossible rather
;; than merely unlikely: `package-refresh-contents' has nothing to fetch, so
;; installing packages never contacts elpa.gnu.org, elpa.nongnu.org or
;; melpa.org, and a dependency that is genuinely missing is named in the
;; package's closing message instead of arriving from somewhere else.
;;
;; Nothing needs archive metadata to resolve a recipe either: every `:vc'
;; declaration here spells out its `:url', and none uses the bare `:vc t' form
;; that looks a repository up in the archives.
;;
;; The cost is `M-x list-packages', which can now only show what is installed,
;; and `use-elpa' in `elpa-conf', which can no longer install anything.  Set
;; this list to install something by hand, and the guard below still keeps the
;; archives out of `:vc' installations.
(setq package-archives nil)

;; `:vc' tracks the latest *release tag* by default --
;; `use-package-normalize--vc-arg' defaults `:rev' to `:last-release'.  Every
;; recipe here grew from a straight recipe pinned to a branch, i.e. to its
;; tip, and several of these packages have no release tag at all.
(setq use-package-vc-prefer-newest t)

;; Each checkout is a git working tree; let `C-x p p' see them.
(setq package-vc-register-as-project t)

(defmacro k/package-vc-isolated (&rest body)
  "Run BODY -- a `package-vc' installation -- insulated from this session.

Two bindings, against two ways an installation reaches outside its own
business.

`package-vc-install' hands dependency resolution to package.el, which wants an
archive index for that and takes as a tarball whatever it cannot find on disk.
`package-archives' is empty above, so that half changes nothing today; it is
here so that filling the list in, to install one thing by hand, does not
quietly put MELPA back underneath every `:vc' checkout.

Signature checking goes with the archives.  `package-vc-install' routes
through `package--archives-initialize', whose
\(unless package-archive-contents (package-refresh-contents)) can never be
satisfied once the archive list is empty -- so every single installation runs
`package-refresh-contents', which downloads nothing but does import
`package-keyring.gpg' into `elpa/gnupg' first, spawning gpg and gpg-agent once
per package.  There is nothing to verify: a `:vc' package arrives as a git
checkout and is not signed, and no archive tarball can arrive at all.

The mode hooks matter most.  Generating autoloads and byte-compiling both put
the package\='s own files into `emacs-lisp-mode', which runs
`my-lisp-coding-hook' from `appearance' -- and that calls
`enable-paredit-mode', `rainbow-delimiters-mode' and `idle-highlight-mode'.
Until all three are on disk the function is void, the hook signals, and the
installation fails; since the hook keeps signalling, so does every
installation after it.  One unreachable repository would otherwise take the
rest of the configuration down with it.  Installing a package has no business
running the editing hooks in any case."
  (declare (indent 0) (debug t))
  `(let ((package-archives nil)
         (package-archive-contents nil)
         (package-check-signature nil)
         (emacs-lisp-mode-hook nil)
         (lisp-mode-hook nil)
         (lisp-data-mode-hook nil)
         (prog-mode-hook nil)
         (after-change-major-mode-hook nil)
         (find-file-hook nil))
     ,@body))

;;-------------------------------------------------------------------
;; Corrections to `package-vc'
;;
;; `package-vc--unpack-1' unions `Package-Requires' over every .el file in the
;; checkout.  straight never did that -- it built only the files a MELPA
;; recipe named -- so a configuration of this size runs into consequences
;; that a handful of `:vc' declarations never would.

;; Three of the six functions corrected below are private to `package-vc', and
;; `advice-add' accepts a symbol whose function cell is empty without a word.
;; A rename upstream would therefore not break anything loudly -- it would
;; quietly stop applying the correction, and the symptom would surface much
;; later as a MELPA tarball beside a checkout or an installation that fails on
;; a version header.  Say so at load time instead.  Re-read the docstrings
;; here after an Emacs upgrade; each says what it is defending against, so it
;; is possible to tell whether it is still needed.
(dolist (fn '(package-vc--unpack-1
              package-vc-install-dependencies
              package-vc--generate-description-file
              package-strip-rcs-id
              package-vc--version
              use-package-vc-install))
  (unless (fboundp fn)
    (display-warning
     'package-conf
     (format "`%s' is gone: the correction for it in package-conf no longer applies, and whatever it was defending against is back" fn)
     :error)))

(defvar k/package-vc--unpacking nil
  "Name of the package `package-vc' is currently unpacking.")

(define-advice package-vc--unpack-1 (:around (fn pkg-desc pkg-dir) k/note-package)
  "Record which package is being unpacked, for the advice below."
  (let ((k/package-vc--unpacking (package-desc-name pkg-desc)))
    (funcall fn pkg-desc pkg-dir)))

(defun k/package-vc--drop-self (name reqs)
  "Remove NAME's requirement on itself from REQS."
  (seq-remove (lambda (dep) (eq (car dep) name)) reqs))

(define-advice package-vc-install-dependencies (:filter-args (args) k/drop-self-dep)
  "Do not resolve a package's dependency on itself through an archive.

Scanning the whole checkout makes a repository whose secondary files depend
on its own main package -- dash.el's `dash-functional.el' on `dash', hydra's
`hydra.el' on `lv', magit's `magit.el' on `magit-section' -- report itself as
one of its own dependencies.  It is not in `package-alist' yet at that point,
so package.el reaches for an archive and drops a MELPA tarball beside the git
checkout; the tarball's date-based version outranks the upstream version
header, so the checkout becomes dead weight and the tarball is what loads."
  (list (k/package-vc--drop-self k/package-vc--unpacking (car args))))

(define-advice package-vc-install-dependencies (:filter-return (missing) k/warn-missing)
  "Say out loud which dependencies went unmet.

With no archives to fall back on, a requirement that no recipe in this
configuration claims is simply not installed: `package-vc' notes it in the
closing line of its installation message and carries on, and the package is
quietly broken until something tries to load it.  Anything named here belongs
in the closure at the end of this file."
  (when missing
    (display-warning
     'package-conf
     (format "%s: unclaimed dependencies %s -- add them to the closure in package-conf"
             k/package-vc--unpacking (mapcar #'car missing))
     :warning))
  missing)

(define-advice package-vc--generate-description-file
    (:before (pkg-desc _pkg-file) k/drop-self-dep)
  "Keep the self-dependency out of the generated `<pkg>-pkg.el'.

`package-vc--unpack-1' reloads that file and hands the result to
`package-activate-1' with `deps', which activates each requirement in turn --
and a package that requires itself is never marked active in time, so the two
call each other until `max-lisp-eval-depth' gives out and the installation
fails."
  (setf (package-desc-reqs pkg-desc)
        (k/package-vc--drop-self (package-desc-name pkg-desc)
                                 (package-desc-reqs pkg-desc))))

(define-advice package-strip-rcs-id (:around (fn str) k/tolerate-loose-version)
  "Return nil for an unparsable version instead of signalling.

The docstring still promises \"Otherwise return nil\", and until Emacs 30 an
`ignore-errors' delivered on it.  `package-vc--unpack-1' calls this directly
to build its closing \"installed\" message, with no handler in sight, so a
`Version:' header that is not a dotted number turns a finished installation
into a failed one -- sesman's `0.3.3-DEV', paredit-everywhere's `DEV',
session's `2.4b (see also `session-version' below)'."
  (condition-case nil (funcall fn str) (error nil)))

(define-advice package-vc--version (:around (fn pkg) k/loose-version-prefix)
  "Fall back to the leading number of an unparsable `Version:' header.

With the advice above `package-vc--version' answers nil for such a header,
and that nil reaches `define-package' in the generated description, where
`version-to-list' rejects it in turn.  The leading number keeps sesman at
0.3.3, which is what `cider' asks for; a header with no leading number at all
yields version 0, and nothing here depends on those by version."
  (or (funcall fn pkg)
      (and-let* ((main (package-vc--main-file pkg))
                 ((file-exists-p main))
                 (raw (with-temp-buffer
                        (insert-file-contents main)
                        (or (lm-header "package-version")
                            (lm-header "version"))))
                 ((string-match "\\`[ \t]*\\([0-9]+\\(?:\\.[0-9]+\\)*\\)" raw)))
        (match-string 1 raw))
      "0"))

(defun k/package-vc-clear-empty-checkout (name)
  "Remove an empty `elpa/NAME\=' directory left behind by a failed clone.

`package-vc--clone\=' creates the target directory before handing it to git, so
a checkout that does not happen -- an unreachable host, a branch that is not
there -- leaves it behind empty.  `package-vc--unpack\=' meets it on the next
attempt and asks \"Overwrite previous checkout?\", which in an interactive
Emacs is a question in the middle of every startup until the repository comes
back.  Only an empty directory is removed, so a real checkout, edited or not,
is never in danger."
  (let ((dir (expand-file-name (symbol-name name) package-user-dir)))
    (when (and (file-directory-p dir) (directory-empty-p dir))
      (delete-directory dir))))

(define-advice use-package-vc-install (:around (fn &rest args) k/demote-errors)
  "Report a failed checkout and carry on with the rest of init.
A `:vc' declaration that signals takes the remaining declarations of its file
-- and every file loaded after it -- down with it.  One unreachable host is
not worth an unconfigured Emacs; `k/package-vc-install' below is demoted the
same way."
  (condition-case err
      (progn
        (k/package-vc-clear-empty-checkout (car (car args)))
        (k/package-vc-isolated (apply fn args)))
    (error
     (display-warning
      'package-conf
      (format "%s: %s" (car (car args)) (error-message-string err))
      :error))))

(defun k/package-vc-install (name url &optional branch lisp-dir main-file)
  "Clone package NAME from URL unless a copy is already installed.

BRANCH, LISP-DIR and MAIN-FILE are the `package-vc' spec keywords of the
same name, needed when a repository does not keep NAME.el at its root.
LISP-DIR has to be spelled out even for the conventional `lisp/' and `src/':
`package-vc--unpack' guesses those into a local variable it never puts back
into the spec, and `package-vc--unpack-1' re-reads the spec -- so a guessed
directory reaches neither the autoloads nor `load-path', and the package
installs empty.

The test is `package-alist' rather than `package-installed-p': the latter
counts a package bundled with Emacs as installed, and several packages
here exist precisely to shadow a stale built-in copy.  A failure is
reported and skipped -- one unreachable repository must not abort init."
  (unless (assq name package-alist)
    (message "package-conf: fetching %s from %s" name url)
    (k/package-vc-clear-empty-checkout name)
    (with-demoted-errors "package-conf: %S"
      (k/package-vc-isolated
        (package-vc-install
         (append (list name :url url)
                 (and branch (list :branch branch))
                 (and lisp-dir (list :lisp-dir lisp-dir))
                 (and main-file (list :main-file main-file)))
         nil)))))

;;-------------------------------------------------------------------
;; GNU ELPA core packages
;;
;; These ship both inside Emacs and on GNU ELPA, so two copies exist side by
;; side.  A current `eglot' needs the ELPA ones -- it asks for flymake 1.4.2
;; while Emacs 30.2 bundles 1.3.7 -- and enforces that with
;; `require-with-check': if the feature was already loaded from the built-in
;; file, it errors out with "Feature `flymake' is now provided by a different
;; file", which surfaces as a `File mode specification error' in every LSP
;; buffer.
;;
;; Claiming them first settles it twice over: `package-activate-all' runs
;; before init.el, so from the second start on their directories head
;; `load-path' before any module is loaded, and on the run that installs them
;; they are activated here, ahead of everything else.
;;
;; `eldoc' is here for a different reason and needs no such ordering.  It is
;; preloaded into Emacs, so whichever copy is on `load-path' the dumped one
;; stays loaded -- and `eglot' knows it: of the seven packages it re-requires
;; at the top of eglot.el, `eldoc' and `seq' are the two it passes `reload'
;; to, force-loading them over the built-ins instead of insisting the file
;; already loaded be the right one.  It has to be on disk to be reloaded,
;; though, and current `eglot' asks for eldoc 1.16.0 while Emacs 30.2 bundles
;; 1.15.0.  `seq' needs no entry: 2.24 is bundled and 2.23 is asked for.  Nor
;; do `let-alist' and `map', bundled at versions that satisfy everything here.
(dolist (spec '((flymake             "https://github.com/emacs-straight/flymake.git"             "master")
                (xref                "https://github.com/emacs-straight/xref.git"                "master")
                (project             "https://github.com/emacs-straight/project.git"             "master")
                (jsonrpc             "https://github.com/emacs-straight/jsonrpc.git"             "master")
                (external-completion "https://github.com/emacs-straight/external-completion.git" "main")
                (eldoc               "https://github.com/emacs-straight/eldoc.git"               "master")))
  (apply #'k/package-vc-install spec))

;;-------------------------------------------------------------------
;; Dependency closure
;;
;; `package-vc' resolves a package's dependencies through the archives, that
;; is, as tarballs.  Listed here are the packages that no `use-package'
;; declaration in this configuration names but something in it depends on;
;; claiming them keeps the whole tree on git, the way straight had it.
;;
;; The order is the dependency order.  A package reached before the one it
;; needs would drag that one in from an archive instead, so append with care
;; -- and note that `dash' lives here rather than next to its `use-package'
;; block in `appearance', since half of this list needs it.
;;
;; Pruning an entry is safe: package.el then installs it from MELPA on
;; demand.  `dap-mode', `lsp-treemacs' and `lsp-docker' are deliberately
;; absent -- they need `lsp-mode', which is declared in `java-conf', and so
;; are claimed there.
(dolist (spec '((compat        "https://github.com/emacs-straight/compat.git"      "master")
                (dash          "https://github.com/magnars/dash.el.git"            "master")
                (s             "https://github.com/magnars/s.el.git"               "master")
                (f             "https://github.com/rejeep/f.el.git"                "master")
                (ht            "https://github.com/Wilfred/ht.el.git"              "master")
                (popup         "https://github.com/auto-complete/popup-el.git"     "master")
                (pos-tip       "https://github.com/pitkali/pos-tip.git"            "master")
                (fringe-helper "https://github.com/nschum/fringe-helper.el.git"    "master")
                (names         "https://github.com/Malabarba/names.git"            "master")
                (queue         "https://github.com/emacs-straight/queue.git"       "master")
                (spinner       "https://github.com/emacs-straight/spinner.git"     "master")
                (avy           "https://github.com/abo-abo/avy.git"                "master")
                (pfuture       "https://github.com/Alexander-Miller/pfuture.git"   "master")
                (yaml          "https://github.com/zkry/yaml.el.git"               "master")
                (websocket     "https://github.com/ahyatt/emacs-websocket.git"     "main")
                (simple-httpd  "https://github.com/skeeto/emacs-web-server.git"    "master")
                (wgrep         "https://github.com/mhayashi1120/Emacs-wgrep.git"   "master")
                (markdown-mode "https://github.com/jrblevin/markdown-mode.git"     "master")
                (js2-mode      "https://github.com/mooz/js2-mode.git"              "master")
                (flx           "https://github.com/lewang/flx.git"                 "master")
                (clojure-mode  "https://github.com/clojure-emacs/clojure-mode.git" "master")
                (parseclj      "https://github.com/clojure-emacs/parseclj.git"     "main")
                (parseedn      "https://github.com/clojure-emacs/parseedn.git"     "main")
                (sesman        "https://github.com/vspinu/sesman.git"              "master")
                ;; `request-deferred.el' sits next to `request.el' in the same
                ;; checkout, so package-vc counts its requirement as well.
                (deferred      "https://github.com/kiwanami/emacs-deferred.git"    "master")
                (request       "https://github.com/tkf/emacs-request.git"          "master")
                (llama         "https://github.com/tarsius/llama.git"              "main")
                (cond-let      "https://github.com/tarsius/cond-let.git"           "main")
                (transient     "https://github.com/magit/transient.git"            "main" "lisp")
                (with-editor   "https://github.com/magit/with-editor.git"          "main" "lisp")
                ;; Shares the magit repository with `magit' itself; package.el
                ;; has no notion of two packages in one checkout, so it gets
                ;; its own clone rather than a tarball off MELPA.
                (magit-section "https://github.com/magit/magit.git"                "main" "lisp")
                ;; Likewise `lv', which lives in the hydra repository.
                (lv            "https://github.com/abo-abo/hydra.git"              "master" nil "lv.el")
                (hydra         "https://github.com/abo-abo/hydra.git"              "master")
                (posframe      "https://github.com/tumashu/posframe.git"           "master")
                (bui           "https://github.com/alezost/bui.el.git"             "master")
                (cfrs          "https://github.com/Alexander-Miller/cfrs.git"      "master")
                (ace-window    "https://github.com/abo-abo/ace-window.git"         "master")
                (treemacs      "https://github.com/Alexander-Miller/treemacs.git"  "master" "src/elisp")))
  (apply #'k/package-vc-install spec))

(defun package-conf-highlight-initialize ()
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("k/package-vc-install\\b" . font-lock-keyword-face)
     ("k/package-vc-install '\\(.*\\)[ )]" (1 font-lock-function-name-face)))))

(eval-after-load "package-conf"
  (lambda ()
    (package-conf-highlight-initialize)))

(provide 'package-conf)

;;; package-conf.el ends here
