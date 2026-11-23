;;; shell-conf.el --- Eshell & shell related configuration.  -*- lexical-binding: t -*-

(require 'cl-lib)

(straight-use-package
 '(eshell-prompt-extras :type git :host github
			                  :repo "suzzvv/eshell-prompt-extras" :branch "master"))
(use-package exec-path-from-shell
  :straight '(exec-path-from-shell :type git :host github
			                             :repo "purcell/exec-path-from-shell"
                                   :branch "master")
  :config (when (eq system-type 'gnu/linux)
              (exec-path-from-shell-initialize)))

(straight-use-package
 '(exec-path-from-shell :type git :host github
			                  :repo "purcell/exec-path-from-shell" :branch "master"))
(straight-use-package
 '(emacs-libvterm :type git :host github
			            :repo "akermu/emacs-libvterm" :branch "master"))

(use-package vterm
  :straight `(vterm
              :type git :host nil
              :repo "https://github.com/akermu/emacs-libvterm"
              :branch "master")
  :bind (:map vterm-mode-map
         ("C-<insert>" . vterm-yank)
         ;; Navigate among windows and frames the same way as in any other
         ;; mode, do not pass M-<arrow> through to the terminal.
         ("M-<left>"   . meta-left)
         ("M-<right>"  . meta-right)
         ("M-<up>"     . windmove-up)
         ("M-<down>"   . windmove-down)))

;; Pasting into a ghostel terminal, screenshots included.
;;
;; cua-mode keeps its CUA keys in `cua--cua-keys-keymap', published through
;; `emulation-mode-map-alists', and those maps outrank both the major mode map
;; and `ghostel-semi-char-mode-map' -- so C-v resolves to `cua-paste' and a
;; binding in ghostel's own maps would never be consulted.  Join the same
;; mechanism, in front of cua's entry, switched on per buffer.  (S-<insert> is
;; absent from that keymap, which is why it can be bound the ordinary way.)
(defvar-local k/ghostel-paste-override nil
  "Non-nil where `k/ghostel-paste-override-map' should take effect.")

(defvar k/ghostel-paste-override-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-v") #'k/ghostel-paste-dwim)
    map)
  "Keymap reclaiming the paste key from cua-mode inside ghostel buffers.")

(add-to-list 'emulation-mode-map-alists
             `((k/ghostel-paste-override . ,k/ghostel-paste-override-map)))

(defun k/ghostel-enable-paste-override ()
  "Let `k/ghostel-paste-override-map' win over cua-mode here."
  (setq-local k/ghostel-paste-override t))

(defun k/ghostel-ace-jump-words ()
  "Let AceJump word mode find the words inside paths in ghostel."
  (setq-local ace-jump-word-mode-syntax-table (standard-syntax-table)))

(defun k/ghostel-clipboard-image-p ()
  "Non-nil when the clipboard holds an image rather than text.
X11 advertises what it can convert the selection to; a screenshot taken
by any of the usual tools offers `image/png' among its TARGETS.  Windows
lists raw clipboard format names instead: `BITMAP', `DIB' and `DIBV5'
for any picture, plus `PNG' from the Snipping Tool."
  (and (display-graphic-p)
       (let ((targets (ignore-errors (gui-get-selection 'CLIPBOARD 'TARGETS))))
         (and (vectorp targets)
              (seq-some (lambda (target)
                          (let ((name (symbol-name target)))
                            (or (string-prefix-p "image/" name)
                                (member name '("BITMAP" "DIB" "DIBV5" "PNG")))))
                        targets)))))

(defun k/ghostel-paste-dwim ()
  "Paste the clipboard into the terminal, screenshots included.

A screenshot reaches Claude Code, Codex or Pi as a keystroke rather than
as data: the CLI answers C-v by reading the clipboard itself and
attaches the image to its prompt.  Nothing has to travel through Emacs,
which has no way to hand a picture to a PTY anyway.  On Windows Claude
Code and Pi listen for M-v instead; Codex uses C-v on every platform.

The image branch is limited to Claude Code, Codex and Pi buffers: to a shell
C-v means `quoted-insert', so sending it there would only arm the next
keystroke to be taken literally.

Everything else is a text paste through `ghostel-yank', which wraps it
in bracketed paste -- a multi-line paste then arrives as one block
instead of as a run of Return keys, which is what `cua-paste' would have
produced by inserting into the buffer and leaving ghostel's foreign-edit
forwarding to pick the text up."
  (interactive)
  (if (and (k/ghostel-clipboard-image-p)
           (or (bound-and-true-p codex-ide-project-root)
               (bound-and-true-p k/codex-project-root)
               (bound-and-true-p pi-ide-project-root)
               (bound-and-true-p claude-code-ide--session)))
      (if (and (eq system-type 'windows-nt)
               (not (or (bound-and-true-p codex-ide-project-root)
                        (bound-and-true-p k/codex-project-root))))
          (ghostel-send-key "v" "meta")
        (ghostel-send-key "v" "ctrl"))
    (ghostel-yank)))

(use-package ghostel
  :straight `(ghostel
              :type git :host nil
              :repo "https://github.com/dakra/ghostel"
              :branch "main"
              ;; Remotes named the GitHub way, not straight's: `origin' is
              ;; the fork, `upstream' the original.
              :remote "upstream"
              :fork (:repo ,(pcase system-type
                              ('windows-nt
                               "https://github.com/kostafey/ghostel.git")
                              ('gnu/linux
                               "git@github.com:kostafey/ghostel.git"))
                     :branch "develop"
                     :remote "origin")
              ;; Same file set as the upstream MELPA recipe: without it
              ;; straight links only `lisp/*.el' into the build directory,
              ;; ghostel finds no bundled `etc/terminfo/' and falls back to
              ;; TERM=xterm-256color (choppy redraws in Claude Code & co).
              :files (:defaults "etc" "src" "vendor"
                                "build.zig" "build.zig.zon" "symbols.map"))
  ;; Keep the native module outside the straight build directory, which is
  ;; wiped on every rebuild/update of the package.
  :custom (ghostel-module-directory
           (expand-file-name "ghostel/" user-emacs-directory))
          ;; On Windows zig's native target is MSVC-flavoured and wants a
          ;; Windows SDK (WindowsSdkNotFound without one).  Build against
          ;; MinGW instead, as upstream's Makefile and release DLLs do.
          (ghostel-module-compile-command
           (if (eq system-type 'windows-nt)
               (format "zig build --prefix %%s -Doptimize=ReleaseFast -Dcpu=baseline -Dtarget=%s-windows-gnu"
                       (if (string-match-p "\\`\\(aarch64\\|arm64\\)"
                                           system-configuration)
                           "aarch64" "x86_64"))
             "zig build --prefix %s -Doptimize=ReleaseFast -Dcpu=baseline"))
  ;; Terminal palette slot 6.  `ansi-color-cyan' is cyan3 (#00CDCD), which
  ;; barely holds together on a light background -- and it is what the Claude
  ;; Code TUI paints the option digits of its prompts with: those come as
  ;; plain SGR 36, not as a theme colour, so only the palette can reach them.
  ;; Use the Organic Green palette for ANSI diff counts and tool markers.
  :custom-face (ghostel-color-cyan ((t (:foreground "#0065CC"))))
               (ghostel-color-red ((t (:foreground "#EF2929"))))
               (ghostel-color-green ((t (:foreground "#119011"))))
               (ghostel-color-bright-black ((t (:foreground "#119011"))))
  ;; The module commands carry no autoload cookies upstream, so M-x knows
  ;; them only once `ghostel' is loaded -- yet they are what to reach for
  ;; when the module is missing or broken, i.e. before ghostel ever ran.
  :commands (ghostel-module-compile ghostel-download-module)
  :bind (("C-x g" . ghostel)
         :map ghostel-semi-char-mode-map
         ("C-f"  . isearch-forward)
         ("C-r"  . isearch-backward)
         ("M-a"  . ace-jump-mode)
         ("<f1>" . consult-buffer)
         ;; Only in semi-char mode: copy mode keeps M-w for
         ;; `ghostel-readonly-copy', which is what it is there for.
         ("M-w"  . get-vc-status)
         ("M-W"  . k/multi-magit-project-status)
         ("C-k"  . k/ghostel-send-C-k-and-kill)
         ;; `ghostel-mode-map' sends C-g on to the program, and Claude Code
         ;; answers ^G by opening the prompt in $EDITOR -- a window outside
         ;; Emacs, with the session parked on "Save and close editor to
         ;; continue..." until it is closed.  Nothing here wants to type ^G
         ;; anyway: the interrupt is `C-c C-c' and the quit `C-c C-\', and
         ;; `C-q C-g' still sends it for the rare program that reads it.
         ;; Copy and Emacs modes bind C-g to `ghostel-readonly-exit', char
         ;; mode goes on sending it -- both are what they should be.
         ("C-g"  . k/ghostel-keyboard-quit)
         ("M-<left>"   . meta-left)
         ("M-<right>"  . meta-right)
         ("M-<up>"     . windmove-up)
         ("M-<down>"   . windmove-down)
         ;; Use the global `mirror-window' binding instead of typing M-m.
         ("M-m"        . nil)
         ("C-<prior>"  . eframe-previous-buffer)
         ("C-<next>"   . eframe-next-buffer)
         ;; Scrolling with the keys `basic-keys' gives it everywhere else:
         ;; a wheel notch for the program, a line for the buffer -- see
         ;; `k/ghostel-scroll'.
         ("C-<up>"     . k/ghostel-scroll-backward)
         ("C-<down>"   . k/ghostel-scroll-forward)
         ("<delete>"   . (lambda () (interactive) (ghostel-send-key "d" "ctrl")))
         ;; C-v is handled by `k/ghostel-paste-override-map' instead: a binding
         ;; here would be shadowed by cua-mode.
         ("S-<insert>" . k/ghostel-paste-dwim)
         ;; Selecting and sexp motion as in any other buffer (`basic-keys').
         ;; Semi-char mode would otherwise forward these to the program, which
         ;; has no use for them; run as Emacs commands they take the buffer
         ;; into copy mode on their own -- `ghostel-mark-activation-input-mode'
         ;; answers the mark, `ghostel-point-leave-input-mode' the bare motion
         ;; -- and there the global bindings carry on from where these leave
         ;; off, the terminal frozen while the region is picked out.
         ("S-<right>"     . (lambda () (interactive) (k/char-forward t)))
         ("S-<left>"      . (lambda () (interactive) (k/char-backward t)))
         ("S-<up>"        . (lambda () (interactive) (k/line-previous t)))
         ("S-<down>"      . (lambda () (interactive) (k/line-next t)))
         ("S-<home>"      . (lambda () (interactive) (k/line-beginning t)))
         ("S-<end>"       . (lambda () (interactive) (k/line-end t)))
         ("C-S-<right>"   . (lambda () (interactive) (k/word-forward t)))
         ("C-S-<left>"    . (lambda () (interactive) (k/word-backward t)))
         ;; Motion with no region of its own has to say so: ghostel wires
         ;; `ghostel-maybe-leave-input' into isearch and the minibuffer only,
         ;; and leaves other jumps to call it.  Without it point drifts off
         ;; the cursor and the next redraw hauls it back.
         ("C-M-<right>"   . (lambda () (interactive)
                              (k/sexp-forward) (ghostel-maybe-leave-input)))
         ("C-M-<left>"    . (lambda () (interactive)
                              (k/sexp-backward) (ghostel-maybe-leave-input)))
         ("C-M-S-<right>" . (lambda () (interactive) (k/sexp-forward t)))
         ("C-M-S-<left>"  . (lambda () (interactive) (k/sexp-backward t)))
         ("C-S-<home>"    . (lambda () (interactive) (k/buffer-beginning t)))
         ("C-S-<end>"     . (lambda () (interactive) (k/buffer-end t)))
         ;; The same scrolling in copy and Emacs modes, which share this map;
         ;; `k/ghostel-scroll' tells the two apart.
         :map ghostel-readonly-mode-map
         ("C-<up>"        . k/ghostel-scroll-backward)
         ("C-<down>"      . k/ghostel-scroll-forward)
         :map project-prefix-map
         ("m" . ghostel-project)
         ("M" . ghostel-project-list-buffers))
  :hook ((ghostel-mode . k/ghostel-enable-paste-override)
         (ghostel-mode . k/ghostel-ace-jump-words))
  :config
  ;; Copying several lines out of a full-screen TUI.
  ;;
  ;; Every copy out of a ghostel buffer runs through
  ;; `ghostel--filter-soft-wraps', which drops the newline of each row the
  ;; terminal marked as soft-wrapped -- so a shell line that spilled over the
  ;; right edge comes back as the single line it logically is.
  ;;
  ;; On the alternate screen that rule misfires.  A TUI paints the grid row by
  ;; row, and Claude Code (like anything built on Ink) ends a row that reaches
  ;; the last column by letting the terminal wrap rather than by writing the
  ;; newline itself.  Its panels are padded to the full width, so nearly every
  ;; row carries the soft-wrap flag and a copied block -- a code listing, a
  ;; diff, a command it proposes -- arrives as one long line with the padding
  ;; baked into the middle of it.
  ;;
  ;; Nothing distinguishes those rows from a genuine spillover, but on the
  ;; alternate screen the distinction does not matter: there is no scrollback
  ;; to reflow, the program has already laid the text out for this width, and
  ;; the rows on screen are the lines to copy.  Keep them, and let the trailing
  ;; whitespace trim `ghostel--clean-copy-text' does next take off the padding.
  (defun k/ghostel-keep-alt-screen-rows (filter-fn text)
    "Keep the row newlines in TEXT while the alternate screen is up.
FILTER-FN is the advised `ghostel--filter-soft-wraps', used as usual
for the main screen, where a wrapped row really is a continuation."
    (if (ghostel-alt-screen-p) text (funcall filter-fn text)))

  (advice-add 'ghostel--filter-soft-wraps :around
              #'k/ghostel-keep-alt-screen-rows)

  ;; Where an ace-jump lands.
  ;;
  ;; ghostel answers a jump out of the live input on its own only for the two
  ;; it is wired into, isearch and the minibuffer, and leaves every other one
  ;; to call `ghostel-maybe-leave-input' -- otherwise point drifts off the
  ;; cursor and the next redraw hauls it back, the same way the sexp motions
  ;; above have to say so.  `M-a' is the jump used here most.
  ;;
  ;; It goes to Emacs mode rather than to the copy mode
  ;; `ghostel-point-leave-input-mode' would pick, because the reason to jump
  ;; into a terminal is to read or copy something while the program keeps
  ;; working, and copy mode freezes exactly the output being followed.  The
  ;; buffer is read-only either way, so everything reached from there -- the
  ;; selection keys, `M-w', hyperlinks -- behaves identically.
  ;;
  ;; `ace-jump-mode-end-hook' runs only after the jump itself, on both the
  ;; single-candidate and the label-selected paths, so an aborted jump leaves
  ;; the mode alone.
  ;;
  ;; Entering Emacs mode can itself haul point back: `ghostel--enter-readonly'
  ;; shows its entry message while the input mode is still semi-char, the
  ;; message redisplays, and a pending window size change runs
  ;; `ghostel--adjust-size', which re-anchors the window and snaps point to
  ;; the terminal cursor before Emacs mode is set.  So put point back where
  ;; the jump left it; off the cursor, Emacs mode stops following the output.
  (defun k/ghostel-ace-jump-leave-input ()
    "Enter `ghostel-emacs-mode' where an ace-jump landed in a terminal.
A no-op outside ghostel buffers, and in copy and Emacs modes, which are
read-only already -- `ghostel-emacs-mode' toggles, and calling it there
would drop the jump straight back into the terminal."
    (when (and (derived-mode-p 'ghostel-mode)
               (not (memq ghostel--input-mode '(copy emacs))))
      (let ((target (point)))
        (ghostel-emacs-mode)
        (goto-char target))))

  (add-hook 'ace-jump-mode-end-hook #'k/ghostel-ace-jump-leave-input)

  (defun k/ghostel-keyboard-quit ()
    "Quit, instead of sending `C-g' to the program.
`ghostel-mode' binds `inhibit-quit' so that C-g reaches the keymap as
an ordinary key -- but reading it still raised `quit-flag', and nothing
lowers it here, so it would fire as a stray quit in the first buffer
that does not inhibit it.  `ghostel-send-C-g' clears the flag for the
same reason; do that, then quit as everywhere else."
    (interactive)
    (setq quit-flag nil)
    (keyboard-quit))

  (defun k/ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel.
Like normal Emacs `C-k'.  Kill to end of line and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl"))

  ;; Scrolling with C-<up> / C-<down>, the keys `basic-keys' gives it
  ;; everywhere else.
  ;;
  ;; Where the scrollback lives decides what they can do.  Under a fullscreen
  ;; TUI -- Claude Code, and anything else on the alternate screen -- the
  ;; buffer holds the visible grid and nothing more: there is no history above
  ;; `point-min' to scroll to, and the transcript worth scrolling is the
  ;; program's own.  Which is why the wheel scrolls one of those and
  ;; `scroll-down-line' scrolls nothing: ghostel hands a wheel notch to the
  ;; program whenever it has asked to track the mouse.
  ;;
  ;; So these keys send the program the press a wheel notch would have sent,
  ;; through the call ghostel's own wheel intercept ends in, which encodes
  ;; whichever mouse protocol is in force.  That is for a program tracking the
  ;; mouse, in semi-char mode and in Emacs mode alike: Emacs mode keeps the
  ;; terminal live, so the program redraws the buffer under the scroll.  For a
  ;; program that ignores the mouse -- a shell, whose scrollback ghostel does
  ;; materialize into the buffer -- and in copy mode, which freezes the
  ;; terminal and leaves the buffer the only thing to scroll, the line scroll
  ;; runs instead, the one the wheel falls back to there.
  ;;
  ;; The wheel itself does not follow into Emacs mode: ghostel's intercept
  ;; forwards it in semi-char and char modes only.
  (defun k/ghostel-scroll (button count)
    "Scroll COUNT wheel notches: BUTTON 4 back, 5 forward.
Sent to the program while it tracks the mouse and the terminal is live
-- semi-char or Emacs mode -- applied to the buffer a line at a time
otherwise.  In Codex, target the transcript even when point is in the
prompt editor."
    (let ((posn (or (and (or (bound-and-true-p codex-ide-project-root)
                             (bound-and-true-p k/codex-project-root))
                         ;; Codex routes wheel events by screen position.  Aim
                         ;; inside the transcript, below its sticky prompt
                         ;; header, rather than at the composer containing the
                         ;; cursor.
                         (posn-at-x-y (* 2 (default-font-width))
                                      (* 2 (default-line-height))))
                    (posn-at-point)
                    (posn-at-x-y 0 0)))
          (scroll (if (eq button 4) #'scroll-down-line #'scroll-up-line)))
      (condition-case err
          (dotimes (_ (or count 1))
            (if (and ghostel--term
                     (memq ghostel--input-mode '(semi-char emacs))
                     (ghostel--mouse-tracking-p ghostel--term))
                (let ((cell (ghostel--posn-cell posn)))
                  (ghostel--mouse-event ghostel--term 0 button ; press
                                        (cdr cell) (car cell) 0))
              (funcall scroll 1)))
        ;; Say so rather than ding, the way `mwheel-scroll' does.
        ((beginning-of-buffer end-of-buffer)
         (message "%s" (error-message-string err))))))

  (defun k/ghostel-scroll-backward (&optional count)
    "Scroll back through the scrollback, COUNT notches (one by default)."
    (interactive "p")
    (k/ghostel-scroll 4 count))

  (defun k/ghostel-scroll-forward (&optional count)
    "Scroll forward through the scrollback, COUNT notches (one by default)."
    (interactive "p")
    (k/ghostel-scroll 5 count))

  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t)
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer))
  )

(defcustom k/default-shell 'eshell
  "Set default shell type. Possible values are one of:
'eshell
'shell."
  :type 'symbol)

(defun k/shell (&optional num)
  (interactive "P")
  (if (eq major-mode 'eshell-mode)
      (message "Already in Eshell")
    (let* ((current-dir (cond
                         ((eq major-mode 'dired-mode)
                          default-directory)
                         (buffer-file-name
                          (file-name-directory (buffer-file-name)))))
           (shell-buffer-name (if num
                                  (format "*%s %s*"
                                          (symbol-name k/default-shell) num)
                                (format "*%s*"
                                        (symbol-name k/default-shell))))
           (shell-bufer-exists-p (get-buffer shell-buffer-name)))
      (with-current-buffer
          (pcase k/default-shell
            ('eshell (or (if (not num)
                             (eframe-pop-buffer 'eshell-mode))
                         (cl-letf (((symbol-function 'pop-to-buffer-same-window)
                                    #'switch-to-buffer-other-window))
                           (eshell num))))
            ('shell (shell shell-buffer-name)))
        (goto-char (point-max))
        (when (and shell-bufer-exists-p
                   (not (equal default-directory current-dir)))
          ;; Clear chars existing in command line.
          (let ((line-beg (save-excursion
                            (eshell-bol)
                            (point))))
            (k/line-end)
            (while (> (point) line-beg)
              (delete-char -1)))
          (insert "cd ")
          (insert current-dir)
          (pcase k/default-shell
            ('eshell (eshell-send-input))
            ('shell (comint-send-input))))))))

(global-set-key (kbd "<f4>") 'k/shell)

;;------------------------------------------------------------
;; shell

(if (eq system-type 'windows-nt)
    ;; C-x C-f C-f /<user>@<host>:<path>
    (setq tramp-default-method "plink"))

(setq w32-quote-process-args t)

;; Windows shell (cmd) correct encoding
(defun k/shell-cp1251 (buffer)
  "Make the process of the shell BUFFER talk cp1251; return BUFFER."
  (set-process-coding-system (get-buffer-process buffer) 'cp1251 'cp1251)
  buffer)

(when (eq system-type 'windows-nt)
  (advice-add 'shell :filter-return #'k/shell-cp1251))

;;------------------------------------------------------------
;; eshell

(defun k/git-branch ()
  "Return your git branch name."
  (let ((branch (car (vc-git-branches))))
    (cond
     ((null branch) nil)
     ((string-match "^(HEAD detached at \\(.+\\))$" branch)
      (concat epe-git-detached-HEAD-char (match-string 1 branch)))
     (t branch))))

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

(provide 'shell-conf)

;;; shell-conf.el ends here
