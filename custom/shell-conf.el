;;; shell-conf.el --- Eshell & shell related configuration.

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

(defun k/ghostel-clipboard-image-p ()
  "Non-nil when the clipboard holds an image rather than text.
X11 advertises what it can convert the selection to; a screenshot taken
by any of the usual tools offers `image/png' among its TARGETS."
  (and (display-graphic-p)
       (let ((targets (ignore-errors (gui-get-selection 'CLIPBOARD 'TARGETS))))
         (and (vectorp targets)
              (seq-some (lambda (target)
                          (string-prefix-p "image/" (symbol-name target)))
                        targets)))))

(defun k/ghostel-paste-dwim ()
  "Paste the clipboard into the terminal, screenshots included.

A screenshot reaches Claude Code as a keystroke rather than as data: the
CLI answers C-v by reading the clipboard itself -- `xclip' under X11,
`wl-paste' under Wayland, `Get-Clipboard' on Windows -- and puts an
`[Image #N]' chip in its prompt.  Nothing has to travel through Emacs,
which has no way to hand a picture to a PTY anyway.

The image branch is limited to Claude Code buffers on purpose: to a
shell C-v means `quoted-insert', so sending it there would only arm the
next keystroke to be taken literally.

Everything else is a text paste through `ghostel-yank', which wraps it
in bracketed paste -- a multi-line paste then arrives as one block
instead of as a run of Return keys, which is what `cua-paste' would have
produced by inserting into the buffer and leaving ghostel's foreign-edit
forwarding to pick the text up."
  (interactive)
  (if (and (k/ghostel-clipboard-image-p)
           (bound-and-true-p claude-code-ide--session))
      (ghostel-send-key "v" "ctrl")
    (ghostel-yank)))

(use-package ghostel
  :straight `(ghostel
              :type git :host nil
              :repo "https://github.com/dakra/ghostel"
              :branch "main"
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
  ;; Terminal palette slot 6.  `ansi-color-cyan' is cyan3 (#00CDCD), which
  ;; barely holds together on a light background -- and it is what the Claude
  ;; Code TUI paints the option digits of its prompts with: those come as
  ;; plain SGR 36, not as a theme colour, so only the palette can reach them.
  :custom-face (ghostel-color-cyan ((t (:foreground "#0065CC"))))
  :bind (("C-x g" . ghostel)
         :map ghostel-semi-char-mode-map
         ("C-f"  . isearch-forward)
         ("C-r"  . isearch-backward)
         ("M-a"  . ace-jump-mode)
         ("<f1>" . consult-buffer)
         ;; Only in semi-char mode: copy mode keeps M-w for
         ;; `ghostel-readonly-copy', which is what it is there for.
         ("M-w"  . get-vc-status)
         ("C-k"  . k/ghostel-send-C-k-and-kill)
         ("M-<left>"   . meta-left)
         ("M-<right>"  . meta-right)
         ("M-<up>"     . windmove-up)
         ("M-<down>"   . windmove-down)
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
         :map project-prefix-map
         ("m" . ghostel-project)
         ("M" . ghostel-project-list-buffers))
  :hook (ghostel-mode . k/ghostel-enable-paste-override)
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
  (defun k/ghostel-ace-jump-leave-input ()
    "Enter `ghostel-emacs-mode' where an ace-jump landed in a terminal.
A no-op outside ghostel buffers, and in copy and Emacs modes, which are
read-only already -- `ghostel-emacs-mode' toggles, and calling it there
would drop the jump straight back into the terminal."
    (when (and (derived-mode-p 'ghostel-mode)
               (not (memq ghostel--input-mode '(copy emacs))))
      (ghostel-emacs-mode)))

  (add-hook 'ace-jump-mode-end-hook #'k/ghostel-ace-jump-leave-input)

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
  ;; So these keys build the event the wheel would have sent and give it to
  ;; the function ghostel's own wheel intercept uses, which encodes whichever
  ;; mouse protocol is in force.  It declines for a program that ignores the
  ;; mouse -- a shell, whose scrollback ghostel does materialize into the
  ;; buffer -- and in copy and Emacs modes, where the buffer is the thing to
  ;; scroll; the line scroll below then runs, exactly as the wheel falls back
  ;; to it.
  (defun k/ghostel-scroll (button count)
    "Scroll COUNT wheel notches: BUTTON 4 back, 5 forward.
Sent to the program while it tracks the mouse, applied to the buffer a
line at a time otherwise."
    (let ((event (list (if (eq button 4) 'wheel-up 'wheel-down)
                       ;; Aimed at the cursor, or at the window corner when
                       ;; the cursor is out of sight.
                       (or (posn-at-point) (posn-at-x-y 0 0))))
          (scroll (if (eq button 4) #'scroll-down-line #'scroll-up-line)))
      (condition-case err
          (dotimes (_ (or count 1))
            (unless (ghostel--forward-scroll-event event button)
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
                         (flet ((pop-to-buffer-same-window
                                 (b) (switch-to-buffer-other-window b)))
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

;;------------------------------------------------------------
;; shell

(if (eq system-type 'windows-nt)
    ;; C-x C-f C-f /<user>@<host>:<path>
    (setq tramp-default-method "plink"))

(setq w32-quote-process-args t)

;; Windows shell (cmd) correct encoding
(when (eq system-type 'windows-nt)
  (defadvice shell (after my-shell-advice)
    (set-process-coding-system 'cp1251 'cp1251))
  (ad-activate 'shell))

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

(setq eshell-prompt-regexp "^λ "
      eshell-prompt-function
      (lambda ()
        (concat
         (propertize (eshell/pwd) 'face 'font-lock-function-name-face)
         (if-let ((branch (k/git-branch)))
             (propertize (concat "\n" branch) 'face 'font-lock-builtin-face))
         (propertize "\nλ" 'face `(:foreground "#5544EE" :weight bold))
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

(defun k/eshell-mode-hook ()
  (define-key eshell-mode-map (kbd "<up>") 'k/line-previous)
  (define-key eshell-mode-map (kbd "<down>") 'k/line-next)
  (define-key eshell-mode-map (kbd "<home>") 'eshell-maybe-bol))

(add-hook 'eshell-mode-hook 'k/eshell-mode-hook)

(provide 'shell-conf)

;;; shell-conf.el ends here
