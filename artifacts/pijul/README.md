# Pijul editor integrations

Thin editor clients over the `pijul` CLI's machine-readable (`--output-format
json`) output. The design goal is that the *contract* — the JSON emitted by the
CLI — is the reusable asset, and each editor client stays as small as possible.
The same JSON is meant to drive a VSCode extension (see the planned
`editors/vscode`) with equally little code.

## Installation

Requires Emacs 28+ and a `pijul` on `PATH` built from this tree. Add the
directory to your `load-path` and load the umbrella `pijul` feature:

```elisp
(add-to-list 'load-path "/path/to/pijul/editors/emacs")
(require 'pijul)
(global-pijul-mode 1)   ; auto-enables in any file under a `.pijul' repo
```

`global-pijul-mode` turns on `pijul-mode` in every file-visiting buffer that has
a `.pijul` directory in a parent, giving you these bindings:

| Key       | Command                | What it does                                  |
|-----------|------------------------|-----------------------------------------------|
| `C-c p b` | `pijul-credit`         | per-line change attribution (blame) overlays  |
| `C-c p d` | `pijul-context-diff`   | record-time preview, highlighted in context   |
| `C-c p c` | `pijul-context-change` | view any committed change, highlighted         |
| `C-c p r` | `pijul-record-preview` | change buffer + companion, with point-following |
| `C-c p l` | `pijul-land`           | land the current fork onto main (`piclaude land`); `C-u` to amend |

If `pijul` is not on `PATH`, set `pijul-context-program` /
`pijul-credit-program` to its absolute path.

`pijul-land` drives the `piclaude` launcher (parallel-agent fork workflow):
it records in the fork, then under a lock pulls main in and pushes back, so
main stays buildable. On a conflict it stops, leaves markers in the fork, and
reverts the affected buffers — resolve them and run `C-c p l` again (it is
re-runnable). Set `pijul-piclaude-program` if `piclaude` is not on `PATH`.

### Nix-managed Emacs

These files use **only built-in Emacs libraries** (no MELPA/ELPA dependencies),
so a declarative/Nix-managed package set is irrelevant — you just need the
directory on `load-path`. Three options, easiest first:

1. **Try it now, no config change.** Evaluate in a running Emacs (`M-x
   eval-expression`, or select and `M-x eval-region`):

   ```elisp
   (add-to-list 'load-path "/path/to/pijul/editors/emacs")
   (require 'pijul)
   (global-pijul-mode 1)
   ```

   Or a throwaway session: `emacs -L /path/to/pijul/editors/emacs -l pijul \
   --eval '(global-pijul-mode 1)'`.

2. **Put the two lines in your init.** Nix controls which *packages* load, but
   your init can add any local `load-path` and `require` it — no packaging
   needed.

3. **Package it with Nix** (permanent, byte-compiled, on `load-path`
   automatically) via `trivialBuild` — no `packageRequires` because there are no
   external deps. With home-manager:

   ```nix
   programs.emacs = {
     enable = true;
     extraPackages = epkgs: [
       (epkgs.trivialBuild {
         pname = "pijul";
         version = "0.1.0";
         src = /path/to/pijul/editors/emacs;   # the directory with the .el files
       })
     ];
   };
   ```

   then just `(require 'pijul)` + `(global-pijul-mode 1)` in your config.

Under Nix your Emacs likely won't see a dev-built `pijul` on `PATH`; set
`pijul-context-program`/`pijul-credit-program` to the absolute binary path (e.g.
your `cargo` target dir) or add it to Emacs's `exec-path`.

### Try the record preview with point-following

From any file in a repository, `M-x pijul-record-preview` (`C-c p r`) opens two
buffers: the change text in `pijul-commit-mode`, and the `*pijul-context*`
companion showing the change inlined in each file (added / removed / ghost). Move
point across hunks in the change buffer and the matching span pulses in the
companion.

### Wiring the *live* `pijul record` flow

`pijul-commit-mode` also auto-activates on the `.pijul-commit` file that
`pijul record` opens in `$EDITOR`, so you get the same companion + point-following
during a real record. Point Pijul at an Emacs client (with a server running:
`M-x server-start`, or `(server-start)` in your init):

```sh
export VISUAL='emacsclient'
export EDITOR="$VISUAL"
```

The record buffer's own directory is a tempdir, so `pijul-commit-mode` locates the
repository via `.pijul` detection, falling back to the most recently visited Pijul
repo (`pijul--last-repo`, set by `global-pijul-mode`) — normally the repo you just
ran `pijul record` in.

## `pijul-credit.el` (proof of concept)

Per-line change attribution as buffer overlays, consuming
`pijul credit --output-format json FILE`. Each line is tinted with a colour
deterministically derived from the change that introduced it; hovering shows the
full change hash(es).

```elisp
(add-to-list 'load-path "/path/to/pijul/editors/emacs")
(require 'pijul-credit)
;; M-x pijul-credit        to annotate
;; M-x pijul-credit-clear  to remove
```

Requires Emacs 28+ (native `json-parse-buffer`) and `pijul` on `PATH`.

## `pijul-context.el` (proof of concept)

The change inlined in the full context of each file it touches, in three
colours — added, removed (inline ghost), added-then-removed — the
["palimpsest"](../../../palimpsest/palimpsest) view. Consumes:

- `pijul change <hash> --context` — a committed change (`files[].vertices[]`);
- `pijul diff --context` — the **record-time preview**: the uncommitted working
  copy, i.e. exactly what `pijul record` is about to record (`files[].segments[]`).

```elisp
(require 'pijul-context)
;; M-x pijul-context-diff     preview what you're recording, in context
;; M-x pijul-context-change   view any committed change in context
```

The intended workflow: with a `.pijul-commit` record buffer open, `M-x
pijul-context-diff` gives you the companion buffer showing the change
highlighted in each file's full context. (Next step: a `pijul-commit-mode` that
follows point — highlight the file span for the hunk under the cursor.)

Both CLI commands are powered by the `palimpsest` crate, linked into the CLI
(dual-licensed AGPL-3.0-or-later OR GPL-2.0-or-later so it composes with the
GPL-2.0-or-later CLI).

### The JSON contract it consumes

`pijul credit --output-format json` emits an ordered array of entries.
Replaying them in order reconstructs the annotated file:

```jsonc
[
  { "type": "line",
    "startLine": 2,          // 1-based
    "lineCount": 1,
    "changes": ["TBLTS7CL6JI5…"],   // full base32 hashes, sorted
    "content": "line two CHANGED\n" },

  // conflicts are delimited by explicit markers in the same stream:
  { "type": "conflict", "marker": "start", "id": 0, "sides": ["…","…"] }
]
```

Note it carries **full** change hashes (the human `credit` output truncates to
12 chars) and explicit line geometry — both are what an editor needs and what
the text renderer throws away.

## Roadmap / relationship to other efforts

- **`credit` (done, POC):** highlight lines by authoring change — above.
- **`status` / `diff` JSON (done):** `status --output-format json` yields
  `{channel, changes:[{path, status, lines}], untracked}`; `diff
  --output-format json` yields `{path: [{operation, line}]}`. Enough for a
  `vc-dir`/SCM status view and jump-to-hunk. `diff`'s line field is still a
  single start line; per-hunk *ranges* (for full gutter bars) are the next
  increment. All read commands share one `--output-format` flag.
- **VC backend:** the pragmatic Emacs path is a `vc-pijul` backend rather than a
  hand-rolled Magit-style porcelain — it yields `vc-dir`, diff, log, annotate and
  `diff-hl` gutter for little code. See the existing
  [`~akagi/vc-pijul`](https://git.sr.ht/~akagi/vc-pijul).
- **Interactive record:** `pijul record` opens the serialized change in
  `$EDITOR`; a dedicated major mode for that buffer (via `emacsclient`) gives a
  native "hunk selection" experience with no extra protocol.
