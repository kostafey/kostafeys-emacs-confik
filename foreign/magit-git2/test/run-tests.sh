#!/bin/sh
# Run the magit-git2 ERT suite against a fresh fixture.
#
#   test/run-tests.sh [ERT-SELECTOR]
#
# MAGIT_LOAD_PATH  colon-separated directories with magit and its
#                  dependencies (default: ~/.emacs.d/straight/build/*)
# MAGIT_GIT2_TEST_REPOS
#                  colon-separated real repositories to test as well
#                  (default: the repository containing this script)
# MAGIT_GIT2_TEST_REPORT
#                  file to append the mismatches to (default: a file in
#                  the temporary directory, removed afterwards)
# EMACS            the Emacs to use (default: emacs)
#
# HOME, XDG_CONFIG_HOME and GIT_CONFIG_SYSTEM point into a temporary
# directory, so neither git nor libgit2 sees your own config.

set -e
here=$(cd "$(dirname "$0")/.." && pwd)
emacs=${EMACS:-emacs}

if [ -z "$MAGIT_LOAD_PATH" ]; then
    MAGIT_LOAD_PATH=$(printf '%s:' "$HOME"/.emacs.d/straight/build/*)
fi
if [ -z "${MAGIT_GIT2_TEST_REPOS+set}" ]; then
    MAGIT_GIT2_TEST_REPOS=$(git -C "$here" rev-parse --show-toplevel)
fi
export MAGIT_GIT2_TEST_REPOS

root=$(mktemp -d "${TMPDIR:-/tmp}/magit-git2-test.XXXXXX")
trap 'rm -rf "$root"' EXIT
mkdir -p "$root/home" "$root/etc" "$root/emacs"

load_path=
old_ifs=$IFS
IFS=:
for dir in $MAGIT_LOAD_PATH; do
    [ -n "$dir" ] && load_path="$load_path -L $dir"
done
IFS=$old_ifs

MAGIT_GIT2_FIXTURE_ROOT=$root \
HOME=$root/home \
XDG_CONFIG_HOME=$root/home/.config \
GIT_CONFIG_SYSTEM=$root/etc/gitconfig \
    "$emacs" --batch --init-directory="$root/emacs" \
    $load_path -L "$here/lisp" -L "$here/test" \
    -l magit-git2-test \
    --eval "(ert-run-tests-batch-and-exit '${1:-t})"
