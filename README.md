
# Table of Contents

1.  [Initial setup](#orgb45ac38)
2.  [Kostafey's keybindings](#orgbd480d4)
    1.  [Humane emacs](#org06d0d29)
        1.  [Exit/hide emacs](#org9322619)
        2.  [Select Copy Paste](#org0d031cc)
        3.  [Undo/redo](#org11105d3)
        4.  [Delete/insert char](#orgfd1a0f7)
        5.  [Save/revert](#orgb753a61)
    2.  [Basic point movements & change buffer's position](#org69b5666)
    3.  [Point hyper-jumps](#org9f87c5a)
        1.  [Bookmarks](#orgf098428)
        2.  [Search & replace](#orgd8e0a4a)
        3.  [Intellectual point jumps](#orgdf0fcef)
    4.  [Frames](#orgd7cf1e6)
    5.  [Command executions](#org9aaaef1)
    6.  [Text transformations](#org0c421d1)
        1.  [Basic text transformations](#orgd05774f)
        2.  [Rectangle operations](#orge853967)
        3.  [Upcase/downcase](#org8c5b31b)
        4.  [Region & misc operations](#org2f36b59)
        5.  [Buffers navigation](#orgc739cdc)
        6.  [ASCII graphic & formatting notes](#org36ab172)
        7.  [Paredit customization](#org94cf3a2)
    7.  [IDE](#org01c6928)
        1.  [Speedbar](#org03afe10)
        2.  [Common prog mode keys](#org4c9d33a)
        3.  [Java](#org2fd40af)
        4.  [Lisp](#orga3ff920)
        5.  [Emacs Lisp](#orga013c07)
        6.  [Clojure](#org43739d3)
        7.  [Lua](#orgf48ea7c)
        8.  [Scala](#org51d720c)
        9.  [Tcl](#org0e6e827)
        10. [Golang](#org585d4c7)
        11. [reStructuredText](#org1148258)
        12. [SQL](#org1d073db)
        13. [Version control](#org91a8d93)
            1.  [Magit & ahg](#org2ab03cc)
            2.  [git-gutter](#orgd0a399e)
    8.  [Mouse](#orgd079935)
    9.  [Menu](#org6353ec1)
        1.  [header<sub>name</sub>](#org650f6ff)
    10. [Org-mode](#org0d778f0)
        1.  [Time schedule](#org6ce2e71)
    11. [Emacs OS](#org8efcd6b)
        1.  [elfeed](#org2e13eb5)
        2.  [stock-ticker](#orgff68dc5)



<a id="orgb45ac38"></a>

# Initial setup

Add to .emacs:

(load-file "~/.emacs.d/init.el")


<a id="orgbd480d4"></a>

# Kostafey's keybindings


<a id="org06d0d29"></a>

## Humane emacs


<a id="org9322619"></a>

### Exit/hide emacs

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">M-z</td>
<td class="org-left">iconify-or-deiconify-frame</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">M-&lt;f4&gt;</td>
<td class="org-left">save-buffers-kill-terminal</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>
</table>


<a id="org0d031cc"></a>

### Select Copy Paste

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-S-v</td>
<td class="org-left">cua-paste-pop</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-M-v</td>
<td class="org-left">(cua-paste-pop -1)</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-M-c</td>
<td class="org-left">lambda</td>
<td class="org-left">Append selected text to temp buffer</td>
</tr>


<tr>
<td class="org-left">C-e</td>
<td class="org-left">cua-exchange-point-and-mark</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-S-e</td>
<td class="org-left">(cua-exchange-point-and-mark 1)</td>
<td class="org-left">Exchange and select</td>
</tr>


<tr>
<td class="org-left">C-a</td>
<td class="org-left">mark-whole-buffer</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">M-s</td>
<td class="org-left">set-mark-command</td>
<td class="org-left">(As old C-SPC)</td>
</tr>
</tbody>
</table>


<a id="org11105d3"></a>

### Undo/redo

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-z</td>
<td class="org-left">undo</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-S-z</td>
<td class="org-left">redo</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-'</td>
<td class="org-left">repeat</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>
</table>


<a id="orgfd1a0f7"></a>

### Delete/insert char

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-b</td>
<td class="org-left">backward-delete-char</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-d</td>
<td class="org-left">delete-char</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-q</td>
<td class="org-left">quoted-insert</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>
</table>


<a id="orgb753a61"></a>

### Save/revert

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-s</td>
<td class="org-left">save-buffer</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-x r</td>
<td class="org-left">revert-buffer</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-x RET r</td>
<td class="org-left">revert-buffer-with-coding-system</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>
</table>


<a id="org69b5666"></a>

## Basic point movements & change buffer's position

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-&lt;right&gt;</td>
<td class="org-left">step-forward-word</td>
<td class="org-left">Like odinary editors, moves</td>
</tr>


<tr>
<td class="org-left">C-&lt;left&gt;</td>
<td class="org-left">step-backward-word</td>
<td class="org-left">forward word/backward word.</td>
</tr>


<tr>
<td class="org-left">C-M-&lt;down&gt;</td>
<td class="org-left">forward-sentence</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-M-&lt;up&gt;</td>
<td class="org-left">backward-sentence</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>
</table>


<a id="org9f87c5a"></a>

## Point hyper-jumps


<a id="orgf098428"></a>

### Bookmarks

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-b</td>
<td class="org-left">bookmark-set</td>
</tr>


<tr>
<td class="org-left">M-b</td>
<td class="org-left">bookmark-jump</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">C-x x</td>
<td class="org-left">goto-last-change</td>
</tr>


<tr>
<td class="org-left">C-c left</td>
<td class="org-left">winner-undo</td>
</tr>


<tr>
<td class="org-left">C-c right</td>
<td class="org-left">winner-redo</td>
</tr>
</tbody>
</table>


<a id="orgd8e0a4a"></a>

### Search & replace

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-f</td>
<td class="org-left">isearch-forward</td>
</tr>


<tr>
<td class="org-left">C-r</td>
<td class="org-left">isearch-backward</td>
</tr>


<tr>
<td class="org-left">M-e</td>
<td class="org-left">isearch-edit-string</td>
</tr>


<tr>
<td class="org-left">C-S-f</td>
<td class="org-left">flx-isearch-forward</td>
</tr>


<tr>
<td class="org-left">C-S-r</td>
<td class="org-left">flx-isearch-backward</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">M-r</td>
<td class="org-left">replace-string</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">C-M-f</td>
<td class="org-left">ag</td>
</tr>


<tr>
<td class="org-left">C-c C-f</td>
<td class="org-left">ack-file</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">control F3</td>
<td class="org-left">highlight-symbol-at-point</td>
</tr>


<tr>
<td class="org-left">F3</td>
<td class="org-left">highlight-symbol-next</td>
</tr>


<tr>
<td class="org-left">shift F3</td>
<td class="org-left">highlight-symbol-prev</td>
</tr>


<tr>
<td class="org-left">meta F3</td>
<td class="org-left">highlight-symbol-remove-all</td>
</tr>


<tr>
<td class="org-left">C-M-<up></td>
<td class="org-left">highlight-symbol-prev</td>
</tr>


<tr>
<td class="org-left">C-M-<down></td>
<td class="org-left">highlight-symbol-next</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">M-a</td>
<td class="org-left">ace-jump-mode</td>
</tr>
</tbody>
</table>


<a id="orgdf0fcef"></a>

### Intellectual point jumps

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">sgml-pretty-print</td>
<td class="org-left">Format selected xml.</td>
</tr>


<tr>
<td class="org-left">C-n</td>
<td class="org-left">sgml-skip-tag-forward</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-b</td>
<td class="org-left">sgml-skip-tag-backward</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">C-M-d</td>
<td class="org-left">hop-at-point</td>
<td class="org-left">Jump to elisp definition</td>
</tr>
</tbody>
</table>


<a id="orgd7cf1e6"></a>

## Frames

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">s-tab</td>
<td class="org-left">other-frame</td>
</tr>
</tbody>
</table>


<a id="org9aaaef1"></a>

## Command executions

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">M-x</td>
<td class="org-left">smex</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">M-X</td>
<td class="org-left">smex-major-mode-commands</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-c C-c M-x</td>
<td class="org-left">execute-extended-command</td>
<td class="org-left">This is your old M-x</td>
</tr>
</tbody>
</table>


<a id="org0c421d1"></a>

## Text transformations


<a id="orgd05774f"></a>

### Basic text transformations

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-n</td>
<td class="org-left">newline</td>
</tr>


<tr>
<td class="org-left">C-o</td>
<td class="org-left">open-line</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">M-t</td>
<td class="org-left">transpose-words</td>
</tr>


<tr>
<td class="org-left">M-y</td>
<td class="org-left">transpose-words -1</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">C-j</td>
<td class="org-left">join-next-line-space-n</td>
</tr>


<tr>
<td class="org-left">C-c j</td>
<td class="org-left">join-next-line-n</td>
</tr>


<tr>
<td class="org-left">C-c C-j</td>
<td class="org-left">join-next-line-semicolon-n</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">C-c c</td>
<td class="org-left">center-line</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">C-M-k</td>
<td class="org-left">kill-whole-line</td>
</tr>


<tr>
<td class="org-left">C-k</td>
<td class="org-left">kill-line</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">C-S-c</td>
<td class="org-left">copy-line</td>
</tr>


<tr>
<td class="org-left">C-S-l</td>
<td class="org-left">mark-line</td>
</tr>


<tr>
<td class="org-left">C-c u</td>
<td class="org-left">copy-url</td>
</tr>


<tr>
<td class="org-left">C-c d</td>
<td class="org-left">duplicate-line</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">C-c C-l</td>
<td class="org-left">toggle-truncate-lines</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">C-c q</td>
<td class="org-left">unfill-paragraph</td>
</tr>
</tbody>
</table>


<a id="orge853967"></a>

### Rectangle operations

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-M-a n</td>
<td class="org-left">rectangle-number-lines</td>
</tr>


<tr>
<td class="org-left">C-M-a v</td>
<td class="org-left">string-insert-rectangle</td>
</tr>


<tr>
<td class="org-left">C-M-a c</td>
<td class="org-left">copy-rectangle-to-clipboard</td>
</tr>


<tr>
<td class="org-left">C-M-a r</td>
<td class="org-left">yank-rectangle</td>
</tr>


<tr>
<td class="org-left">M-u</td>
<td class="org-left">cua-upcase-rectangle</td>
</tr>
</tbody>
</table>


<a id="org8c5b31b"></a>

### Upcase/downcase

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-S-<up></td>
<td class="org-left">toggle-letter-case</td>
</tr>
</tbody>
</table>


<a id="org2f36b59"></a>

### Region & misc operations

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-M-a :</td>
<td class="org-left">align-by-column</td>
</tr>


<tr>
<td class="org-left">C-M-a '</td>
<td class="org-left">align-by-quote</td>
</tr>


<tr>
<td class="org-left">align-regexp</td>
<td class="org-left">align-regexp</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">C-;</td>
<td class="org-left">comment-or-uncomment-this</td>
</tr>


<tr>
<td class="org-left">C-/</td>
<td class="org-left">comment-or-uncomment-this</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">C-\`</td>
<td class="org-left">u:en/ru-recode-region</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">C-M-R</td>
<td class="org-left">replace-regexp</td>
</tr>


<tr>
<td class="org-left">M-R</td>
<td class="org-left">query-replace</td>
</tr>


<tr>
<td class="org-left">C-M-a k</td>
<td class="org-left">keep-lines</td>
</tr>


<tr>
<td class="org-left">C-M-a f</td>
<td class="org-left">flush-lines</td>
</tr>
</tbody>
</table>


<a id="orgc739cdc"></a>

### Buffers navigation

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">control next</td>
<td class="org-left">my-next-buffer</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">control prior</td>
<td class="org-left">my-previous-buffer</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-x a s</td>
<td class="org-left">find-file-from-clipboard</td>
<td class="org-left">Open file or directory path</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">from clipboard (kill ring)</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">if path exists.</td>
</tr>
</tbody>
</table>


<a id="org36ab172"></a>

### ASCII graphic & formatting notes

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">markdown-insert-header-setext-1</td>
<td class="org-left">Add double underline to text</td>
</tr>


<tr>
<td class="org-left">markdown-insert-header-setext-2</td>
<td class="org-left">Add underline to text</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">comment-box</td>
<td class="org-left">Comment region, putting it inside a box</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">picture-draw-rectangle</td>
<td class="org-left">Draw rectangle around rectangle-mark</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">selection</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">table-insert</td>
<td class="org-left">Table creation & manipulation</td>
</tr>


<tr>
<td class="org-left">table-recognize</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">table-unrecognize</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">\*table&#x2013;cell-center-paragraph</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">center-line</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">center-region</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>
</table>


<a id="org94cf3a2"></a>

### Paredit customization

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">M-S-(</td>
<td class="org-left">paredit-wrap-round</td>
<td class="org-left">(foo  #bar baz)</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#x2014;></td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">(foo (#bar) baz)</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">M-S-s</td>
<td class="org-left">paredit-splice-sexp</td>
<td class="org-left">(foo (bar# baz) quux)</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#x2014;></td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">(foo bar# baz quux)</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">C-S-)</td>
<td class="org-left">paredit-forward-slurp-sexp</td>
<td class="org-left">(foo (bar #baz) quux zot)</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#x2014;></td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">(foo (bar #baz quux) zot)</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">C-S-}</td>
<td class="org-left">paredit-forward-barf-sexp</td>
<td class="org-left">(foo (bar #baz quux) zot)</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#x2014;></td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">(foo (bar #baz) quux zot)</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">C-S-(</td>
<td class="org-left">paredit-backward-slurp-sexp</td>
<td class="org-left">(foo bar (baz# quux) zot)</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#x2014;></td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">(foo (bar baz# quux) zot)</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">C-S-{</td>
<td class="org-left">paredit-backward-barf-sexp</td>
<td class="org-left">(foo (bar baz #quux) zot)</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#x2014;></td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">(foo bar (baz #quux) zot)</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">ESC <up></td>
<td class="org-left">paredit-splice-sexp-killing-backward</td>
<td class="org-left">(foo (bar #(sqrt n)))</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#x2013;></td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">(foo #(sqrt n))</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">M-s-<right></td>
<td class="org-left">transpose-sexps</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">M-s-<left></td>
<td class="org-left">(transpose-sexps -1)</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>
</table>


<a id="org01c6928"></a>

## IDE

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">s-s</td>
<td class="org-left">sr-speedbar-toggle</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-x B</td>
<td class="org-left">semantic-mrub-switch-tag</td>
<td class="org-left">Backward after semantic-ia-fast-jump</td>
</tr>
</tbody>
</table>


<a id="org03afe10"></a>

### Speedbar


<a id="org4c9d33a"></a>

### Common prog mode keys

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">M-S-<left></td>
<td class="org-left">hop-backward</td>
<td class="org-left">Hop backward if M-<left> is uncertain</td>
</tr>


<tr>
<td class="org-left">M-S-<right></td>
<td class="org-left">hop-forward</td>
<td class="org-left">Hop forward if M-<right> is uncertain</td>
</tr>
</tbody>
</table>


<a id="org2fd40af"></a>

### Java


<a id="orga3ff920"></a>

### Lisp

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-c C-k</td>
<td class="org-left">slime-compile-and-load-file</td>
</tr>


<tr>
<td class="org-left">C-c h</td>
<td class="org-left">slime-hyperspec-lookup</td>
</tr>
</tbody>
</table>


<a id="orga013c07"></a>

### Emacs Lisp

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-c C-p</td>
<td class="org-left">k/el-pprint-eval-last-sexp</td>
</tr>
</tbody>
</table>


<a id="org43739d3"></a>

### Clojure


<a id="orgf48ea7c"></a>

### Lua

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-c C-l</td>
<td class="org-left">lua-send-buffer</td>
</tr>


<tr>
<td class="org-left">C-c C-f</td>
<td class="org-left">lua-search-documentation</td>
</tr>


<tr>
<td class="org-left">C-c C-c</td>
<td class="org-left">lua-send-current-line</td>
</tr>


<tr>
<td class="org-left">M-e</td>
<td class="org-left">lua-send-region</td>
</tr>
</tbody>
</table>


<a id="org51d720c"></a>

### Scala

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-n j</td>
<td class="org-left">ensime</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-c C-v s</td>
<td class="org-left">ensime-sbt-switch</td>
<td class="org-left">Switch to the sbt shell</td>
</tr>


<tr>
<td class="org-left">C-n s</td>
<td class="org-left">ensime-sbt-switch</td>
<td class="org-left">Switch to the sbt shell</td>
</tr>


<tr>
<td class="org-left">C-c C-v z</td>
<td class="org-left">ensime-inf-switch</td>
<td class="org-left">Start/switch to scala REPL</td>
</tr>


<tr>
<td class="org-left">C-n c</td>
<td class="org-left">ensime-inf-switch</td>
<td class="org-left">Start/switch to scala REPL</td>
</tr>


<tr>
<td class="org-left">C-c C-v e</td>
<td class="org-left">ensime-print-errors-at-point</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-M-/</td>
<td class="org-left">ensime-print-errors-at-point</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-c C-v t</td>
<td class="org-left">ensime-type-at-point</td>
<td class="org-left">Echo the type at point to the minibuffer</td>
</tr>


<tr>
<td class="org-left">M-=</td>
<td class="org-left">ensime-type-at-point</td>
<td class="org-left">Echo the type at point to the minibuffer</td>
</tr>


<tr>
<td class="org-left">C-c C-v b</td>
<td class="org-left">ensime-inf-eval-buffer</td>
<td class="org-left">Send whole buffer to Scala interpreter</td>
</tr>


<tr>
<td class="org-left">C-n e b</td>
<td class="org-left">k/ensime-eval-buffer</td>
<td class="org-left">Send whole buffer to Scala interpreter</td>
</tr>


<tr>
<td class="org-left">C-c C-r</td>
<td class="org-left">ensime-inf-eval-region</td>
<td class="org-left">Send current region to Scala interpreter</td>
</tr>


<tr>
<td class="org-left">M-e</td>
<td class="org-left">ensime-inf-eval-region</td>
<td class="org-left">Send current region to Scala interpreter</td>
</tr>


<tr>
<td class="org-left">C-x C-e</td>
<td class="org-left">k/ensime-eval-last-scala-expr</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-n q</td>
<td class="org-left">k/ensime-quit</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-n k</td>
<td class="org-left">k/ensime-compile</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-M-d</td>
<td class="org-left">hop-at-point</td>
<td class="org-left">Jump to definition</td>
</tr>


<tr>
<td class="org-left">C-c i</td>
<td class="org-left">ensime-import-type-at-point</td>
<td class="org-left">Suggest possible imports.</td>
</tr>
</tbody>
</table>


<a id="org0e6e827"></a>

### Tcl

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-c C-t</td>
<td class="org-left">inferior-tcl</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-c C-f</td>
<td class="org-left">tcl-load-file</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-c C-v</td>
<td class="org-left">tcl-eval-defun</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">C-c C-x</td>
<td class="org-left">tcl-eval-region</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">M-e</td>
<td class="org-left">tcl-eval-region</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>
</table>


<a id="org585d4c7"></a>

### Golang


<a id="org1148258"></a>

### reStructuredText


<a id="org1d073db"></a>

### SQL


<a id="org91a8d93"></a>

### Version control


<a id="org2ab03cc"></a>

#### Magit & ahg

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-w</td>
<td class="org-left">prh:kill-current-buffer</td>
<td class="org-left">kill current buffer</td>
</tr>


<tr>
<td class="org-left">M-w</td>
<td class="org-left">get-vc-status</td>
<td class="org-left"><b>prog-mode</b>: git or hg status</td>
</tr>


<tr>
<td class="org-left">M-w</td>
<td class="org-left">diffview-current</td>
<td class="org-left"><b>magit-mode</b>: two-window</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">(side-by-side) comparsion</td>
</tr>


<tr>
<td class="org-left">S-M-w</td>
<td class="org-left">magit-copy-buffer-revision</td>
<td class="org-left">get buffer revision</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">magit-log-buffer-file</td>
<td class="org-left">Show log for the blob or file</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">visited in the current buffer.</td>
</tr>
</tbody>
</table>


<a id="orgd0a399e"></a>

#### git-gutter

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-M-g &lt;down&gt;</td>
<td class="org-left">git-gutter:next-hunk</td>
</tr>


<tr>
<td class="org-left">C-M-g &lt;down&gt;</td>
<td class="org-left">git-gutter:previous-hunk</td>
</tr>


<tr>
<td class="org-left">C-M-g p</td>
<td class="org-left">git-gutter:popup-hunk</td>
</tr>
</tbody>
</table>


<a id="orgd079935"></a>

## Mouse


<a id="org6353ec1"></a>

## Menu


<a id="org650f6ff"></a>

### header<sub>name</sub>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">M-\`</td>
<td class="org-left">tmm-menubar</td>
<td class="org-left">Text menu</td>
</tr>
</tbody>
</table>


<a id="org0d778f0"></a>

## Org-mode


<a id="org6ce2e71"></a>

### Time schedule

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-c C-t</td>
<td class="org-left">org-todo</td>
<td class="org-left">Change the TODO state of an item</td>
</tr>


<tr>
<td class="org-left">C-c C-x C-i</td>
<td class="org-left">org-clock-in</td>
<td class="org-left">Start the clock on the current item</td>
</tr>


<tr>
<td class="org-left">C-c C-x C-o</td>
<td class="org-left">org-clock-out</td>
<td class="org-left">Stop the currently running clock</td>
</tr>


<tr>
<td class="org-left">C-c C-x C-r</td>
<td class="org-left">org-clock-report</td>
<td class="org-left">Create a table containing a report</td>
</tr>


<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
<td class="org-left">about clocked time</td>
</tr>
</tbody>
</table>


<a id="org8efcd6b"></a>

## Emacs OS


<a id="org2e13eb5"></a>

### elfeed


<a id="orgff68dc5"></a>

### stock-ticker

