
# Table of Contents

1.  [Initial setup](#org6e4ca97)
2.  [Kostafey's keybindings](#orga4cd4b3)
    1.  [Humane emacs](#org5f0fa71)
        1.  [Exit/hide emacs](#orga411041)
        2.  [Select Copy Paste](#org2070701)
        3.  [Undo/redo](#org4f1378a)
        4.  [Delete/insert char](#org87503df)
        5.  [Save/revert](#orgc7a5e25)
    2.  [Basic point movements & change buffer's position](#orgef67a04)
    3.  [Point hyper-jumps](#orgee8b79f)
        1.  [Bookmarks](#org2a72f35)
        2.  [Search & replace](#org20f3075)
        3.  [Intellectual point jumps](#org58b1f0a)
    4.  [Frames](#org96e1639)
    5.  [Command executions](#orgfe9b44e)
    6.  [Text transformations](#orgd990584)
        1.  [Basic text transformations](#org651c567)
        2.  [Rectangle operations](#orgd892d55)
        3.  [Upcase/downcase](#orgf732fa4)
        4.  [Region & misc operations](#orga9fe8f4)
        5.  [Buffers navigation](#orgb685f9b)
        6.  [ASCII graphic & formatting notes](#org574b27a)
        7.  [Paredit customization](#org8b379e9)
    7.  [IDE](#orgfc8fb71)
        1.  [Speedbar](#org43cc4ac)
        2.  [Common prog mode keys](#orge832102)
        3.  [Java](#orge6fd81f)
        4.  [Lisp](#org2da9f3c)
        5.  [Emacs Lisp](#orgd9986ac)
        6.  [Clojure](#org66a54e1)
        7.  [Lua](#org36a533c)
        8.  [Scala](#org5463c70)
        9.  [Tcl](#org4348c8b)
        10. [Golang](#org2fdc238)
        11. [reStructuredText](#orgf75aeea)
        12. [SQL](#orgc5d3d4b)
        13. [Version control](#org252a8b4)
            1.  [Magit & ahg](#orgb7c52db)
            2.  [git-gutter](#org5f65391)
    8.  [Mouse](#orgfc89b10)
    9.  [Menu](#orgbc3206f)
        1.  [header<sub>name</sub>](#orgb7350cb)
    10. [Org-mode](#org290006d)
        1.  [Time schedule](#org9cd0c56)
    11. [Emacs OS](#orge3578c5)
        1.  [elfeed](#org7f5aa1f)
        2.  [stock-ticker](#org4899544)



<a id="org6e4ca97"></a>

# Initial setup

Add to .emacs:

(load-file "~/.emacs.d/init.el")


<a id="orga4cd4b3"></a>

# Kostafey's keybindings


<a id="org5f0fa71"></a>

## Humane emacs


<a id="orga411041"></a>

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


<a id="org2070701"></a>

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


<a id="org4f1378a"></a>

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


<a id="org87503df"></a>

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


<a id="orgc7a5e25"></a>

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


<a id="orgef67a04"></a>

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


<a id="orgee8b79f"></a>

## Point hyper-jumps


<a id="org2a72f35"></a>

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


<a id="org20f3075"></a>

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
<td class="org-left">highlight-symbol</td>
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


<a id="org58b1f0a"></a>

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


<a id="org96e1639"></a>

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


<a id="orgfe9b44e"></a>

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


<a id="orgd990584"></a>

## Text transformations


<a id="org651c567"></a>

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


<a id="orgd892d55"></a>

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


<a id="orgf732fa4"></a>

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


<tr>
<td class="org-left">C-S-<down></td>
<td class="org-left">toggle-camelcase-underscores</td>
</tr>
</tbody>
</table>


<a id="orga9fe8f4"></a>

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


<a id="orgb685f9b"></a>

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


<a id="org574b27a"></a>

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


<a id="org8b379e9"></a>

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


<a id="orgfc8fb71"></a>

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


<a id="org43cc4ac"></a>

### Speedbar


<a id="orge832102"></a>

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


<a id="orge6fd81f"></a>

### Java


<a id="org2da9f3c"></a>

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


<a id="orgd9986ac"></a>

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

<tbody>
<tr>
<td class="org-left">C-n e b</td>
<td class="org-left">eval-buffer</td>
</tr>
</tbody>
</table>


<a id="org66a54e1"></a>

### Clojure


<a id="org36a533c"></a>

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


<a id="org5463c70"></a>

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


<a id="org4348c8b"></a>

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


<a id="org2fdc238"></a>

### Golang


<a id="orgf75aeea"></a>

### reStructuredText


<a id="orgc5d3d4b"></a>

### SQL


<a id="org252a8b4"></a>

### Version control


<a id="orgb7c52db"></a>

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


<a id="org5f65391"></a>

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


<a id="orgfc89b10"></a>

## Mouse


<a id="orgbc3206f"></a>

## Menu


<a id="orgb7350cb"></a>

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


<a id="org290006d"></a>

## Org-mode


<a id="org9cd0c56"></a>

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


<a id="orge3578c5"></a>

## Emacs OS


<a id="org7f5aa1f"></a>

### elfeed


<a id="org4899544"></a>

### stock-ticker

