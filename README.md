<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Kostafey's keybindings</a>
<ul>
<li><a href="#sec-1-1">1.1. Humane emacs</a>
<ul>
<li><a href="#sec-1-1-1">1.1.1. Exit/hide emacs</a></li>
<li><a href="#sec-1-1-2">1.1.2. Select Copy Paste</a></li>
<li><a href="#sec-1-1-3">1.1.3. Undo/redo</a></li>
<li><a href="#sec-1-1-4">1.1.4. Delete/insert char</a></li>
<li><a href="#sec-1-1-5">1.1.5. Save/revert</a></li>
</ul>
</li>
<li><a href="#sec-1-2">1.2. Basic point movements &amp; change buffer's position</a></li>
<li><a href="#sec-1-3">1.3. Point hyper-jumps</a>
<ul>
<li><a href="#sec-1-3-1">1.3.1. Bookmarks</a></li>
<li><a href="#sec-1-3-2">1.3.2. Search &amp; replace</a></li>
<li><a href="#sec-1-3-3">1.3.3. Intellectual point jumps</a></li>
</ul>
</li>
<li><a href="#sec-1-4">1.4. Command executions</a></li>
<li><a href="#sec-1-5">1.5. Text transformations</a>
<ul>
<li><a href="#sec-1-5-1">1.5.1. Basic text transformations</a></li>
<li><a href="#sec-1-5-2">1.5.2. Rectangle operations</a></li>
<li><a href="#sec-1-5-3">1.5.3. Upcase/downcase</a></li>
<li><a href="#sec-1-5-4">1.5.4. Region &amp; misc operations</a></li>
</ul>
</li>
<li><a href="#sec-1-6">1.6. IDE</a></li>
<li><a href="#sec-1-7">1.7. Menu</a>
<ul>
<li><a href="#sec-1-7-1">1.7.1. header<sub>name</sub></a></li>
</ul>
</li>
<li><a href="#sec-1-8">1.8. Org-mode</a>
<ul>
<li><a href="#sec-1-8-1">1.8.1. Time schedule</a></li>
</ul>
</li>
</ul>
</li>
</ul>
</div>
</div>

# Kostafey's keybindings

## Humane emacs

### Exit/hide emacs

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Key</th>
<th scope="col" class="left">Command</th>
<th scope="col" class="left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">M-z</td>
<td class="left">iconify-or-deiconify-frame</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">M-&lt;f4&gt;</td>
<td class="left">save-buffers-kill-terminal</td>
<td class="left">&#xa0;</td>
</tr>
</tbody>
</table>

### Select Copy Paste

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Key</th>
<th scope="col" class="left">Command</th>
<th scope="col" class="left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">C-S-v</td>
<td class="left">cua-paste-pop</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-M-v</td>
<td class="left">(cua-paste-pop -1)</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-M-c</td>
<td class="left">lambda</td>
<td class="left">Append selected text to temp buffer</td>
</tr>


<tr>
<td class="left">C-e</td>
<td class="left">cua-exchange-point-and-mark</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-S-e</td>
<td class="left">(cua-exchange-point-and-mark 1)</td>
<td class="left">Exchange and select</td>
</tr>


<tr>
<td class="left">C-a</td>
<td class="left">mark-whole-buffer</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">M-s</td>
<td class="left">set-mark-command</td>
<td class="left">(As old C-SPC)</td>
</tr>
</tbody>
</table>

### Undo/redo

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Key</th>
<th scope="col" class="left">Command</th>
<th scope="col" class="left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">C-z</td>
<td class="left">undo</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-S-z</td>
<td class="left">redo</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-'</td>
<td class="left">repeat</td>
<td class="left">&#xa0;</td>
</tr>
</tbody>
</table>

### Delete/insert char

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Key</th>
<th scope="col" class="left">Command</th>
<th scope="col" class="left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">C-b</td>
<td class="left">backward-delete-char</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-d</td>
<td class="left">delete-char</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-q</td>
<td class="left">quoted-insert</td>
<td class="left">&#xa0;</td>
</tr>
</tbody>
</table>

### Save/revert

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Key</th>
<th scope="col" class="left">Command</th>
<th scope="col" class="left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">C-s</td>
<td class="left">save-buffer</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-x r</td>
<td class="left">revert-buffer</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-x RET r</td>
<td class="left">revert-buffer-with-coding-system</td>
<td class="left">&#xa0;</td>
</tr>
</tbody>
</table>

## Basic point movements & change buffer's position

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Key</th>
<th scope="col" class="left">Command</th>
<th scope="col" class="left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">C-&lt;right&gt;</td>
<td class="left">step-forward-word</td>
<td class="left">Like odinary editors, moves</td>
</tr>


<tr>
<td class="left">C-&lt;left&gt;</td>
<td class="left">step-backward-word</td>
<td class="left">forward word/backward word.</td>
</tr>


<tr>
<td class="left">C-M-&lt;down&gt;</td>
<td class="left">forward-sentence</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-M-&lt;up&gt;</td>
<td class="left">backward-sentence</td>
<td class="left">&#xa0;</td>
</tr>
</tbody>
</table>

## Point hyper-jumps

### Bookmarks

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Key</th>
<th scope="col" class="left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">C-b</td>
<td class="left">bookmark-set</td>
</tr>


<tr>
<td class="left">M-b</td>
<td class="left">bookmark-jump</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">C-x x</td>
<td class="left">goto-last-change</td>
</tr>


<tr>
<td class="left">C-c left</td>
<td class="left">winner-undo</td>
</tr>


<tr>
<td class="left">C-c right</td>
<td class="left">winner-redo</td>
</tr>
</tbody>
</table>

### Search & replace

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Key</th>
<th scope="col" class="left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">C-f</td>
<td class="left">isearch-forward</td>
</tr>


<tr>
<td class="left">C-r</td>
<td class="left">isearch-backward</td>
</tr>


<tr>
<td class="left">M-e</td>
<td class="left">isearch-edit-string</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">M-r</td>
<td class="left">replace-string</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">C-M-f</td>
<td class="left">ack</td>
</tr>


<tr>
<td class="left">C-c C-f</td>
<td class="left">ack-file</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">control F3</td>
<td class="left">highlight-symbol-at-point</td>
</tr>


<tr>
<td class="left">F3</td>
<td class="left">highlight-symbol-next</td>
</tr>


<tr>
<td class="left">shift F3</td>
<td class="left">highlight-symbol-prev</td>
</tr>


<tr>
<td class="left">meta F3</td>
<td class="left">highlight-symbol-remove-all</td>
</tr>
</tbody>
</table>

### Intellectual point jumps

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Key</th>
<th scope="col" class="left">Command</th>
<th scope="col" class="left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">&#xa0;</td>
<td class="left">sgml-pretty-print</td>
<td class="left">Format selected xml.</td>
</tr>


<tr>
<td class="left">C-n</td>
<td class="left">sgml-skip-tag-forward</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-b</td>
<td class="left">sgml-skip-tag-backward</td>
<td class="left">&#xa0;</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">C-M-d</td>
<td class="left">find-function-jump-at-point</td>
<td class="left">Jump to elisp definition</td>
</tr>
</tbody>
</table>

## Command executions

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Key</th>
<th scope="col" class="left">Command</th>
<th scope="col" class="left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">M-x</td>
<td class="left">smex</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">M-X</td>
<td class="left">smex-major-mode-commands</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-c C-c M-x</td>
<td class="left">execute-extended-command</td>
<td class="left">This is your old M-x</td>
</tr>
</tbody>
</table>

## Text transformations

### Basic text transformations

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Key</th>
<th scope="col" class="left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">C-n</td>
<td class="left">newline</td>
</tr>


<tr>
<td class="left">C-o</td>
<td class="left">open-line</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">M-t</td>
<td class="left">transpose-words</td>
</tr>


<tr>
<td class="left">M-y</td>
<td class="left">transpose-words -1</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">C-j</td>
<td class="left">join-next-line-space-n</td>
</tr>


<tr>
<td class="left">C-c j</td>
<td class="left">join-next-line-n</td>
</tr>


<tr>
<td class="left">C-c C-j</td>
<td class="left">join-next-line-semicolon-n</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">C-c c</td>
<td class="left">center-line</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">C-M-k</td>
<td class="left">kill-whole-line</td>
</tr>


<tr>
<td class="left">C-k</td>
<td class="left">kill-line</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">C-S-c</td>
<td class="left">copy-line</td>
</tr>


<tr>
<td class="left">C-S-l</td>
<td class="left">mark-line</td>
</tr>


<tr>
<td class="left">C-c u</td>
<td class="left">copy-url</td>
</tr>


<tr>
<td class="left">C-c d</td>
<td class="left">duplicate-line</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">C-c C-l</td>
<td class="left">toggle-truncate-lines</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">C-c q</td>
<td class="left">unfill-paragraph</td>
</tr>
</tbody>
</table>

### Rectangle operations

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Key</th>
<th scope="col" class="left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">C-M-a n</td>
<td class="left">rectangle-number-lines</td>
</tr>


<tr>
<td class="left">C-M-a v</td>
<td class="left">string-insert-rectangle</td>
</tr>


<tr>
<td class="left">C-M-a c</td>
<td class="left">copy-rectangle-to-clipboard</td>
</tr>


<tr>
<td class="left">C-M-a r</td>
<td class="left">yank-rectangle</td>
</tr>


<tr>
<td class="left">M-u</td>
<td class="left">cua-upcase-rectangle</td>
</tr>
</tbody>
</table>

### Upcase/downcase

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Key</th>
<th scope="col" class="left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">C-S-<up></td>
<td class="left">toggle-letter-case</td>
</tr>
</tbody>
</table>

### Region & misc operations

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Key</th>
<th scope="col" class="left">Command</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">C-M-a :</td>
<td class="left">align-by-column</td>
</tr>


<tr>
<td class="left">C-M-a '</td>
<td class="left">align-by-quote</td>
</tr>


<tr>
<td class="left">align-regexp</td>
<td class="left">align-regexp</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">C-;</td>
<td class="left">comment-or-uncomment-this</td>
</tr>


<tr>
<td class="left">C-/</td>
<td class="left">comment-or-uncomment-this</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">C-\`</td>
<td class="left">u:en/ru-recode-region</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">C-M-R</td>
<td class="left">replace-regexp</td>
</tr>


<tr>
<td class="left">M-R</td>
<td class="left">query-replace</td>
</tr>


<tr>
<td class="left">C-M-a k</td>
<td class="left">keep-lines</td>
</tr>


<tr>
<td class="left">C-M-a f</td>
<td class="left">flush-lines</td>
</tr>
</tbody>
</table>

## IDE

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Key</th>
<th scope="col" class="left">Command</th>
<th scope="col" class="left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">s-s</td>
<td class="left">sr-speedbar-toggle</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-x B</td>
<td class="left">semantic-mrub-switch-tag</td>
<td class="left">Backward after semantic-ia-fast-jump</td>
</tr>
</tbody>
</table>

## Menu

### header<sub>name</sub>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Key</th>
<th scope="col" class="left">Command</th>
<th scope="col" class="left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">M-\`</td>
<td class="left">tmm-menubar</td>
<td class="left">Text menu</td>
</tr>
</tbody>
</table>

## Org-mode

### Time schedule

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Key</th>
<th scope="col" class="left">Command</th>
<th scope="col" class="left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">C-c C-t</td>
<td class="left">org-todo</td>
<td class="left">Change the TODO state of an item</td>
</tr>


<tr>
<td class="left">C-c C-x C-i</td>
<td class="left">org-clock-in</td>
<td class="left">Start the clock on the current item</td>
</tr>


<tr>
<td class="left">C-c C-x C-o</td>
<td class="left">org-clock-out</td>
<td class="left">Stop the currently running clock</td>
</tr>


<tr>
<td class="left">C-c C-x C-r</td>
<td class="left">org-clock-report</td>
<td class="left">Create a table containing a report</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">about clocked time</td>
</tr>
</tbody>
</table>
