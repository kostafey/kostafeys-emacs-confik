<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Initial setup</a></li>
<li><a href="#sec-2">2. Kostafey's keybindings</a>
<ul>
<li><a href="#sec-2-1">2.1. Humane emacs</a>
<ul>
<li><a href="#sec-2-1-1">2.1.1. Exit/hide emacs</a></li>
<li><a href="#sec-2-1-2">2.1.2. Select Copy Paste</a></li>
<li><a href="#sec-2-1-3">2.1.3. Undo/redo</a></li>
<li><a href="#sec-2-1-4">2.1.4. Delete/insert char</a></li>
<li><a href="#sec-2-1-5">2.1.5. Save/revert</a></li>
</ul>
</li>
<li><a href="#sec-2-2">2.2. Basic point movements &amp; change buffer's position</a></li>
<li><a href="#sec-2-3">2.3. Point hyper-jumps</a>
<ul>
<li><a href="#sec-2-3-1">2.3.1. Bookmarks</a></li>
<li><a href="#sec-2-3-2">2.3.2. Search &amp; replace</a></li>
<li><a href="#sec-2-3-3">2.3.3. Intellectual point jumps</a></li>
</ul>
</li>
<li><a href="#sec-2-4">2.4. Frames</a></li>
<li><a href="#sec-2-5">2.5. Command executions</a></li>
<li><a href="#sec-2-6">2.6. Text transformations</a>
<ul>
<li><a href="#sec-2-6-1">2.6.1. Basic text transformations</a></li>
<li><a href="#sec-2-6-2">2.6.2. Rectangle operations</a></li>
<li><a href="#sec-2-6-3">2.6.3. Upcase/downcase</a></li>
<li><a href="#sec-2-6-4">2.6.4. Region &amp; misc operations</a></li>
<li><a href="#sec-2-6-5">2.6.5. Buffers navigation</a></li>
<li><a href="#sec-2-6-6">2.6.6. ASCII graphic &amp; formatting notes</a></li>
<li><a href="#sec-2-6-7">2.6.7. Paredit customization</a></li>
</ul>
</li>
<li><a href="#sec-2-7">2.7. IDE</a>
<ul>
<li><a href="#sec-2-7-1">2.7.1. Speedbar</a></li>
<li><a href="#sec-2-7-2">2.7.2. Common prog mode keys</a></li>
<li><a href="#sec-2-7-3">2.7.3. Java</a></li>
<li><a href="#sec-2-7-4">2.7.4. Lisp</a></li>
<li><a href="#sec-2-7-5">2.7.5. Emacs Lisp</a></li>
<li><a href="#sec-2-7-6">2.7.6. Clojure</a></li>
<li><a href="#sec-2-7-7">2.7.7. Lua</a></li>
<li><a href="#sec-2-7-8">2.7.8. Scala</a></li>
<li><a href="#sec-2-7-9">2.7.9. Tcl</a></li>
<li><a href="#sec-2-7-10">2.7.10. Golang</a></li>
<li><a href="#sec-2-7-11">2.7.11. reStructuredText</a></li>
<li><a href="#sec-2-7-12">2.7.12. SQL</a></li>
<li><a href="#sec-2-7-13">2.7.13. Version control</a>
<ul>
<li><a href="#sec-2-7-13-1">2.7.13.1. Magit &amp; ahg</a></li>
<li><a href="#sec-2-7-13-2">2.7.13.2. git-gutter</a></li>
</ul>
</li>
</ul>
</li>
<li><a href="#sec-2-8">2.8. Mouse</a></li>
<li><a href="#sec-2-9">2.9. Menu</a>
<ul>
<li><a href="#sec-2-9-1">2.9.1. header<sub>name</sub></a></li>
</ul>
</li>
<li><a href="#sec-2-10">2.10. Org-mode</a>
<ul>
<li><a href="#sec-2-10-1">2.10.1. Time schedule</a></li>
</ul>
</li>
<li><a href="#sec-2-11">2.11. Emacs OS</a>
<ul>
<li><a href="#sec-2-11-1">2.11.1. elfeed</a></li>
<li><a href="#sec-2-11-2">2.11.2. stock-ticker</a></li>
</ul>
</li>
</ul>
</li>
</ul>
</div>
</div>


# Initial setup<a id="sec-1" name="sec-1"></a>

Add to .emacs:

(load-file "~/.emacs.d/init.el")

# Kostafey's keybindings<a id="sec-2" name="sec-2"></a>

## Humane emacs<a id="sec-2-1" name="sec-2-1"></a>

### Exit/hide emacs<a id="sec-2-1-1" name="sec-2-1-1"></a>

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

### Select Copy Paste<a id="sec-2-1-2" name="sec-2-1-2"></a>

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

### Undo/redo<a id="sec-2-1-3" name="sec-2-1-3"></a>

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

### Delete/insert char<a id="sec-2-1-4" name="sec-2-1-4"></a>

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

### Save/revert<a id="sec-2-1-5" name="sec-2-1-5"></a>

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

## Basic point movements & change buffer's position<a id="sec-2-2" name="sec-2-2"></a>

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

## Point hyper-jumps<a id="sec-2-3" name="sec-2-3"></a>

### Bookmarks<a id="sec-2-3-1" name="sec-2-3-1"></a>

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

### Search & replace<a id="sec-2-3-2" name="sec-2-3-2"></a>

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


<tr>
<td class="left">C-S-f</td>
<td class="left">flx-isearch-forward</td>
</tr>


<tr>
<td class="left">C-S-r</td>
<td class="left">flx-isearch-backward</td>
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
<td class="left">ag</td>
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


<tr>
<td class="left">C-M-<up></td>
<td class="left">highlight-symbol-prev</td>
</tr>


<tr>
<td class="left">C-M-<down></td>
<td class="left">highlight-symbol-next</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">M-a</td>
<td class="left">ace-jump-mode</td>
</tr>
</tbody>
</table>

### Intellectual point jumps<a id="sec-2-3-3" name="sec-2-3-3"></a>

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
<td class="left">hop-at-point</td>
<td class="left">Jump to elisp definition</td>
</tr>
</tbody>
</table>

## Frames<a id="sec-2-4" name="sec-2-4"></a>

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
<td class="left">s-tab</td>
<td class="left">other-frame</td>
</tr>
</tbody>
</table>

## Command executions<a id="sec-2-5" name="sec-2-5"></a>

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

## Text transformations<a id="sec-2-6" name="sec-2-6"></a>

### Basic text transformations<a id="sec-2-6-1" name="sec-2-6-1"></a>

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

### Rectangle operations<a id="sec-2-6-2" name="sec-2-6-2"></a>

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

### Upcase/downcase<a id="sec-2-6-3" name="sec-2-6-3"></a>

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

### Region & misc operations<a id="sec-2-6-4" name="sec-2-6-4"></a>

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

### Buffers navigation<a id="sec-2-6-5" name="sec-2-6-5"></a>

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
<td class="left">control next</td>
<td class="left">my-next-buffer</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">control prior</td>
<td class="left">my-previous-buffer</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-x a s</td>
<td class="left">find-file-from-clipboard</td>
<td class="left">Open file or directory path</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">from clipboard (kill ring)</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">if path exists.</td>
</tr>
</tbody>
</table>

### ASCII graphic & formatting notes<a id="sec-2-6-6" name="sec-2-6-6"></a>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Command</th>
<th scope="col" class="left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">markdown-insert-header-setext-1</td>
<td class="left">Add double underline to text</td>
</tr>


<tr>
<td class="left">markdown-insert-header-setext-2</td>
<td class="left">Add underline to text</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">comment-box</td>
<td class="left">Comment region, putting it inside a box</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">picture-draw-rectangle</td>
<td class="left">Draw rectangle around rectangle-mark</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">selection</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">table-insert</td>
<td class="left">Table creation & manipulation</td>
</tr>


<tr>
<td class="left">table-recognize</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">table-unrecognize</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">\*table&#x2013;cell-center-paragraph</td>
<td class="left">&#xa0;</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">center-line</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">center-region</td>
<td class="left">&#xa0;</td>
</tr>
</tbody>
</table>

### Paredit customization<a id="sec-2-6-7" name="sec-2-6-7"></a>

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
<td class="left">M-S-(</td>
<td class="left">paredit-wrap-round</td>
<td class="left">(foo  #bar baz)</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#x2014;></td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">(foo (#bar) baz)</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">M-S-s</td>
<td class="left">paredit-splice-sexp</td>
<td class="left">(foo (bar# baz) quux)</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#x2014;></td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">(foo bar# baz quux)</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">C-S-)</td>
<td class="left">paredit-forward-slurp-sexp</td>
<td class="left">(foo (bar #baz) quux zot)</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#x2014;></td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">(foo (bar #baz quux) zot)</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">C-S-}</td>
<td class="left">paredit-forward-barf-sexp</td>
<td class="left">(foo (bar #baz quux) zot)</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#x2014;></td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">(foo (bar #baz) quux zot)</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">C-S-(</td>
<td class="left">paredit-backward-slurp-sexp</td>
<td class="left">(foo bar (baz# quux) zot)</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#x2014;></td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">(foo (bar baz# quux) zot)</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">C-S-{</td>
<td class="left">paredit-backward-barf-sexp</td>
<td class="left">(foo (bar baz #quux) zot)</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#x2014;></td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">(foo bar (baz #quux) zot)</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">ESC <up></td>
<td class="left">paredit-splice-sexp-killing-backward</td>
<td class="left">(foo (bar #(sqrt n)))</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#x2013;></td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">(foo #(sqrt n))</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">M-s-<right></td>
<td class="left">transpose-sexps</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">M-s-<left></td>
<td class="left">(transpose-sexps -1)</td>
<td class="left">&#xa0;</td>
</tr>
</tbody>
</table>

## IDE<a id="sec-2-7" name="sec-2-7"></a>

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

### Speedbar<a id="sec-2-7-1" name="sec-2-7-1"></a>

### Common prog mode keys<a id="sec-2-7-2" name="sec-2-7-2"></a>

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
<td class="left">M-S-<left></td>
<td class="left">hop-backward</td>
<td class="left">Hop backward if M-<left> is uncertain</td>
</tr>


<tr>
<td class="left">M-S-<right></td>
<td class="left">hop-forward</td>
<td class="left">Hop forward if M-<right> is uncertain</td>
</tr>
</tbody>
</table>

### Java<a id="sec-2-7-3" name="sec-2-7-3"></a>

### Lisp<a id="sec-2-7-4" name="sec-2-7-4"></a>

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
<td class="left">C-c C-k</td>
<td class="left">slime-compile-and-load-file</td>
</tr>


<tr>
<td class="left">C-c h</td>
<td class="left">slime-hyperspec-lookup</td>
</tr>
</tbody>
</table>

### Emacs Lisp<a id="sec-2-7-5" name="sec-2-7-5"></a>

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
<td class="left">C-c C-p</td>
<td class="left">k/el-pprint-eval-last-sexp</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="left">C-n e b</td>
<td class="left">eval-buffer</td>
</tr>
</tbody>
</table>

### Clojure<a id="sec-2-7-6" name="sec-2-7-6"></a>

### Lua<a id="sec-2-7-7" name="sec-2-7-7"></a>

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
<td class="left">C-c C-l</td>
<td class="left">lua-send-buffer</td>
</tr>


<tr>
<td class="left">C-c C-f</td>
<td class="left">lua-search-documentation</td>
</tr>


<tr>
<td class="left">C-c C-c</td>
<td class="left">lua-send-current-line</td>
</tr>


<tr>
<td class="left">M-e</td>
<td class="left">lua-send-region</td>
</tr>
</tbody>
</table>

### Scala<a id="sec-2-7-8" name="sec-2-7-8"></a>

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
<td class="left">C-n j</td>
<td class="left">ensime</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-c C-v s</td>
<td class="left">ensime-sbt-switch</td>
<td class="left">Switch to the sbt shell</td>
</tr>


<tr>
<td class="left">C-n s</td>
<td class="left">ensime-sbt-switch</td>
<td class="left">Switch to the sbt shell</td>
</tr>


<tr>
<td class="left">C-c C-v z</td>
<td class="left">ensime-inf-switch</td>
<td class="left">Start/switch to scala REPL</td>
</tr>


<tr>
<td class="left">C-n c</td>
<td class="left">ensime-inf-switch</td>
<td class="left">Start/switch to scala REPL</td>
</tr>


<tr>
<td class="left">C-c C-v e</td>
<td class="left">ensime-print-errors-at-point</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-M-/</td>
<td class="left">ensime-print-errors-at-point</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-c C-v t</td>
<td class="left">ensime-type-at-point</td>
<td class="left">Echo the type at point to the minibuffer</td>
</tr>


<tr>
<td class="left">M-=</td>
<td class="left">ensime-type-at-point</td>
<td class="left">Echo the type at point to the minibuffer</td>
</tr>


<tr>
<td class="left">C-c C-v b</td>
<td class="left">ensime-inf-eval-buffer</td>
<td class="left">Send whole buffer to Scala interpreter</td>
</tr>


<tr>
<td class="left">C-n e b</td>
<td class="left">k/ensime-eval-buffer</td>
<td class="left">Send whole buffer to Scala interpreter</td>
</tr>


<tr>
<td class="left">C-c C-r</td>
<td class="left">ensime-inf-eval-region</td>
<td class="left">Send current region to Scala interpreter</td>
</tr>


<tr>
<td class="left">M-e</td>
<td class="left">ensime-inf-eval-region</td>
<td class="left">Send current region to Scala interpreter</td>
</tr>


<tr>
<td class="left">C-x C-e</td>
<td class="left">k/ensime-eval-last-scala-expr</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-n q</td>
<td class="left">k/ensime-quit</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-n k</td>
<td class="left">k/ensime-compile</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-M-d</td>
<td class="left">hop-at-point</td>
<td class="left">Jump to definition</td>
</tr>


<tr>
<td class="left">C-c i</td>
<td class="left">ensime-import-type-at-point</td>
<td class="left">Suggest possible imports.</td>
</tr>
</tbody>
</table>

### Tcl<a id="sec-2-7-9" name="sec-2-7-9"></a>

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
<td class="left">inferior-tcl</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-c C-f</td>
<td class="left">tcl-load-file</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-c C-v</td>
<td class="left">tcl-eval-defun</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">C-c C-x</td>
<td class="left">tcl-eval-region</td>
<td class="left">&#xa0;</td>
</tr>


<tr>
<td class="left">M-e</td>
<td class="left">tcl-eval-region</td>
<td class="left">&#xa0;</td>
</tr>
</tbody>
</table>

### Golang<a id="sec-2-7-10" name="sec-2-7-10"></a>

### reStructuredText<a id="sec-2-7-11" name="sec-2-7-11"></a>

### SQL<a id="sec-2-7-12" name="sec-2-7-12"></a>

### Version control<a id="sec-2-7-13" name="sec-2-7-13"></a>

#### Magit & ahg<a id="sec-2-7-13-1" name="sec-2-7-13-1"></a>

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
<td class="left">C-w</td>
<td class="left">prh:kill-current-buffer</td>
<td class="left">kill current buffer</td>
</tr>


<tr>
<td class="left">M-w</td>
<td class="left">get-vc-status</td>
<td class="left"><b>prog-mode</b>: git or hg status</td>
</tr>


<tr>
<td class="left">M-w</td>
<td class="left">diffview-current</td>
<td class="left"><b>magit-mode</b>: two-window</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">(side-by-side) comparsion</td>
</tr>


<tr>
<td class="left">S-M-w</td>
<td class="left">magit-copy-buffer-revision</td>
<td class="left">get buffer revision</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">magit-log-buffer-file</td>
<td class="left">Show log for the blob or file</td>
</tr>


<tr>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">visited in the current buffer.</td>
</tr>
</tbody>
</table>

#### git-gutter<a id="sec-2-7-13-2" name="sec-2-7-13-2"></a>

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
<td class="left">C-M-g &lt;down&gt;</td>
<td class="left">git-gutter:next-hunk</td>
</tr>


<tr>
<td class="left">C-M-g &lt;down&gt;</td>
<td class="left">git-gutter:previous-hunk</td>
</tr>


<tr>
<td class="left">C-M-g p</td>
<td class="left">git-gutter:popup-hunk</td>
</tr>
</tbody>
</table>

## Mouse<a id="sec-2-8" name="sec-2-8"></a>

## Menu<a id="sec-2-9" name="sec-2-9"></a>

### header<sub>name</sub><a id="sec-2-9-1" name="sec-2-9-1"></a>

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

## Org-mode<a id="sec-2-10" name="sec-2-10"></a>

### Time schedule<a id="sec-2-10-1" name="sec-2-10-1"></a>

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

## Emacs OS<a id="sec-2-11" name="sec-2-11"></a>

### elfeed<a id="sec-2-11-1" name="sec-2-11-1"></a>

### stock-ticker<a id="sec-2-11-2" name="sec-2-11-2"></a>
