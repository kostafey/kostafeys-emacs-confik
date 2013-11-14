<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">Kostafey's keybindings</a>
<ul>
<li><a href="#sec-1-1">Humane emacs</a>
<ul>
<li><a href="#sec-1-1-1">Exit/hide emacs</a></li>
<li><a href="#sec-1-1-2">Select Copy Paste</a></li>
<li><a href="#sec-1-1-3">Undo/redo</a></li>
<li><a href="#sec-1-1-4">Delete/insert char</a></li>
<li><a href="#sec-1-1-5">Save/revert</a></li>
</ul>
</li>
<li><a href="#sec-1-2">Basic point movements &amp; change buffer's position</a>
<ul>
<li><a href="#sec-1-2-1">Intellectual point jumps</a></li>
</ul>
</li>
<li><a href="#sec-1-3">IDE</a></li>
<li><a href="#sec-1-4">Menu</a>
<ul>
<li><a href="#sec-1-4-1">header<sub>name</sub></a></li>
</ul>
</li>
<li><a href="#sec-1-5">Org-mode</a>
<ul>
<li><a href="#sec-1-5-1">Time schedule</a></li>
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
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
<td class="left">&#xa0;</td>
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
