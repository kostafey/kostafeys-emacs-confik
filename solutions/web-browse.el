;;; web-browse.el --- Interact with a web browser.  -*- lexical-binding: t -*-

;;; Commentary:

;; Search Google for the region or a query, open a URL, and point
;; `browse-url' at the first browser found.

;;; Code:

(require 'browse-url)

(defun web-browse-find-executable ()
  "Return the first browser found among the known ones, or nil."
  (seq-find #'executable-find
            '("chromium" "chromium-browser" "google-chrome-stable"
              "palemoon" "firefox"
              "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"
              "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")))

(if-let* ((browser (web-browse-find-executable)))
    (setq browse-url-browser-function #'browse-url-generic
          browse-url-generic-program browser)
  (message "Can't find any browser in the PATH"))

(defun web-browse-google (&optional empty)
  "Search Google for the region if active, or for a query from a prompt.
The prompt offers the symbol at point; with prefix arg EMPTY, it starts
empty."
  (interactive "P")
  (browse-url
   (concat "https://www.google.com/search?ie=utf-8&oe=utf-8&q="
           (url-hexify-string
            (if (use-region-p)
                (buffer-substring-no-properties (region-beginning)
                                                (region-end))
              (read-string "Google: "
                           (unless empty
                             (thing-at-point 'symbol t))))))))

(defun web-browse-google-query ()
  "Search Google for the region if active, or for a query typed anew."
  (interactive)
  (web-browse-google t))

(defun web-browse-google-home ()
  "Open the Google home page."
  (interactive)
  (browse-url "https://www.google.com"))

(defun web-browse-url (&optional symbol)
  "Open the region if active, or a URL from a prompt, in the browser.
With prefix arg SYMBOL, the prompt offers the symbol at point.  A URL
typed without a scheme gets https://."
  (interactive "P")
  (browse-url
   (if (use-region-p)
       (buffer-substring-no-properties (region-beginning) (region-end))
     (let ((url (read-string "Go to URL: "
                             (and symbol (thing-at-point 'symbol t)))))
       (if (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*://" url)
           url
         (concat "https://" url))))))

(provide 'web-browse)

;;; web-browse.el ends here
