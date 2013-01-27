;;; sqlpretty.el --- Various multi-line operations

;; Copyright (C) 1998,2003 David M. Boon
;; XEmacs/Emacs

;; License agrement

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(unless (fboundp 'region-exists-p)
  (defun region-exists-p () mark-active))

;; This function does the actual triming.
;; All the others are calls to this with predefined regexps
;;;###autoload
(defun trim (regexp &optional replace)
  "Searches for `regexp' and deletes any matches.
If optional arg `replace' is set, it replaces the match.
If the region is active, only trims the lines in the region."
  (interactive "*sRegexp: ")
  (save-excursion
    (save-restriction
      (if (region-exists-p)
	  (narrow-to-region (region-beginning) (region-end)))
      (goto-char (point-min))
      (if (not replace) (setq replace ""))
      (while (re-search-forward regexp nil t)
	(replace-match replace)))))

;;;###autoload
(defun trim-lines ()
  "Trim whitespace (including CR) from the ends of all lines.
If the region is active, only trims the lines in the region."
  (interactive "*")
  (trim "^"  "+ \" ")
  (trim "+$"  " \"$"))
  ;(trim "[ \t\r]+$" " \""))

(defun unmake-sql-string() 
   "remove language specific formatting"
   (interactive "*")
   (trim "^ *\+ *\""  "XX"))

;;;###autoload
(defun trim-empty-lines ()
  "Trim all empty lines.
An empty line is a line with only whitespace (space, tab, CR) characters.
If the region is active, only trims the lines in the region."
  (interactive "*")
  (trim "^[ \t]*[\r]?\n"))

;;;###autoload
(defun trim-spaces ()
  "Trim all sequences of spaces to one space.
If the region is active, only trims the lines in the region."
  (interactive "*")
  (trim " +" " "))

;;;###autoload
(defun trim-leading-spaces ()
  "Trim all white space at the starts of lines.
If the region is active, only trims the lines in the region."
  (interactive "*")
  (trim "^[ \t]+"))

;;;###autoload
(defun trim-defines ()
  "Trim `#define' lines to isolate the `identifier'.
If the region is active, only trims the lines in the region."
  (interactive "*")
  (trim "\\(^[ \t]*#define[ \t]+\\)\\([^ \t(]*\\)\\(.*\\)" "\\2"))

;;;###autoload
(defun untrim-lines ()
  "Add ^M to the end of all lines.
If the region is active, only untrims the lines in the region."
  (interactive)
  (save-excursion
    (save-restriction
      (if (region-exists-p)
	  (narrow-to-region (region-beginning) (region-end)))
      (goto-char (point-min))
      (while (not (eobp))
	(end-of-line)
	(insert ?\r)
	(forward-char)))))

(provide 'sql-pretty)
