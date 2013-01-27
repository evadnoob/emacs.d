;;; highlight-current-line.el--- routines for highlighting the current line in modes.

;; Author: David M. Boon <davidmboon@hotmail.com>
;; Created: 02/07/2005
;; Version: $Revision: $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; to install put this file in your load-path, then add:
;;    (require 'highlight-current-line) 
;; to your .emacs.  Once available, invoke M-x toggle-highlight-current-line

;;; Change History 
;; 02/07/2005 created


(defface dmb-current-line-face
  '((((class color) (background light))
     (:background "alice blue")) ;;gray93
    (((class color) (background dark))
     (:background "alice blue"))
    (((class grayscale monochrome)
      (background light))
     (:background "black"))
    (((class grayscale monochrome)
      (background dark))
     (:background "white")))
  "face to use for current line")

(defvar dmb-current-line-highlighting-on nil
  "variable holds state of current line highlighting mode, 
use `toggle-highlight-current-line")

(defvar dmb-current-line-overlay nil
  "overlay for current line")

(defvar dmb-highlight-current-line-modes 
  (list "Emacs-Lisp" "JDE" "nXML" "Python" "Dired by name" "Shell") 
  "a list of mode names to apply the current line highlighting mode")

(defun highlight-current-line () 
  "using an overlay, change color of current line"
  ;;(princ (format "%s %s" dmb-highlight-current-line-modes mode-name)) 
  (if dmb-current-line-highlighting-on
      (when (member mode-name dmb-highlight-current-line-modes)
      (setq dmb-current-line-overlay 
              (make-overlay (point-at-bol) (point-at-eol)))
        (overlay-put dmb-current-line-overlay 'face 'dmb-current-line-face)
        (add-hook 'pre-command-hook 'unhighlight-current-line nil t))))


(defun unhighlight-current-line ()
  "remove overlay and change color of current line to default"
  (delete-overlay dmb-current-line-overlay)
  (setq dmb-current-line-overlay nil))

(defun toggle-highlight-current-line ()
  "turn on or off current line highlighting"
  (interactive)
  (if dmb-current-line-highlighting-on 
      (setq dmb-current-line-highlighting-on nil)
    (setq dmb-current-line-highlighting-on t))

  (if dmb-current-line-highlighting-on
      (add-hook 'post-command-hook 'highlight-current-line)
    (remove-hook 'post-command-hook 'highlight-current-line))

  (if dmb-current-line-highlighting-on 
      (princ "highlight-current-line is on")
    (princ "highligh-current-line is off")))


(provide 'highlight-current-line)