;;; mb-depth+.el --- Indicate minibuffer-depth in prompt
;;
;; Filename: mb-depth+.el
;; Description: Indicate minibuffer-depth in prompt
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2006, Drew Adams, all rights reserved.
;; Created: Sat Nov 18 16:37:53 2006
;; Version: 
;; Last-Updated: Sat Nov 18 17:13:41 2006 (-28800 Pacific Standard Time)
;;           By: dradams
;;     Update #: 15
;; URL: http://www.emacswiki.org/cgi-bin/wiki/mb-depth+.el
;; Keywords: convenience
;; Compatibility: GNU Emacs 22
;; 
;; Features that might be required by this library:
;;
;;   `mb-depth'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;  This library modifies library `mb-depth.el' slightly, to let you
;;  decide what depth indicator format to use, and which face to
;;  highlight it in.  It provides a minor tweak to function
;;  `minibuf-depth-setup-minibuffer', which, in `mb-depth.el',
;;  hard-codes the face and indicator format.
;;
;;  In addition, the default indicator format is simpler than that in
;;  `mb-depth.el', and the default face is `default' instead of
;;  `highlight'.
;;
;;  Faces defined here:
;;
;;    `mb-depth-indicator'.
;;
;;  User options defined here:
;;
;;    `mb-depth-indicator-format'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 2006/11/18 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'mb-depth)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface mb-depth-indicator '((t (:inherit default)))
  "Face used to indicate minibuffer depth."
  :group 'convenience :group 'faces)

(defcustom mb-depth-indicator-format "%d) "
  "*Format string for minibuffer depth indicator."
  :type 'string :group 'convenience)


;; REPLACE original defined in `mb-depth.el'.
;; Use face `mb-depth-indicator' and option `mb-depth-indicator-format'.
;;
;; This function goes on minibuffer-setup-hook
(defun minibuf-depth-setup-minibuffer ()
  "Set up a minibuffer for `minibuffer-indicate-depth-mode'.
The prompt should already have been inserted."
  (when (> (minibuffer-depth) 1)
    (setq minibuf-depth-overlay (make-overlay (point-min) (1+ (point-min))))
    (overlay-put minibuf-depth-overlay 'before-string
		 (propertize (format mb-depth-indicator-format (minibuffer-depth))
                             'face 'mb-depth-indicator))
    (overlay-put minibuf-depth-overlay 'evaporate t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mb-depth+)

;;; mb-depth+.el ends here
