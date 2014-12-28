;;; highlight-current-line.el--- routines for highlighting the current line in modes.

;; Author: David M. Boon <david.boon@gmail.com>
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

;; To add a mode to the highlight current line modes, do:
;;  (add-to-list 'dmb-highlight-current-line-modes "HTML")
;;

;;; Change History 
;; 02/07/2005 created


(defface highlight-current-line-face
  '((((class color) (background light))
     ;;(:background "dark slate gray" ))  ;; #e2ecf5 ;; light goldenrod yellow, gray93, "alice blue"
     (:background "light goldenrod yellow" ))
    ;;(:background "alice blue" ))
     ;;(:background "floral white" ))   ;;more yellow than white
    ;;(:background "alice blue"))

    (((class color) (background dark))
     ;;(:background "dark slate gray"))
     (:background "#ECE7BA")) ;; , #16001CCC2A70
     ;;(:background "alice blue"))
    (((class grayscale monochrome)
      (background light))
     (:background "black"))
    (((class grayscale monochrome)
      (background dark))
     ;;(:background "white")
     (:background "dark slate gray")))
  "face to use for current line")

(defvar dmb-current-line-highlighting-on nil
  "variable holds state of current line highlighting mode, 
use `toggle-highlight-current-line")

(defvar dmb-current-line-overlay nil
  "overlay for current line")

;; (defvar dmb-highlight-current-line-modes 
;;   (list "Emacs-Lisp" "JDE" "nXML" "Python" "Dired by name" "HTML") 
;;   "a list of mode names to apply the current line highlighting mode")

(defvar highlight-current-line-modes-include
  (list 'emacs-lisp-mode 'jde-mode 'nxml-mode 'python-mode 'dired-mode 'ibuffer-mode 'sql-interactive-mode 'conf-javaprop-mode 'java-mode 'javascript-mode 'buffer-menu+ 'bs-show) 
  "a list of mode names to apply the current line highlighting mode")


(defun highlight-current-line () 
  "using an overlay, change color of current line"
  (when (memq major-mode highlight-current-line-modes-include)
    (setq dmb-current-line-overlay 
          ;;(make-overlay (point-at-bol) (point-at-eol)))
          (make-overlay (point-at-bol) (1+ (line-end-position))))
    
    (overlay-put dmb-current-line-overlay 'face 'highlight-current-line-face)
    (add-hook 'pre-command-hook 'unhighlight-current-line nil t)))


(defun unhighlight-current-line ()
  "remove overlay and change color of current line to default"
  (delete-overlay dmb-current-line-overlay)
  (setq dmb-current-line-overlay nil))

(define-minor-mode highlight-current-line-mode 
  :global t :lighter " hcl"


  (if highlight-current-line-mode
      (progn 
        (add-hook 'post-command-hook 'highlight-current-line)
        (princ "highlight-current-line is on"))
    (progn 
      (remove-hook 'post-command-hook 'highlight-current-line)
      (princ "highligh-current-line is off"))))



(defun highlight-current-line-add-current-mode ()
  "add the current buffer's mode to the list of hightlighting modes, temporarily"
  (interactive)
  (add-to-list 'highlight-current-line-modes-include major-mode))

(provide 'highlight-current-line)
