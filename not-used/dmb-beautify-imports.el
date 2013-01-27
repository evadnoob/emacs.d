;;; beautify-imports.el --- simple routing for beautifying imports for sets of files.

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
;;    (require 'beautify-imports) 
;; to your .emacs.  Once available, run find-dired to find a
;; set of files, mark the files, the invoke M-x beautify-imports

;;; Change History 
;; 02/07/2005 created

;;
;; organize java source files
;;
(defcustom organize-entire-package-hook 
  (list 'jde-import-organize )
  "hook that gets run for each java file.  Used to have `jde-import-kill-extra-imports, 
but that caused errors so removed it."
  :type 'hook
  :group 'dave)

(defconst ignore-these-files-list 
  (list "." ".." "CVS")
  "A list of files/patterns to skip when scanning java folders")



(defun organize-entire-package (dir &optional recursing)
  "walk a directory and run `jde-import-organize' on *.java files.
Can also set `organize-entire-package-hook' to run additional functions.

Optional argument recursing should never be set, this method will set 
that argument internally.
"
  (interactive (list (read-file-name "what path? ")))
  
  (setq organize-start-time (current-time))
  (setq outbuf (get-buffer-create "*organize-package-log*"))
    
  (if (not recursing)
      (save-excursion 
         (set-buffer outbuf)
         ;(erase-buffer)
         (princ (format "start time %s\n" (current-time-string)) outbuf)))
  
  (if (not dir)
      (error "Missing start directory"))
  (if (not (file-directory-p dir))
      (error "Start path specified is not a directory"))
  
;; in order to keep these files out of the recent list temporarily add an exclusion for 
;; *.*
;;   (if (featurep 'recentf-mode)
;;       (recentf-mode nil))

  (dolist (file (directory-files dir t))
    (setq just-filename (file-name-nondirectory file))
    (princ
     (format 
      "%s isdir:%s file:%s excluded:%s\n"
      file
      (file-directory-p file) 
      just-filename 
      (and (member just-filename ignore-these-files-list))) outbuf)
    
    (setq outwin (display-buffer outbuf nil t))
    (save-selected-window
      (select-window outwin)
      (goto-char (point-max)))
    
    (let ((redisplay-dont-pause t))
;      (recenter -1)
      (sit-for 0.001))
;;     (redraw-display)
;;     (sit-for 0.001)
   
    
    (cond 
     ((and (not (file-directory-p file)) (string-match "\.java$" file) (not (member just-filename ignore-these-files-list)))
      ;(princ (format "running jde-import-organize on %s\n" file) outbuf)
      (setq starttime (float-time))
      (setq buf (find-file-noselect (expand-file-name file)))
      (set-buffer buf)
      ;(redraw-display)
      ;;might be a good idea to put hooks here so that others may add functions/behaviors?
      (run-hooks 'organize-entire-package-hook)
      (princ (format " %s\n " (- (float-time) starttime)) outbuf)
      ;(princ (format "%s\n" (- current-time-string)) outbuf)
;;       (if (not (buffer-modified-p (current-buffer)))
;;           (kill-buffer (current-buffer))))
      (if (buffer-modified-p (current-buffer))
          (save-buffer))
      (kill-buffer (current-buffer)))
     ((and (file-directory-p file) (not (member just-filename ignore-these-files-list)))
      (organize-entire-package file t))))
  
  (if (not recursing)
      (princ (format "end time %s seconds\n" (time-to-seconds (time-subtract (current-time) organize-start-time))) outbuf)))


(defun beautify-imports-dired ()
  "run jde-import-organize, various other jde-import functions on
 a set of files returned from dired."
  (interactive) 
  (save-excursion			
    (let ((filelist (dired-get-marked-files)))
      (while filelist
        (progn 
          (set-buffer (find-file (car filelist)))
          (jde-import-organize)
          (jde-import-kill-extra-imports))
        (setq filelist (cdr filelist))
        (if (not (buffer-modified-p))
            (kill-buffer (current-buffer))))))
  (princ "beautify-imports done"))



(defun beautify-imports-prompt ()
  "run jde-import-organize, various other jde-import functions on
 a set of files returned from dired."
  (interactive) 
  (save-excursion			
    (let ((filelist (dired-get-marked-files)))
      (while filelist
        (progn 
          (set-buffer (find-file (car filelist)))
          (jde-import-organize)
          (jde-import-kill-extra-imports))
        (setq filelist (cdr filelist))
        (if (not (buffer-modified-p))
            (kill-buffer (current-buffer))))))
  (princ "beautify-imports done"))



(provide 'beautify-imports)

