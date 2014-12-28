;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-19.23.el
;; Release:      $efs release: 1.16 $
;; Version:      $Revision: 1.10 $
;; RCS:          
;; Description:  efs support for the original GNU Emacs from FSF,
;;               version 19.23 and later
;; Author:       Sandy Rutherford <sandy@ibm550.sissa.it>
;; Created:      Mon May 23 21:44:12 1994 by sandy on ibm550
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

(provide 'efs-19\.23)
(require 'efs-cu)
(require 'efs-ovwrt)

;;; Variables

(defconst efs-19\.23-version
  (concat (substring "$efs release: 1.16 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.10 $" 11 -2)))

(defvar buffer-file-truename)

;;; efs functions which need special definitions

(defun efs-set-buffer-file-name (filename)
  ;; Sets the buffer local variables for filename appropriately.
  ;; A special function because Lucid and FSF do this differently.
  (setq buffer-file-name filename
	buffer-file-truename (file-truename filename)))

;;; Functions that don't access the file-name-handler-alist yet, but should.

(defun efs-19\.23-expand-file-name (name &optional default)
  (let ((handler (find-file-name-handler name 'expand-file-name)))
    (if handler
	(funcall handler 'expand-file-name name default)
      (efs-real-expand-file-name name default))))

(efs-overwrite-fn "efs" 'expand-file-name 'efs-19\.23-expand-file-name)

(defun efs-19\.23-backup-buffer ()
  "Documented as original"
  (if buffer-file-name
      (let ((handler (find-file-name-handler buffer-file-name 'backup-buffer)))
	(if handler
	    (funcall handler 'backup-buffer)
	  (efs-real-backup-buffer)))))

(efs-overwrite-fn "efs" 'backup-buffer 'efs-19\.23-backup-buffer)

(defun efs-19\.23-abbreviate-file-name (filename)
  "Documented as original"
  (let ((handler (find-file-name-handler filename 'abbreviate-file-name)))
    (if handler
	(funcall handler 'abbreviate-file-name filename)
      (let (file-name-handler-alist)
	(efs-real-abbreviate-file-name filename)))))

(efs-overwrite-fn "efs" 'abbreviate-file-name 'efs-19\.23-abbreviate-file-name)

(defun efs-19\.23-recover-file (file)
  "Documented as original"
  (interactive
   (let ((prompt-file buffer-file-name)
	 (file-name nil)
	 (file-dir nil))
     (and prompt-file
	  (setq file-name (file-name-nondirectory prompt-file)
		file-dir (file-name-directory prompt-file)))
     (list (read-file-name "Recover file: "
			   file-dir nil nil file-name))))
  (let* ((file (expand-file-name file))
	 (handler (or (find-file-name-handler file 'recover-file)
		      (find-file-name-handler 
		       (let ((buffer-file-name file))
			 (make-auto-save-file-name))
		       'recover-file))))
    (if handler
	(funcall handler 'recover-file file)
      (efs-real-recover-file file))))

(efs-overwrite-fn "efs" 'recover-file 'efs-19\.23-recover-file)

(defun efs-19\.23-create-file-buffer (filename)
  "Documented as original"
  (let ((handler (find-file-name-handler filename 'create-file-buffer)))
    (if handler
	(funcall handler 'create-file-buffer filename)
      (let (file-name-handler-alist)
	(efs-real-create-file-buffer filename)))))

(efs-overwrite-fn "efs" 'create-file-buffer 'efs-19\.23-create-file-buffer)

(defun efs-19\.23-substitute-in-file-name (filename)
  "Documented as original."
  (let ((handler (find-file-name-handler filename 'substitute-in-file-name)))
    (if handler
	(funcall handler 'substitute-in-file-name filename)
      (let (file-name-handler-alist)
	(efs-real-substitute-in-file-name filename)))))

(efs-overwrite-fn "efs" 'substitute-in-file-name
		  'efs-19\.23-substitute-in-file-name)

;; make-directory-internal doesn't use the right OP symbol for
;; find-file-name-handler.  Fixed for 19.26.

(defun efs-19\.23-make-directory-internal (dirname)
  "Documented as original"
  (let* ((dirname (expand-file-name dirname))
	 (handler (find-file-name-handler dirname
					  'make-directory-internal)))
    (if handler
	(funcall handler 'make-directory-internal dirname)
      (let (file-name-handler-alist)
	(efs-real-make-directory-internal dirname)))))

(efs-overwrite-fn "efs" 'make-directory-internal
		  'efs-19\.23-make-directory-internal)


;;; Compatibility

(defun char-before (&optional pos)
  (let ((pos (if (not (null pos))
		 pos
	       (point))))
    (if (< (1- pos) (point-min))
	nil
      (save-excursion
	(goto-char pos)
	(preceding-char)))))

;; end of efs-19.23.el
