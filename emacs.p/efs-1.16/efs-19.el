;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-19.el
;; Release:      $efs release: 1.16 $
;; Version:      $Revision: 1.11 $
;; RCS:          
;; Description:  efs support for the original GNU Emacs from FSF,
;;               versions 19.1 through 19.22
;; Author:       Sandy Rutherford <sandy@ibm550.sissa.it>
;; Created:      Tue May 25 00:37:51 1993 by sandy on ibm550
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

(provide 'efs-19)
(require 'efs-cu)
(require 'fn-handler)
(require 'efs-ovwrt)

;;; Variables

(defconst efs-19-version
  (concat (substring "$efs release: 1.16 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.11 $" 11 -2)))

;;; efs functions which need special definitions

(defun efs-set-buffer-file-name (filename)
  ;; Sets the buffer local variables for filename appropriately.
  ;; A special function because Lucid and FSF do this differently.
  (setq buffer-file-name filename
	buffer-file-truename (file-truename filename)))

;; comint in these Emacs versions doesn't have comint-output-filter

(defun efs-process-log-string (proc str)
  ;; For a given PROCESS, log the given STRING at the end of its
  ;; associated buffer.
  (let ((obuff (current-buffer))
	(buff (get-buffer (process-buffer proc))))
    (if buff
	(unwind-protect
	    (progn
	      (set-buffer buff)
	      (save-excursion
		;; Insert the text, moving the process-marker.
		(goto-char (process-mark proc))
		(insert-before-markers str)))
	  (set-buffer obuff)))))

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

;;; end of efs-19.el
