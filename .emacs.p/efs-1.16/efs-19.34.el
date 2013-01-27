;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-19.34.el
;; Release:      $efs release: 1.16 $
;; Version:      $Revision: 1.1 $
;; RCS:          
;; Description:  efs support for the original GNU Emacs from FSF,
;;               version 19.34 and later
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

(provide 'efs-19\.34)
(require 'efs-cu)
(require 'efs-ovwrt)

;;; Variables

(defconst efs-19\.34-version
  (concat (substring "$efs release: 1.16 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.1 $" 11 -2)))

(defvar buffer-file-truename)

;;; efs functions which need special definitions

(defun efs-set-buffer-file-name (filename)
  ;; Sets the buffer local variables for filename appropriately.
  ;; A special function because Lucid and FSF do this differently.
  (setq buffer-file-name filename
	buffer-file-truename (file-truename filename)))


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

;; end of efs-19.34.el
