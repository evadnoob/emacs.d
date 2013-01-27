;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-18.el
;; Release:      $efs release: 1.16 $
;; Version:      $Revision: 1.14 $
;; RCS:          
;; Description:  efs overload support for Gnu Emacs V18.X
;; Author:       Sandy Rutherford <sandy@ibm550.sissa.it>
;; Created:      Tue Jun  1 19:54:23 1993 by sandy on ibm550
;; Modified:     Sun Nov 27 11:44:51 1994 by sandy on gandalf
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is part of efs. See efs.el for copyright
;;; (it's copylefted) and warrranty (there isn't one) information.

(provide 'efs-18)
(require 'emacs-19) ; For V19 compatibility
(require 'efs-cu)
(require 'fn-handler)
(require 'efs-ovwrt)

;;; Variables

(defconst efs-18-version
  (concat (substring "$efs release: 1.16 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.14 $" 11 -2)))


(defconst efs-buffer-hashtable (efs-make-hashtable 23))
;; Hash table for storing buffer visisted file modtime for remote files.

;;; efs functions which need special defs for V18.

(defconst efs-tmp-keymap (make-sparse-keymap))
(define-key efs-tmp-keymap "\C-m" 'exit-minibuffer)

(defun efs-repaint-minibuffer ()
  "Set minibuf_message = 0, so that the contents of the minibuffer will show.
This is the Gnu Emacs V18 version of this function."
  (if (eq (selected-window) (minibuffer-window))
      (let ((unread-command-char ?\C-m)
	    (enable-recursive-minibuffers t))
	(read-from-minibuffer "" nil efs-tmp-keymap nil))))

(defun efs-gmt-time ()
  ;; Emacs 18 doesn't have current-time.  In emacs-19.el, there is a
  ;; current-time function, but it is fake.  It just ticks like a clock, it's
  ;; not a clock.  Doing the below there would be too slow.
  (let ((temp (car (efs-make-tmp-name nil nil))))
    (unwind-protect
	(progn
	  (write-region (point) (point) temp nil 'quiet)
	  (nth 5 (file-attributes temp)))
      (efs-del-tmp-name temp))))

(defun efs-local-file-type (file)
  ;; Looks at the beginning (magic-cookie) of a local file to determine
  ;; if it is a text file or not.  If it's not a text file, it doesn't care
  ;; about what type of binary file, so this doesn't really look for a magic
  ;; cookie.
  ;; Doesn't call efs-save-match-data.  The caller should do so.
  (save-excursion
    (set-buffer (get-buffer-create efs-data-buffer-name))
    (erase-buffer)
    (call-process "head" file t nil "-c" "16")
    (goto-char (point-min))
    (if (looking-at "[ -~\n\r\C-L]*\\'")
	'text
      '8-binary)))

;;; Modtime support

(defun efs-set-emacs-bvf-mdtm (buffer mdtm)
  ;; In emacs 18 we don't have access to emacs' internal cache of buffer
  ;; file modtimes.  We keep these in our own hashtable.
  (and (consp mdtm) (listp (cdr mdtm))
       (setq mdtm (cons (car mdtm) (nth 1 mdtm))))
  (efs-put-hash-entry (if (stringp buffer) buffer (buffer-name buffer))
		      mdtm efs-buffer-hashtable))
;; Emacs cache is nonsense.  However, leaving it around seems to make undo
;; behave more reasonably.  It's a kludge, but that is all that is possible
;; in emacs 18.
;;   (save-excursion
;;     (set-buffer buffer)))
;;     (let (file-name-handler-alist)
;;       (clear-visited-file-modtime))))

;; visited-file-modtime isn't defined in V18.  Give it a def here
;; for remote files only.

(defun visited-file-modtime ()
  (if (and buffer-file-name (efs-ftp-path buffer-file-name))
      (or (efs-get-hash-entry (buffer-name (current-buffer))
			      efs-buffer-hashtable)
	  0)
    (error
     "efs: visited-file-modtime only works for remote files in Emacs 18.")))

(defun efs-clear-visited-file-modtime ()
  ;; Version of clear-visited-file-modtime for buffers visiting remote files.
  (let ((ent (efs-get-file-entry buffer-file-name))
	(file-name-handler-alist nil))
    ;; Not clearing emacs' internal cache seems to fake undo into behaving
    ;; better.
    ;; (clear-visited-file-modtime) ; for emacs' cache
    (efs-put-hash-entry (buffer-file-name) nil efs-buffer-hashtable)
    (if (nth 5 ent)
	(setcar (nthcdr 5 ent) nil))))

;; comint in V18 doesn't comint-output-filter

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

(defun efs-18-expand-file-name (name &optional default)
  (let ((handler (find-file-name-handler name 'expand-file-name)))
    (if handler
	(funcall handler 'expand-file-name name default)
      (efs-real-expand-file-name name default))))

(efs-overwrite-fn "efs" 'expand-file-name 'efs-18-expand-file-name)

;; Need to overwrite this, so that it calls the right completion functions.
(defun efs-18-read-file-name-internal (string dir action)
  "Documented as original."
  (if (eq action 'lambda)
      (and (> (length string) 0)
	   (file-exists-p (substitute-in-file-name string)))
    (let ((name "")
	  (realdir dir)
	  specdir)
      (or (zerop (length string))
	  (setq string (substitute-in-file-name string)
		name (file-name-nondirectory string)
		specdir (file-name-directory string)))
      (if specdir
	  (setq realdir (expand-file-name specdir dir)))
      (if action
	  (file-name-all-completions name realdir)
	(let ((val (file-name-completion name realdir)))
	  (if (and specdir (stringp val))
	      (efs-quote-dollars (concat specdir val))
	    val))))))

(efs-overwrite-fn "efs" 'read-file-name-internal
		  'efs-18-read-file-name-internal)


(defun efs-18-clear-visited-file-modtime ()
  "Documented as original"
  (let ((handler (and buffer-file-name
		      (find-file-name-handler buffer-file-name
					      'clear-visited-file-modtime))))
    (if handler
	(funcall handler 'clear-visited-file-modtime)
      (let (file-name-handler-alist)
	(efs-real-clear-visited-file-modtime)))))

(efs-overwrite-fn "efs" 'clear-visited-file-modtime
		  'efs-18-clear-visited-file-modtime)

(if (featurep 'efs)
    (put 'clear-visited-file-modtime 'efs 'efs-clear-visited-file-modtime)
  (add-hook
   'efs-load-hook
   (function
    (lambda ()
      (put 'clear-visited-file-modtime 'efs
	   'efs-clear-visited-file-modtime)))))

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

;;; end of efs-18.el
