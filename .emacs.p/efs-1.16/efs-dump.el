;; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File:         efs-dump.el
;; Release:      $efs release: 1.16 $
;; Version:      $Revision: 1.2 $
;; RCS:
;; Description:  Install a bare-bones EFS hook into file-name-handler-alist
;;               for dumping
;; Author:       Mike Sperber <sperber@informatik.uni-tuebingen.de>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'efs-dump)

(defconst efs-dump-version
  (concat (substring "$efs release: 1.16 $" 14 -2)
	  "/"
	  (substring "$Revision: 1.2 $" 11 -2)))

;;;###autoload
(defvar efs-path-root-regexp "^/[^/:]+:"
  "Regexp to match the `/user@host:' root of an efs full path.")

;;;###autoload
(or (assoc efs-path-root-regexp file-name-handler-alist)
    (setq file-name-handler-alist
	  (cons
	   (cons efs-path-root-regexp 'remote-path-file-handler-function)
	   file-name-handler-alist)))

(autoload 'efs-file-handler-function "efs" "\
Function to call special file handlers for remote files." nil nil)

;;;###autoload
(defun remote-path-file-handler-function (operation &rest args)
  "Function to call special file handlers for remote files."
  (if allow-remote-paths
      (apply 'efs-file-handler-function operation args)
    (let ((inhibit-file-name-handlers
	   (cons 'remote-path-file-handler-function
		 (and (eq inhibit-file-name-operation operation)
		      inhibit-file-name-handlers)))
	  (inhibit-file-name-operation operation))
      (apply operation args))))

;;; end of efs-dump.el
