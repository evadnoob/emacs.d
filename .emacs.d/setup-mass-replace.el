;;
;; Modified on 01/16/04 David M Boon, originally written by
;; mfsr.el 0.2
;; by Jan Peter de Ruiter, 1995
;; Feel free to distribute to anyone.
;;
;; usage: 

;;


(defun mass-replace-pattern-in-files (dir pattern fromregex toregex)
  "File Search and Replace Regexp Interactively over files marked
   in dired mode."
  (interactive "Ddir: 
spattern(file pattern, not regex): 
sreplace regexp: 
swith regexp: 
") 
  (message (format "dir %s %s %s %s " dir pattern fromregex toregex))
  (save-excursion			
    (walk-path dir '(lambda (directory filename) 
                      (message "you are here")
                      (some-file-search-replace filename fromregex toregex))))
    (princ "done."))


(defun mass-replace-marked-files (fromrx torx)
  "File search and replace regexp Interactively over files marked
   in dired mode.
Usage:
 1) do find(f4 for me, or `find-dired'), results in a dired buffer.
 
 2), mark the files you want to search and replace in using either:
 `dired-mark-files-containing-regexp', `dired-mark-files-regexp', or
 `dired-mark-subdir-files', or some other way of marking

 3) M-x mass-replace-marked-files 

 4) enter your to and from regexp 

Files replaced will no be saved, so invoke `save-some-buffers' to save all 
of the files.
"

  (interactive "sreplace regexp: 
swith regexp: ") 
      ;(setq (case-fold-search nil))
      ;(set (make-local-variable 'sgml-xml-mode) t)
      ;(set (make-local-variable 'case-fold-search) nil)
      ;(message (format "case-fold-search is %s" case-fold-search))
  (save-excursion			
    (let ((filelist (dired-get-marked-files)))
      (while filelist
        ;(message "replacing %s" filelist)
	(some-file-search-replace (car filelist) fromrx torx)
	(setq filelist (cdr filelist)))))
  (princ "done."))

(defun some-file-search-replace (filename fromrx torx)
  "Searches for a given file, and performs a search-replace over it.
   If it is in an existing buffer, dave buffer will be processed.
   Otherwise, it is loaded into a buffer and then processed. Files
   that are changed will not be closed or saved, so that it is still
   possible to undo the changes in every file."
  (message "replacing '%s' with '%s' in '%s'" fromrx torx filename)
  (progn (set-buffer (find-file-literally filename)) 
         (goto-char (point-min))
         (while (re-search-forward fromrx nil t)
           (replace-match torx nil nil))
   (if (not (buffer-modified-p))
       (kill-buffer (current-buffer)))
   ))

(defun in-buffer-p (filename)
  (member (get-file-buffer filename) (buffer-list)))

(provide 'mass-replace)



;;
;; search for <\?xml version="1\.0" encoding="UTF-8"\?>
;; replace with:
;; <\?xml version="1\.0" encoding="UTF-8"\?>
;; <!DOCTYPE jbosscmp-jdbc PUBLIC
;;      "-//JBoss//DTD JBOSSCMP-JDBC 3.2//EN"
;;      "http://www.jboss.org/j2ee/dtd/jbosscmp-jdbc_3_2.dtd">


;;beginning-of-buffer
;;Don't use command in Lisp programs!
;;(goto-char (point-min)) is faster and avoids clobbering the mark.

(provide 'mass-replace)
