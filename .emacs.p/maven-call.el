(define-key jde-mode-map [f5] 'maven-call)
(defvar maven-history nil)
(defvar *maven-compilation-buffer* (get-buffer-create " *maven-compilation-buffer*")
  "The buffer that output from mvn is written to .")

(defun maven-process-setup ()
  "Setup compilation variables and buffer for `grep'.
Set up `compilation-exit-message-function' and run `grep-setup-hook'."
  (set (make-local-variable 'compilation-exit-message-function)
       (lambda (status code msg)
	 (if (eq status 'exit)
	     (cond ((zerop code)
		    '("finished (no errors)\n" . "matched"))
		   ((= code 1)
		    '("finished with no matches found\n" . "no match"))
		   (t
		    (cons msg code)))
	   (cons msg code)))))

(define-compilation-mode maven-compilation-mode "Maven"
  "Sets up the maven compilation buffer."
  (set (make-local-variable 'compilation-process-setup-function)
       'maven-process-setup)
  (set (make-local-variable 'compilation-disable-input) t))

(defun maven (target)
  "invoke maven goals"
;;;   (interactive
;;;    (progn
;;;      (let ((default "compile"))
;;;        (list (read-from-minibuffer "Run maven (like this): "
;;; 				   (if current-prefix-arg
;;; 				       default "compile")
;;; 				   nil nil 'maven-history
;;; 				   (if current-prefix-arg nil default))))))

  (interactive
   (progn
     (let ((default "compile test-compile"))
       (list (read-from-minibuffer "Run maven (like this): "
				   (if current-prefix-arg
				       default default)
				   nil nil 'maven-history
				   (if current-prefix-arg nil default))))))


  (let* ((buildfile (file-search-upward (file-name-directory (buffer-file-name)) "pom.xml"))
	 (outbuf (get-buffer-create *maven-compilation-buffer*))
	 (curbuf (current-buffer)))
    ;;(switch-to-buffer-other-window outbuf)
    (save-excursion
      (with-current-buffer outbuf
        (if (get-buffer *maven-compilation-buffer*)
            (erase-buffer))
        ;;(insert "#> mvn -f " buildfile " " target "\n")
        ;;(switch-to-buffer-other-window curbuf)
        (setq command-args (concat "mvn" " " "-f" " \"" buildfile "\" " target))
        (compilation-start command-args 'maven-compilation-mode nil t)))))
    
(defun file-search-upward (directory file)
  "search a file upward"
  (let* ((updir (file-truename (concat (file-name-directory directory) "../")))
	 (curfd   (if (not (string= (substring directory (- (length directory) 1)) "/"))
		      (concat directory "/" file)
		    (concat directory file))))
    (if (file-exists-p curfd)
	curfd
      (if (and (not (string= (file-truename directory) updir))
	       (< (length updir) (length (file-truename directory))))
	  (file-search-upward updir file)
	nil))))