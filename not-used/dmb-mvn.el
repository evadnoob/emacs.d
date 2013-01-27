(defvar *mvn-output-buffer* (get-buffer-create " *POM parse output*")
  "The buffer that output from mvn is written to.")

(defun pom-parse-classpath ()
  ""
  (interactive)
  (let ((*mvn-output-buffer* (get-buffer-create *mvn-output-buffer*)))
    (display-buffer *mvn-output-buffer*)
    
    (with-current-buffer *mvn-output-buffer* 
      (erase-buffer)
      ;; "-f pom.xml"
      (call-process "mvn" nil *mvn-output-buffer* t
                    "-B" "-N"
                    "help:effective-pom"
                    "dependency:build-classpath"
                    "-DincludeScope=compile"
                    "-DincludeTypes=jar")
      
      (goto-char (point-min))
      (let ((line-before-classpath (search-forward "Dependencies classpath:" nil t)))
        (message "line-before-classpath: %s" line-before-classpath )
        (if line-before-classpath
            (progn 
              (forward-line)
              ;;buffer-substring
              (message "point-bol: %s point-eol: %s" (point-at-bol) (point-at-eol))
              (let ((classpath (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
                (message "classpath line: %s" classpath )))
          (error "failed to find classpath in mvn dependency:build-classpath output"))))))


(defun pom-parse-classpath2 ()
  ""
  (interactive)
  (let ((*mvn-output-buffer* (get-buffer-create *mvn-output-buffer*)))
    (display-buffer *mvn-output-buffer*)
    
    (with-current-buffer *mvn-output-buffer* 
      (erase-buffer)
      ;; "-f pom.xml"
      (call-process "mvn" nil *mvn-output-buffer* t
                    "-B" "-N"
                    "help:effective-pom"
                    "dependency:build-classpath"
                    "-DincludeScope=compile"
                    "-DincludeTypes=jar")
      
      (goto-char (point-min))
      (let ((line-before-classpath (search-forward "Dependencies classpath:" nil t)))
        (message "line-before-classpath: %s" line-before-classpath )
        (if line-before-classpath
            (progn 
              (forward-line)
              ;;buffer-substring
              (message "point-bol: %s point-eol: %s" (point-at-bol) (point-at-eol))
              (let ((classpath (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
                ;;(message "classpath line: %s" classpath )
                classpath))
          (error "failed to find classpath in mvn dependency:build-classpath output"))))))
