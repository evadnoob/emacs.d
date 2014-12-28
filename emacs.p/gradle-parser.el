;;; gradle-parser.el --- Tools for using attributes from a gradle '.gradle' file
;;
;; Copyright (c) 2007 David Boon <david.boon@gmail.com>
;;
;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;; PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;; PERFORMANCE OF THIS SOFTWARE.
;;
;; Revision control metadata:
;; $Id$
;; $HeadURL$
;;
;;;
;;
;; A typical prj.el now looks like this:
;;
;; (jde-project-file-version "1.0")
;; (jde-set-variables
;;  '(jde-compile-option-command-line-args
;;    (quote ("-Xlint:all" "-Xlint:-serial"))))
;;
;; (require 'gradle-parser)
;; (with-gradle ()
;;   (gradle-set-jde-variables pom))
;;

(require 'cl)
(require 'jde)
(require 'xml)

(defgroup gradle nil
  "Options controlling the gradle parser."
  :group 'tools)

(defcustom gradle-file-name "build.gradle"
  "*Default name of a gradle build file."
  :type 'string
  :group 'gradle)

(defcustom gradle-home "gradle"
  ""
  :type 'string
  :group 'gradle)


(defcustom gradle-command "gradle"
  ""
  :type 'string
  :group 'gradle)

(defvar *gradle-cache*
  (make-hash-table :test 'equal)
  "The cache for parsed gradle files.  Don't touch.")


(defvar *gradle-output-buffer* (get-buffer-create " *gradle parse output*")
  "The buffer that output is written to.")

(defun gradle-clear-gradle-cache ()
  "Clears the gradle cache."
  (interactive)
  (clrhash *gradle-cache*))

(defun* gradle-find-gradle-file (&optional (gradle-file-name gradle-file-name))
  "Find the next .gradle file upwards in the directory hierarchy."
  (interactive)
  (let ((gradle (expand-file-name gradle-file-name)))
    (while (not (file-exists-p gradle))
      (if (jde-root-dir-p (file-name-directory gradle))
          (error "%s not found" (file-name-nondirectory gradle))
        (setq gradle (expand-file-name (concat "../" (file-name-nondirectory gradle))
                                    (file-name-directory gradle)))))
    gradle))

(defun gradle-file-last-modified-time (file)
  "The last-modified time of `file', as a 32-bit integer."
  (let ((lastmod (nth 5 (file-attributes file))))
    (logior (lsh (nth 0 lastmod) 16)
            (nth 1 lastmod))))

(defun* gradle-parse-gradle (&optional (gradle-file (gradle-find-gradle-file)))
  "parse the contents of the gradle build file"
  (let* ((gradle-file-last-mod-time (gradle-file-last-modified-time gradle-file))
         (cached-gradle-data (gethash gradle-file *gradle-cache*)))
    (if (and cached-gradle-data
             (<= gradle-file-last-mod-time (car cached-gradle-data)))
        (cdr cached-gradle-data)
      (with-current-buffer *gradle-output-buffer*
        (erase-buffer)
        (if (> (let ((default-directory (file-name-directory gradle-file)))
                 (call-process 
                  "sh"
                  nil *gradle-output-buffer* t 
                  (concat gradle-home "/bin/" gradle-command)
                  "--dependencies" 
                  "--build-file" 
                  gradle-file))
               0)
            (progn (display-buffer *gradle-output-buffer* t)
                   (error "%s call failed" gradle-command))
          (display-buffer *gradle-output-buffer* t)
          ;;parse buffer contents here, assign to dependenicies
          ;;(puthash gradle-file (cons gradle-file-last-mod-time dependencies)  ;; pom
          ;;         *gradle-cache*))))))
          )))))
  
(defun gradle-parse-classpath-buffer ()
  ""
  ;;(display-buffer *mvn-output-buffer*)
  (with-current-buffer *gradle-output-buffer* 
    (goto-char (point-min))
    (let ((line-before-classpath (search-forward "^-archives - " nil t)))
      (message "line-before-classpath: %s" line-before-classpath )
      (if line-before-classpath
          (progn 
            (forward-line)
            ;;buffer-substring
            ;;(message "point-bol: %s point-eol: %s" (point-at-bol) (point-at-eol))
            (let ((tmp-classpath (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
              (split-string tmp-classpath path-separator)))
        (error "failed to find classpath in mvn dependency:build-classpath output")))))

;; (defun pom-parse-classpath-buffer ()
;;   ""
;;   ;;(display-buffer *mvn-output-buffer*)
;;   (with-current-buffer *mvn-output-buffer* 
;;     (goto-char (point-min))
;;     (let ((line-before-classpath (search-forward "Dependencies classpath:" nil t)))
;;       (message "line-before-classpath: %s" line-before-classpath )
;;       (if line-before-classpath
;;           (progn 
;;             (forward-line)
;;             ;;buffer-substring
;;             ;;(message "point-bol: %s point-eol: %s" (point-at-bol) (point-at-eol))
;;             (let ((tmp-classpath (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
;;               (split-string tmp-classpath path-separator)))
;;         (error "failed to find classpath in mvn dependency:build-classpath output")))))

;; (defun pom-parse-classpath-file (classpath-file)
;;   (with-temp-buffer
;;     (insert "(\"")
;;     (insert-file-contents-literally classpath-file)
;;     (goto-char (point-max))
;;     (insert "\")")
;;     (goto-char (point-min))
;;     (while (search-forward "\\" nil t)
;;       (replace-match "/" nil t))
;;     (goto-char (point-min))
;;     (while (search-forward ":" nil t)
;;       (unless (equal (let ((start (match-beginning 0)))
;;                        (buffer-substring start (+ start 2)))
;;                      ":/")
;;         (replace-match "\" \"" nil t)))
;;     (read-from-whole-string (buffer-substring (point-min) (point-max)))))



;; (defun* pom-set-jde-variables (&optional (pom-node *pom-node*))
;;   "Sets the JDE variables `jde-project-name', `jde-global-classpath',
;; `jde-compile-option-directory', `jde-compile-option-source',
;; `jde-compile-option-target', `jde-compile-option-encoding',
;; `jde-sourcepath' and `jde-built-class-path' to sensible values based
;; on the given POM."
;;   (let ((target-directory (pom-get-property "project.build.outputDirectory" pom-node)))
;;     (jde-set-variables
;;      '(jde-project-name (pom-get-property "project.name" pom-node))
;;      '(jde-global-classpath (list* target-directory
;;                                    (pom-compile-classpath pom-node)))
;;      '(jde-compile-option-directory target-directory)
;;      '(jde-compile-option-source (list (pom-compiler-source pom-node)))
;;      '(jde-compile-option-target (list (pom-compiler-target pom-node)))
;;      '(jde-compile-option-encoding (pom-plugin-configuration-property pom-node 'maven-compiler-plugin 'encoding))
;;      '(jde-sourcepath (pom-get-property "project.build.sourceDirectory" pom-node))
;;      '(jde-built-class-path (list target-directory)))))

;; (defmacro with-gradle (gradle-file &rest body)
;;   "Execute BODY with `*pom-node*' bound to the result of calling `pom-parse-pom' on POM-FILE."
;;   `(let ((*pom-node* (pom-parse-pom (or ,pom-file (pom-find-pom-file)))))
;;      ,@body))

;;(put 'with-pom 'lisp-indent-function 1)

(provide 'gradle-parser)
