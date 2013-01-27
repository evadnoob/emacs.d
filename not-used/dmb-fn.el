;; dmb-fn.el

(defun walk-path (dir action)
  "walk DIR executing ACTION with (dir file)"
  (cond ((file-directory-p dir)
         (or (char-equal ?/ (aref dir(1- (length dir))))
             (setq dir (file-name-as-directory dir)))
         (let ((lst (directory-files dir nil nil t))
               fullname file)
           (while lst
             (setq file (car lst))
             (setq lst (cdr lst))
             (cond ((member file '("." "..")))
                   (t
                    (and (funcall action dir file)
                         (setq fullname (concat dir file))
                         (file-directory-p fullname)
                         (walk-path fullname action)))))))
        (t
         (funcall action
                  (file-name-directory dir)
                  (file-name-nondirectory dir)))))

;; (defun walk-path2 (dir action)
;;   "walk DIR executing ACTION with (dir file)"
;;   (cond ((file-directory-p dir)
;;          (or (char-equal ?/ (aref dir(1- (length dir))))
;;              (setq dir (file-name-as-directory dir)))
;;          (dolist (file (directory-files dir nil nil t))
;;            (cond ((member file '("." "..")))
;;                    (t
;;                     (and (funcall action dir file)
;;                          (setq fullname (concat dir file))
;;                          (file-directory-p fullname)
;;                          (walk-path fullname action)))))
;;         (t
;;          (funcall action
;;                   (file-name-directory dir)
;;                   (file-name-nondirectory dir))))))


(defun walk-path-visitor (dir file)
  "Called by walk-path for each file found"
  (message (concat  dir file)))

;;(walk-path "~/notes" 'walk-path-visitor)
;;(walk-path2 "~/notes" 'walk-path-visitor)

;;;###autoload
(defun make-tags-file (path background)
  "simple function to create TAGS file"
  (interactive 
   (list (read-file-name "path: ") t))

  (let ((expanded-path (expand-file-name path)))
    
    (if (not (file-directory-p expanded-path))
        (error "Start path specified is not an existing directory"))
    
    (cd expanded-path)
    
    (setq truefilename (file-truename path)
          ;;dmb-tags-args (format "find %s -name \"*.java\" -print | ctags -e --language-force=java --members --output=%sTAGS -" (file-truename path) (file-truename path)))
          ;;dmb-tags-args (format "ctags -R -e --language-force=java -f %sTAGS" (file-truename path) (file-truename path)))
          dmb-tags-args (format "ctags -R -e --exclude='target' --exclude=uber.sql --extra=+q --langmap=SQL:.sql.pks.pkb  -V -f \"%sTAGS\"" (file-truename expanded-path)))
    (princ (format "running %s" dmb-tags-args))
    
    (setq buf1 (get-buffer-create (format "*etags - %s *" expanded-path)))
    (when background
      (shell-command (concat dmb-tags-args " &") buf1 buf1))
    (when (not background)
      (shell-command dmb-tags-args buf1 buf1))))



;;;###autoload
(defun delete-whitespace ()
  "Delete characters from point up to next non-whitespace char"
  (interactive)
  (let ((here (point)))
    (skip-syntax-forward "-")
    (if (/= (point) here)
	(delete-region (point) here))))

;;;###autoload
(defadvice just-one-space (around delete-whitespace (&optional n) activate compile)
  "redefined the behavior of just-one-space, allow for universal
argument to delete only unneeded whitespace according to major
mode.."
  (interactive "*P")
  (if current-prefix-arg       (delete-whitespace)
    ad-do-it))


(autoload 'make-code-statement "dmb-code-statement" "" t)
(autoload 'unmake-code-statement "dmb-code-statement" "" t)

(autoload 'mass-replace-marked-files "dmb-mass-replace" "" t)
(autoload 'mass-replace-pattern-in-files "dmb-mass-replace" "" t)


;; TBA: Need to get these working for regions, perhaps.
;;;###autoload
(defun unix2dos ()
  ;; This function really does work now, changed `replace-string()'
  ;; to `replace-regexp()' which does the business !! *PP*
  "Convert this entire buffer from UNIX text file format to MS-DOS."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "$"  "\015" )
    (goto-char (point-max))
    (insert "\n\C-z")))

;;;###autoload
(defun dos2unix ()
  "Convert this entire buffer from MS-DOS text file format to UNIX."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "\r$" "" nil)
    (goto-char (1- (point-max)))
    (if (looking-at "\C-z")
        (delete-char 1))))


;;;###autoload
(defun archive-decompile-class-hook ()
  "a hook to automatically decompile classfiles from jar files"
  (princ (format "my-archive-extract-hook() %s %s %s" (current-buffer) buffer-file-name (string-match "\.class$" buffer-file-name)))
  (require 'jde)
  (require 'decompile)
  (if (string-match "\.class$" buffer-file-name)
      (jdc-buffer)))
(add-hook 'archive-extract-hooks 'archive-decompile-class-hook)


;;;###autoload
(defun dired-do-replace-regexp (from to &optional delimited)
  "Do `query-replace-regexp' of FROM with TO, on all marked files.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit], RET or q), you can resume the query replace
with the command \\[tags-loop-continue]."
  (interactive
   (let ((common
	  (query-replace-read-args
	   "Query replace regexp in marked files" t t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common))))
  (dolist (file (dired-get-marked-files nil nil 'dired-nondirectory-p))
    (message (format "file: %s" file))
    ;;(let ((buffer (get-file-buffer file)))
    ;;  (if (and buffer (with-current-buffer buffer
	 ;;	buffer-read-only))
    ;;  
	 ;; (error "File `%s' is visited read-only" file))
    (setq buffer (find-file-noselect file))
    (message (format "current buffer %s" buffer))
    (with-current-buffer buffer
      (message (format "%s" file))
      (while (re-search-forward from nil t)
        (replace-match to nil nil)))))


;;;###autoload
(defun zap-upto-char (arg char)
  "like `zap-to-char', but doesn't zap char"
  (interactive "p\ncZap up to char: ")
  (zap-to-char arg char)
  (insert char)
  (backward-char))

;;;###autoload
(defun truncate-shell-contents() 
  "remove the entire shell contents"
  (interactive)
  (set (make-local-variable 'comint-buffer-maximum-size) 0)  
  (comint-truncate-buffer))


;;http://www.emacswiki.org/cgi-bin/wiki/TransposeWindows
;;;###autoload
(defun swap-frames ()
  "swap the currently selected window buffer with the other windows buffer"
  (interactive)
  (when (> (count-windows) 1)
    (princ (format "%s, %s" (selected-frame) (next-window)))
    (setq buf-top (window-buffer (next-window)))
    (setq buf-bottom (window-buffer (selected-window)))
    (princ (format "top: %s bottom: %s" buf-top buf-bottom))
    (set-window-buffer (next-window) buf-bottom)
    (set-window-buffer (selected-window) buf-top)))

(defun explorer-from-here (&optional arg)
  "start an explorer session from the current buffers working directory."
 (interactive "P")
 (setq xyz (expand-file-name (directory-file-name default-directory)))
 (setq xyz (replace-regexp-in-string "\/" "\\\\" (directory-file-name xyz)))
 ;;(w32-shell-execute "open" "explorer" (concat "/e,/select," xyz)))
 ;;(shell-command (concat "explorer /n,/root, \"" xyz "\"")))
 
 (if current-prefix-arg 
     (w32-shell-execute "open" "explorer" (concat "/e,/select," xyz))
   (w32-shell-execute "open" "explorer" (concat "/n,/root," xyz))))

(defvar *tf* (get-buffer-create " *tf*")
  "output buffer for tf commands .")


(defun tf-checkout () 
  "use Team Foundation to checkout the current file"
  (interactive)
  (let ((buffer (buffer-file-name)))
    (with-current-buffer *tf*
      (toggle-read-only -1)
      ;;(buffer-disable-undo (current-buffer))
      (erase-buffer)
      (call-process "tf" nil t t "checkout" buffer)
      ))
  ;;(setq buffer-read-only nil)
  (toggle-read-only -1))

;;tf diff file\;version file.java /format:unified 
(defun tf-diff-unified () 
  "use Team Foundation to diff the current file"
  (interactive)
  (let ((tf-diff-filename (buffer-file-name))
        (buffer (buffer-file-name)))
    (with-current-buffer *tf*
      ;;(buffer-disable-undo (current-buffer))
      (erase-buffer)
      ;;(call-process "tf.exe" nil *tf* t (format " diff %s /format:unified" buffer))
      (call-process "tf" nil *tf* t "help")
      )))



(defun buffer-order-next-mark (arg)
  (interactive "p")
  (when (mark)
    (let* ((p (point))
           (m (mark))
           (n p)
           (count (if (null arg) 1 arg))
           (abscount (abs count))
           (rel
            (funcall
             (if (< 0 count) 'identity 'reverse)
             (sort (cons (cons 0 p)
                         (cons (cons (- m p) m)
                               (if mark-ring
                                   (mapcar (lambda (mrm)
                                             (cons (- mrm p) mrm))
                                           mark-ring)
                                 nil)))
                   (lambda (c d) (< (car c) (car d))))))
           (cur rel))
      (while (and (numberp (caar cur)) (/= (caar cur) 0))
        (setq cur (cdr cur)))
      (while (and (numberp (caadr cur)) (= (caadr cur) 0))
        (setq cur (cdr cur)))
      (while (< 0 abscount)
        (setq cur (cdr cur))
        (when (null cur) (setq cur rel))
        (setq abscount (- abscount 1)))
      (when (number-or-marker-p (cdar cur))
        (goto-char (cdar cur))))))

(defun buffer-order-prev-mark (arg)
  (interactive "p")
  (buffer-order-next-mark
   (if (null arg) -1 (- arg))))

(global-set-key [C-S-right] 'buffer-order-next-mark)
(global-set-key [C-S-left] 'buffer-order-prev-mark)


(global-set-key (kbd "C-c d") 'fc-insert-date)
(defun fc-insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ;;((not prefix) "%Y-%m-%d")
                 ((not prefix) "%A %Y-%m-%d %T%z")
                 ((equal prefix '(4)) "%d.%m.%Y")
                 (t "%A, %d. %B %Y")))
        (system-time-locale "en_US"))
    (insert (format-time-string format))))



(defun tidy-buffer-xml ()
  "Run Tidy HTML parser on current buffer. Function to run Tidy HTML parser on buffer, requires tidy"
  (interactive)
  (if (get-buffer "tidy-errs") (kill-buffer "tidy-errs"))
  (shell-command-on-region (point-min) (point-max)
    "tidy -config ~/.emacs.d/.tidyrc" t t)
  ;; (shell-command-on-region (point-min) (point-max)
  ;;   "tidy -xml -f /tmp/tidy-errs -q -i -wrap 72 -utf8 -omit" t t)
  (find-file-other-window "/tmp/tidy-errs")
  (other-window 1)
  (delete-file "/tmp/tidy-errs")
  (message "buffer tidy'ed")
 )


(defun count-buffer-by-mode (p-mode)
  "Count the number of buffers that match a mode"
  (setq buffer-matches -1)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      ;(princ (format "%s %s " (buffer-name buffer) mode-name))
      (if (and (not (string-match "\*etags -.*$" (buffer-name buffer))) (string-match p-mode mode-name))
          (setq buffer-matches (1+ buffer-matches)))))           ;(princ (format "adding buffer %s to buffer-matches new size %s " (buffer-name buffer) buffer-matches))     (princ "\n"))
  ;(princ (format "buffer matches: %s\n" buffer-matches))
  (1+ buffer-matches))


(defun cvs-diff-with (revision) 
  "perform ediff on a previous revision of current buffer"
  (interactive (list (read-string "revision: ")))
  (setq dmb-filename (buffer-file-name (current-buffer)))
  (setq dmb-previous-buffer (current-buffer))
  (setq just-filename (file-name-nondirectory dmb-filename))
  (save-excursion 
    (with-temp-buffer
      (insert-file-contents (concat (file-name-directory dmb-filename) "/CVS/Repository"))
      (re-search-forward "^.*$" nil t) ;trims \n off of Repository file contents
      (setq module-name (match-string 0))
      (setq module-filename (concat module-name "/" just-filename)))
    (setq dmb-filename-with-revision (concat (file-name-sans-extension just-filename)  "_" revision "." (file-name-extension just-filename)))
    (with-current-buffer (get-buffer-create dmb-filename-with-revision)
      (princ (format "module-name = %s\n" module-filename))
      (insert 
       (concat (shell-command-to-string 
                (format "cvs -q checkout -p -r %s %s" revision module-filename))))
      ;set-visited-file-name
      (write-file (concat temporary-file-directory "/" (file-name-nondirectory dmb-filename-with-revision)))
      (ediff-buffers dmb-previous-buffer (current-buffer)))))


(defun make-mongo-object-id ()
  "shell to mongo shell and create an object id"
  (interactive)
  ;;(insert (shell-command-to-string "/usr/local/bin/mongo --nodb --norc --quiet --eval 'ObjectId()' | perl -pe 's/\n//'")))
  (insert (shell-command-to-string "/usr/local/bin/mongo --nodb --norc --quiet --eval 'ObjectId()' | perl -pe 's/ObjectId\(.+\)\n/\${1}/'")))


;;
;; http://stackoverflow.com/a/998472/1160488
;; 
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

;;(global-set-key (kbd "C-d") 'duplicate-line)



(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line "
  (interactive "P")
  (let ((beg (line-beginning-position)) 
        (end (line-end-position arg)))
    (copy-region-as-kill beg end)))

(global-set-key (kbd "C-c l")         (quote copy-line))
(global-set-key (kbd "C-c p")         (quote duplicate-line))
