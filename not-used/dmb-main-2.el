;; (setq default-frame-alist
;;       '(
;;         ;(cursor-type bar . 3)
;;         (cursor-type bar . 4)
;;       ; (top . 0) (left . 150) 
;;       ;(width . 85) (height . 63)
;;         (cursor-color . "light slate blue")
;;       ;  (blink-cursor-delay 0)
;;        ;(cursor-color . "cornflower blue")
;;        (font . "-*-Lucida Console-normal-r-*-*-11-82-96-96-c-*-iso8859-1")))

(require 'info)

;;(load-file "~/emacs/dmb-minimal.el")

(require 'saveplace)

(when running-xemacs
  (paren-set-mode)
  (setq 
   progress-feedback-use-echo-area t 
   progress-feedback-style "small"
   lazy-lock-minimum-size 1024
   lazy-lock-hide-invisible nil
   lazy-lock-defer-on-scrolling t
   lazy-lock-defer-time 15
   lazy-lock-defer-on-the-fly t
   lazy-lock-defer-contextually t
   lazy-lock-walk-windows nil
   font-lock-always-fontify-immediately nil
   mswindows-alt-by-itself-activates-menu nil
   mwheel-follow-mouse t))

;;
;; use virtual-dired-mode for files saved with .dired extension
;;
(require 'dired)
(require 'dired-x)
;;(autoload 'dired-x "dired")
(autoload 'dired-virtual-mode "dired")
;;(require 'dired-x)
(setq auto-mode-alist (cons '("[^/]\\.dired$" . dired-virtual-mode)
                            auto-mode-alist))

(require 'grep)
;;(require 'grep+)
(setq 
  grep-window-height 25
  grep-hit-face 'dmb-grep-hit-face
  ;;grep-match-face 'dmb-grep-match-face
  grep-match-face 'dmb-face-three
  grep-scroll-output t
  grep-highlight-matches t
  grep-use-null-device nil)
(set-face-underline-p 'dmb-grep-match-face nil)
(set-face-underline-p 'compilation-line-number nil)

;;(set-face-background 





(add-hook 
 'grep-mode-hook 
 (lambda () 
   "hook to rename buffer to include find args"
   (if (string-match "\*grep\*" (buffer-name (current-buffer)))
       (progn (rename-buffer (format "*grep* [%s]" command-args) 1)))))

(add-hook 
 'dired-mode-hook 
 (lambda () 
   "hook to rename buffer to include find args"
   (if (string-match "\*Find\*" (buffer-name (current-buffer)))
       (progn (rename-buffer (format "*find* [%s]" find-args) 1)))))

(defun simple-grep1 (dir options)
  "a slightly more improved grep-find"
  ;;(interactive "Dgrep (directory): \nsgrep (directory): %s (like this): \ns ")
  (interactive (list (read-directory-name "grep (directory): ")
                     (read-from-minibuffer "grep (like this): " '("grep --include=\"*.*\" -nrH -e  ./*" . 30) nil nil 'grep-history)))
  (save-excursion
	 ;;(cd dir)
	 ;;run grep like: grep -Hnr --include="*.java" "OfferBroker" c:/khub/ReqRecSearchFilter/java/
	 ;;--exclude=\".#*\" --exclude=\"*\\~\"
	 (setenv "GREP_OPTIONS" "--exclude=.#* --exclude=*\~ --exclude=CVS/* --exclude=*.svn-base")
	 (grep (concat "cd " dir " && " options))))



(defun simple-grep2 (dir options)
  "a slightly more improved grep-find"
  ;;(interactive "Dgrep (directory): \nsgrep (directory): %s (like this): \ns ")
  (interactive (list (read-directory-name "grep (directory): ")
                     (read-from-minibuffer "grep (args): " '("grep --include=\"*.*\" -nrH -e  ./*" . 30) nil nil 'grep-history)))
  (save-excursion
    (setq buffer (get-buffer-create "*grep*"))
    
    (with-current-buffer buffer
      (cd dir)
      (setenv "GREP_OPTIONS" "--exclude=.#* --exclude=*\~ --exclude=CVS/* --exclude=*.svn-base")
      ;;fake out the grep-mode-hook by setting command-args to options
      (setq command-args options)
      ;;run grep like: grep -Hnr --include="*.java" "OfferBroker" c:/khub/ReqRecSearchFilter/java/
      ;;--exclude=\".#*\" --exclude=\"*\\~\"

      (compilation-start options 'grep-mode nil t)
      ;;(setq next-error-function 'dave-next-error)
)))




(defun simple-grep (dir mask regexp)
  "simplfied interface to find-dired.  Prompts for directory,
file mask, and regexp to look for.  Executes `find-dired'.
Uses the default find-file history list.  Also uses the default
`regexp-history' list.  Prompting pattern is similar to query-replace."
  (interactive "Dgrep (directory): \nsgrep (directory): %s (file mask): \nsgrep (directory): %s (file-mask): %s (grep regexp): ")
  (cd dir)
  ;;run grep like: grep -Hnr --include="*.java" "OfferBroker" c:/khub/ReqRecSearchFilter/java/
  ;;--exclude=\".#*\" --exclude=\"*\\~\"
  (setenv "GREP_OPTIONS" "--exclude=.#* --exclude=*\~ --exclude=CVS/*")
  (grep
   (format "grep  -Hrn --include=\"%s\" \"%s\" \"%s\""
            mask regexp dir)))

(defun x-find-grep ()
  "simplfied interface to find-dired.  Prompts for directory, 
file mask, and regexp to look for.  Executes `find-dired'.
Uses the default find-file history list.  Also uses the default
`regexp-history' list.  Prompting pattern is similar to query-replace."
  (interactive)
  
  (set 'dmb-dir 
       (read-file-name "directory: " default-directory default-directory t))
  
  (cd dmb-dir)
  (grep-find))


;;
;; Replace "yes or no" with y or n
;;
(defun yes-or-no-p (arg)
  "An alias for y-or-n-p, because I hate having to type 'yes' or 'no'."
  (y-or-n-p arg))




;;
;; load python mode
;;
(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))

(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))

(setq interpreter-mode-alist
      (cons '("python2.4" . python-mode) interpreter-mode-alist))

(autoload 'python-mode "python-mode" "Python editing mode." t)



;;
;; recent
;;
(when running-gnu-emacs
  (setq-default
   recentf-max-menu-items 30
   recentf-max-saved-items 100
   recentf-menu-path nil
   recentf-menu-title "Recent"
   backup-inhibited nil
   delete-old-versions t
   version-control t
   indent-tabs-mode nil)   
  (require 'recentf)
  (recentf-mode))




;;
;; fix indentation and some other stuff
;;
;; (add-hook
;; 'c-mode-common-hook
;; (lambda ()
;;    (setq c-basic-offset 3
;;          c-comment-only-line-offset '(0 . 0)
;;          c-block-comments-indent-p nil
;;          c-recognize-knr-p nil
;;          )
;;    (define-key c-mode-base-map "\C-m" 'newline-and-indent)
;;    ;(c-set-offset 'label -1000)
;;    (c-set-offset 'substatement-open 0)
;;    (c-set-offset 'access-label -2)
;;    (c-set-offset 'func-decl-cont 0)
;;    (c-set-offset 'arglist-intro '++)
;;    (c-set-offset 'arglist-close '+)
;;    (c-set-offset 'block-open '+)
;;    (c-set-offset 'case-label '+)
;;    (c-set-offset 'statement-case-open 0)
;;    '(lambda () (c-toggle-auto-hungry-state 1))
;;    '(lambda () (c-toggle-auto-newline-state 1))
;;    ))


;;
;; Printing
;;
(setq ps-printer-name t)
(require 'ps-print)
(setq ps-lpr-command (concat devtools-dir "/Ghostgum/gsview/gsprint.exe"))
;; THis line causes ghostscript to query which printer to
;; use - which you may not need if, for example, you only
;; have one printer.
(setq ps-lpr-switches '("-query"))
(custom-set-variables
   ;'(blink-cursor nil)
   '(ps-header-font-size 6)
   '(ps-line-number t)
   '(ps-header-title-font-size '(6 . 6))
   '(ps-header-offset 10)
   '(ps-header-line-pad 0.10)
   ;'(ps-top-margin 2)
   ;'(ps-landscape-mode t)
   ;'(ps-n-up-printing 4)
   ;'(ps-n-up-filling (left-top))
 )





;;
;; Truncation, and allow it to be toggled
;;
;;(setq default-truncate-lines t             ; start off truncating long lines
;; truncate-partial-width-windows nil)
(toggle-truncate-lines 1) ;; no-wrap




(setq animals '(gazelle giraffe lion tiger))

(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))

;(print-elements-of-list animals)






;; TBA: Need to get these working for regions, perhaps.
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

(defun dos2unix ()
  "Convert this entire buffer from MS-DOS text file format to UNIX."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "\r$" "" nil)
    (goto-char (1- (point-max)))
    (if (looking-at "\C-z")
        (delete-char 1))))

;;
;; Emacs Code Browser ( ECB )
;;
(add-to-list 'load-path (expand-file-name "~/emacs/packages/ecb-snap"))
(require 'ecb-autoloads)
;;(ecb-activate)


;;better buffer menu
(require 'bs)
(setq
 ;;bs--show-all t
 bs-default-configuration "all"
 ;;bs-default-configuration "all-intern-last"
 ;;bs-default-configuration "dmb-files"
 ;;bs-default-configuration "dmb-files-and-scratch"
 ;;bs-buffer-sort-function 'bs-sort-buffer-interns-are-last
 bs-max-window-height 35)


;;#997009


(add-to-list 'bs-mode-font-lock-keywords '("^..\\(.*SQLi.*\\)$" 1 'dmb-face-three))
(add-to-list 'bs-mode-font-lock-keywords '("^..\\(.*JDE .*\\)$" 1 'dmb-face-one)) ;; font-lock-keyword-face font-lock-variable-name-face
(add-to-list 'bs-mode-font-lock-keywords '("^..\\(.*SGML.*\\)$" 1 'dmb-face-five)) ;;font-lock-builtin-face
(add-to-list 'bs-mode-font-lock-keywords '("^..\\(.*Shell.*\\)$" 1 'dmb-face-two))
(add-to-list 'bs-mode-font-lock-keywords '("^..\\(.*Emacs-Lisp.*\\)$" 1 'dmb-face-four))


;; (add-to-list 'bs-configurations
;; 	     '("java" nil nil "^[^#]" nil nil))
(add-to-list 'bs-configurations
	     '("dired" nil nil nil (lambda (buf)
				     (with-current-buffer buf
				       (not (eq major-mode 'dired-mode)))) nil))

(add-to-list 'bs-configurations
	     '("java" nil nil nil (lambda (buf)
				     (with-current-buffer buf
				       (not (eq major-mode 'jde-mode)))) nil))




(add-to-list 'bs-configurations
             '("SQL" nil nil nil
               (lambda (buf)
                 (with-current-buffer buf
                   (not (memq major-mode
                              '(sql-interactive-mode sql-mode))))) nil))


(add-to-list 'bs-configurations
             '("dmb-files-and-scratch" nil nil nil
               (lambda (buf)
                 (bs-config-clear)
                 ;;(setq 
                 ;; bs-buffer-sort-function 'bs-sort-buffer-interns-are-last)
                 (with-current-buffer buf
                   (not (memq major-mode
                              '(sql-interactive-mode sql-mode jde-mode shell-mode python-mode))))) nil))




(defun bs-diff-marked-buffers ()
  "Perform diff of files marked in buffer list, requires `bs'"
  (interactive)
  (when (= (safe-length bs--marked-buffers) 2)
    (ediff-buffers (nth 0 bs--marked-buffers) (nth 1 bs--marked-buffers)))
  
  (when (= (safe-length bs--marked-buffers) 3)
    (ediff3 (buffer-file-name (nth 0 bs--marked-buffers))
            (buffer-file-name (nth 1 bs--marked-buffers)) 
            (buffer-file-name (nth 2 bs--marked-buffers)))))


(defun bs-query-replace-marked-buffers (regexp to-string)
  "Perform a `query-replace-regexp' marked files, requires `bs'"
  (interactive (list (read-from-minibuffer "replace regexp: ") 
                     (read-from-minibuffer "with what: ")))
  (dolist (buffer bs--marked-buffers)
    (save-window-excursion
      (delete-other-windows)
      (switch-to-buffer buffer)
      (query-replace-regexp regexp to-string nil (point-min) (point-max)))))

(defun bs-close-marked-buffers ()
  "close all marked buffers"
  (interactive)
  (dolist (buffer bs--marked-buffers)
    (save-window-excursion
      (switch-to-buffer buffer)
      (kill-current-buffer)))
  (bs--redisplay t))
(define-key bs-mode-map (kbd "C") 'bs-close-marked-buffers)



(defun dmb-bs-delete ()
  "Kill buffer on current line."
  (interactive)
  (let ((current (bs--current-buffer))
	(inhibit-read-only t))
    ;(unless (kill-buffer current)
    ;  (error "Buffer was not deleted"))
    (kill-buffer current)
    (setq bs-current-list (delq current bs-current-list))
    (beginning-of-line)
    (delete-region (point) (save-excursion
			     (end-of-line)
			     (if (eobp) (point) (1+ (point)))))
    (if (eobp)
	(progn
	  (backward-delete-char 1)
	  (beginning-of-line)
	  (recenter -1)))
    (bs--set-window-height)))
(define-key bs-mode-map "d" 'dmb-bs-delete)



;(set-register ?z '(file . "~/notes/notes.txt"))

;; (require 'session)
;; (add-hook 'after-init-hook 'session-initialize)



;; (add-hook 'mail-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'font-lock-comment-face)
;;                  'mail-quotation-face)))

;; (defun kill-buffers-matching (regexp)
;; (interactive "sKill buffers matching: ")
;; (dolist (i (buffer-list))
;;    (when (string-match regexp (buffer-name i))
;;      (kill-buffer i))))


(defun reset-minibuffer()
  "remove all text from the minibuffer"
  (interactive)
  (save-window-excursion
    (select-window (minibuffer-window))
    (erase-buffer)))



;;
;; pmd
;;
;(add-to-list 'load-path (expand-file-name "~/emacs/packages/pmd-emacs-0.5"))
;(require 'pmd)


;;
;; tramp
;;
;;; (add-to-list 'load-path (expand-file-name "~/emacs/packages/tramp-2.1.5/lisp"))
;;; ;;(require 'tramp)
;;; (add-to-list 'Info-default-directory-list "~/emacs/packages/tramp-2.1.5/info")
(autoload 'tramp "tramp")


;;archive extract hook

(defun archive-decompile-class-hook ()
  "a hook to automatically decompile classfiles from jar files"
  (princ (format "my-archive-extract-hook() %s %s %s" (current-buffer) buffer-file-name (string-match "\.class$" buffer-file-name)))
  (require 'jde)
  (require 'decompile)
  (if (string-match "\.class$" buffer-file-name)
      (jdc-buffer)))
(add-hook 'archive-extract-hooks 'archive-decompile-class-hook)


;;
;; beautify java imports
;;
;;(require 'beautify-imports)

;;
;; highlight current line
;;
;;(require 'highlight-current-line)





(defun back-to-indentation-or-beginning ()
   (interactive)
   (if (= (point) (save-excursion (back-to-indentation) (point)))
       (beginning-of-line)
     (back-to-indentation)))

(defun beginning-or-indentation (&optional n)
  "Move cursor to beginning of this line or to its indentation.
  If at indentation position of this line, move to beginning of line.
  If at beginning of line, move to beginning of previous line.
  Else, move to indentation position of this line.

  With arg N, move backward to the beginning of the Nth previous line.
  Interactively, N is the prefix arg."
  (interactive "P")
  (cond ((or (bolp) n)
         (forward-line (- (prefix-numeric-value n))))
        ((save-excursion (skip-chars-backward " \t") (bolp)) ; At indentation.
         (forward-line 0))
        (t (back-to-indentation))))

;(global-set-key (kbd "<home>") 'back-to-indentation-or-beginning)
;(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)

;;(global-set-key (kbd "<home>") 'beginning-or-indentation)
;;(global-set-key (kbd "C-a") 'beginning-or-indentation)

;;
;;Don't echo passwords when communicating with interactive programs
;;
;;(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)




;;
;; strip ^M from compilation mode
;;
(defun my-compilation-mode-hook ()
  (setq truncate-lines nil)
  (font-lock-mode t)) 
(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)
;(add-hook 'compilation-mode-hook 'comint-strip-ctrl-m); generates error: incremental parser error: "Wrong type argument: consp, nil"






(require 'ediff)
;; ;; turn off the gray highlighting
;; (add-hook
;;  'ediff-startup-hook
;;  '(lambda ()
;;     (ediff-toggle-hilit)))

;(setq ediff-keep-variants t)
;; Some custom configuration to ediff
(defvar my-ediff-bwin-config nil "Window configuration before ediff.")
(defcustom my-ediff-bwin-reg ?b
  "*Register to be set up to hold `my-ediff-bwin-config'
    configuration.")

(defun my-ediff-bsh ()
  "Function to be called before any buffers or window setup for
    ediff."
  (remove-hook 'ediff-quit-hook 'ediff-cleanup-mess)
  (window-configuration-to-register my-ediff-bwin-reg))

(defun my-ediff-aswh ()
"setup hook used to remove the `ediff-cleanup-mess' function.  It causes errors."
  (remove-hook 'ediff-quit-hook 'ediff-cleanup-mess))

(defun my-ediff-qh ()
  "Function to be called when ediff quits."
  (remove-hook 'ediff-quit-hook 'ediff-cleanup-mess)
  (ediff-cleanup-mess)
  (jump-to-register my-ediff-bwin-reg))

(add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'my-ediff-aswh);
(add-hook 'ediff-quit-hook 'my-ediff-qh)


;;getting an error when this hook is registered here: Debugger entered--Lisp error: (wrong-type-argument number-or-marker-p nil)
;; so removing this hook.
;(remove-hook 'ediff-quit-hook 'ediff-cleanup-mess)
;(remove-hook 'ediff-quit-hook 'my-ediff-qh)
;(add-hook 'ediff-quit-hook 'ediff-cleanup-mess)

;senator-narrow-to-defun
;senator-mark-defun 
;(require 'jde)


;(setq
; timer 
; (run-with-idle-timer 5 t 
;                      (lambda () 
;                        ""
;                        (message "idle timer...")
;                        recentf-save-list)))
                           

(add-to-list 'same-window-buffer-names "*Apropos*")
(add-to-list 'same-window-buffer-names "*Backtrace*")
(add-to-list 'same-window-regexps "^\\*debug.*\\*$")
;(add-to-list 'same-window-regexps "^\\*grep.*\\*$")
(add-to-list 'same-window-buffer-names "*Async Shell Command*")


;(define-key sql-mode-map [delete] 'delete-char)



;;(make-comint-in-buffer "dave" nil "tail" nil "-f" "e:/khub/CacheProto2.x/jboss/server/lms1/log/server.log")


;(make-comint-in-buffer "logs@10.11.241.60" nil "plink" nil "-pw" "logv13r" "logs@10.11.241.60" "tail -f ~prod/logs/lms1/startup.log")
;(make-comint-in-buffer "logs@10.11.241.60" nil "plink" nil "-pw" "logv13r" "logs@10.11.241.60" "tail -f ~prod/logs/lms1/startup.log")






(require 'python-mode)
(add-hook 'py-mode-hook 'lambda () 
          (define-key py-mode-map (kbd "C-c C-c") 'comment-region))

;; this hook is really annoying, so remove it...this hook really slows
;; down exiting emacs after having opened a lot of big java files.
;(remove-hook 'kill-emacs-hook 'semanticdb-kill-emacs-hook)
;; and since it re-adds itself anytime you open a java file 
;; remove the hook when you open a file
(add-hook 
 'find-file-hook 
 '(lambda()
    (remove-hook 'kill-emacs-hook 'semanticdb-kill-emacs-hook)))

;; add this so that files recently opened don't just 
;; get added to the list but get saved also
;(add-hook 'find-file-hooks 'recentf-save-list)


;;fix this, semantic-db requires that this directory exists
;;
(when (not (file-directory-p semanticdb-default-save-directory))
  (make-directory semanticdb-default-save-directory))
           

;(require 'session)
;(session-initialize)

(defun dired-ediff-marked-files ()
  "Run ediff on marked ediff files."
  (interactive)
  (set 'marked-files (dired-get-marked-files))
  (when (= (safe-length marked-files) 2)
    (ediff-files (nth 0 marked-files) (nth 1 marked-files)))
  
  (when (= (safe-length marked-files) 3)
    (ediff3 (buffer-file-name (nth 0 marked-files))
            (buffer-file-name (nth 1 marked-files)) 
            (buffer-file-name (nth 2 marked-files)))))



(defun sof/dired-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
   (let (buffer-read-only)
     (forward-line 2) ;; beyond dir. header  
     (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))

 (add-hook 'dired-after-readin-hook 'sof/dired-sort)





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




;;(setq backup-directory-alist (cons '(".*\\.java$" . "~/backup") backup-directory-alist))
;;(setq backup-directory-alist (cons '("." . "~/backup") backup-directory-alist))

(setenv "PATH" (concat cygwin-dir "/bin" ";" (getenv "PATH")))
(setenv "PATH" (concat (getenv "EMACSPATH") ";" (getenv "PATH")))
(setenv "PATH" (concat devtools-dir "/svn-1.4.3/bin" ";" (getenv "PATH")))


(defun make-tags-file (path background)
  "simple function to create TAGS file"
  (interactive 
   (list (read-file-name "path: ") t))

  (if (not (file-directory-p path))
      (error "Start path specified is not an existing directory"))

  (cd path)

  (setq truefilename (file-truename path)
        ;;dmb-tags-args (format "find %s -name \"*.java\" -print | ctags -e --language-force=java --members --output=%sTAGS -" (file-truename path) (file-truename path)))
        ;;dmb-tags-args (format "ctags -R -e --language-force=java -f %sTAGS" (file-truename path) (file-truename path)))
        dmb-tags-args (format "ctags -R -e --extra=+q --langmap=SQL:.sql.pks  -V -f \"%sTAGS\"" (file-truename path)))
  (princ (format "running %s" dmb-tags-args))
  
  (setq buf1 (get-buffer-create (format "*etags - %s *" path)))
  (when background
    (shell-command (concat dmb-tags-args " &") buf1 buf1))
  (when (not background)
    (shell-command dmb-tags-args buf1 buf1)))
   
;"2:45pm"
;; (run-at-time 
;;  "11:23pm" 86400 
;;  '(lambda()  
;;     (dolist (dir (directory-files "~/khub/" t))
;;       (setq branch-dir (concat dir "/java/"))
;;       (message (format "dir: %s, exists?: %s" branch-dir (file-directory-p branch-dir)))
;;       (if (file-directory-p branch-dir)
;;           (make-tags-file branch-dir nil)))))

(defun my-find-tag-hook()
  "remember files I open with `find-tag'"
  ;;(message (format "you are here %s" (buffer-file-name (current-buffer))))
  (add-to-list 'file-name-history (buffer-file-name (current-buffer))))

(add-hook 
 'find-tag-hook 'my-find-tag-hook)
    


(defun bs-indent-marked-buffers() 
  "For all marked buffers in the buffer list run `indent-region' on them."
  (interactive)
  (dolist (buffer bs--marked-buffers)
    (with-current-buffer buffer
      (save-window-excursion
        (message (format "%s %s\n" (buffer-name buffer) mode-name))
        (indent-region (point-min) (point-max))))))

(require 'pmd)

(setq auto-mode-alist (cons '("\\.jsp$" . xml-mode) auto-mode-alist))


(autoload 'wikipedia-mode
  "wikipedia-mode.el"
  "Major mode for editing documents in Wikipedia markup." t)

(setq auto-mode-alist
      (cons '("\\.wiki\\'" . wikipedia-mode) auto-mode-alist))

(add-to-list 'auto-mode-alist '("mozex.\\.*" . wikipedia-mode))
(add-hook 'wikipedia-mode-hook
          '(lambda() "" 
             
             (define-key outline-mode-map [\C-left] nil)
             (define-key outline-mode-map [\C-right] nil)
             (define-key outline-mode-map [\M-left] nil)
             (define-key outline-mode-map [\M-right] nil)
             
             (define-key wikipedia-mode-map [\C-left] nil)
             (define-key wikipedia-mode-map [\C-right] nil)
             (define-key wikipedia-mode-map [\M-left] nil)
             (define-key wikipedia-mode-map [\M-right] nil)))


;;(add-to-list 'same-window-regexps "^\\*sql\\*.*")

(setq auto-mode-alist
      (cons '("\\.java\\'" . jde-mode) auto-mode-alist))


(defun truncate-shell-contents() 
  "remove the entire shell contents"
  (interactive)
  (set (make-local-variable 'comint-buffer-maximum-size) 0)  
  (comint-truncate-buffer))


;;remove this association, for now....
;; assq-delete-all key alist
;;(setq auto-mode-alist (delq "\\.ins\\'" auto-mode-alist ) auto-mode-alist)
;;(assq-delete-all "\\.ins\\'" auto-mode-alist)


(setq auto-mode-alist
      (cons '("\\.ins\\'" . sql-mode) auto-mode-alist))


;; enable highlighting regexps


(add-to-list 'same-window-regexps "^\\*etags\\*.*")



(defun display-prefix (arg)
  "Display the value of the raw prefix arg."
  (interactive "P")
  (message "%s" arg))





(defun force-read-comint-input-ring() 
  ""
  (interactive)
  (comint-read-input-ring)
  (message (format "%s" comint-input-ring)))

(defun force-write-comint-input-ring() 
  ""
  (interactive)
  (comint-write-input-ring)
  (message (format "%s" comint-input-ring)))


(defun kill-etags-buffers()
  "kills buffers with name like *etags -"
  (interactive)
  (princ (format "\nkilling\n"))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if (string-match "\*etags -.*$"  (buffer-name))
          (progn (princ (format "\n  %s" buffer))
                 (kill-buffer buffer)
                 nil)))))


;;regexp-opt


;; (define-minor-mode show-tabs-mode
;;   "minor mode shows a face for tabs"
;;   nil
;;   "STM"
  
;;
;; show tabs as ^I
;; http://www.emacswiki.org/cgi-bin/wiki/ShowWhiteSpace
(defvar dmb-show-tabs nil 
  "indicates to show tabs")

;;(define-minor-mode hi-lock-mode
(defun toggle-show-tabs() 
  ""
  (interactive)
  
  (message "toggling tabs")
  (if dmb-show-tabs
      (standard-display-ascii ?\t "	")
    (standard-display-ascii ?\t "^I"))
  (setq dmb-show-tabs (not dmb-show-tabs))
  (force-window-update))


(defface dmb-blink-region-face
  '((((class color) (background light))
     (:background "lavendar")) ;;gray93
    (((class color) (background dark))
     (:background "light steel blue"))
    (((class grayscale monochrome)
      (background light))
     (:background "black"))
    (((class grayscale monochrome)
      (background dark))
     (:background "white")))
  "face to use for current line")


(defvar dmb-region-overlay nil
  "overlay for current line")

(defun blink-region ()
  ""
  (interactive)
  
  (setq dmb-region-overlay 
        (make-overlay (region-beginning) (region-end)))
  (overlay-put dmb-region-overlay 'face 'dmb-blink-region-face)
  (run-with-timer 0.50 nil 
                  '(lambda() 
                     (delete-overlay dmb-region-overlay)
                     (setq dmb-region-overlay nil))))




(defun dmb-count() 
  ""
  (interactive)
  (message "staring count...")
  (setq patterns '("^2006\.03\.28" "^2006\.03\.29" "^2006\.03\.30" "^2006\.03\.31" "^2006\.04\.01" "^2006\.04\.02" "^2006\.04\.03" "^2006\.04\.04"  "^2006\.04\.05"  "^2006\.04\.06" "^2006\.04\.07"  "^2006\.04\.08" "^2006\.04\.09" "^2006\.04\.10" "^2006\.04\.11" "^2006\.04\.12" "^2006\.04\.13" "^2006\.04\.14"))
  
  (while patterns
    (message (format "%s" (count-matches (car patterns))))
    (setq patterns (cdr patterns))))



(defun pg-kill-this-line (n)
  "Kill the line point is on.
  With prefix arg, kill this many lines starting at the line point is on."
  (interactive "p")
  (kill-region (line-beginning-position)
               (progn (forward-line n) (point))))

(defun pg-duplicate-this-line (n)
  "Duplicates the line point is on.  
 With prefix arg, duplicate current line this many times."
  (interactive "p")
  (save-excursion 
    (copy-region-as-kill (line-beginning-position) 
                         (progn (forward-line 1) (point)))
    (while (< 0 n)
      (yank)
      (setq n (1- n)))))

(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "M-<up>") 'move-line-up)



(defun isearch-yank-symbol ()
  "Put symbol at current point into search string."
  (interactive)
  (let ((sym (find-tag-default)))
    (if (null sym)
        (message "No symbol at point")
      (isearch-yank-regexp
       (concat "\\_<" (regexp-quote sym) "\\_>")))))




(require 'vc)
(require 'psvn)
(add-to-list 'vc-handled-backends `SVN)

(require 'hi-lock)
;;(set-face-background 'hi-green "dark khaki")
(set-face-background 'hi-green nil)
(set-face-foreground 'hi-green "PaleGreen2")


(defface hi-light-goldenrod
  '((((class color)) (:background "light goldenrod")))
  "additional face for hi-lock."
  :group 'hi-lock-faces)

(defface hi-lemon-chiffon
  '((((class color)) (:background "lemon chiffon")))
  "additional face for hi-lock."
  :group 'hi-lock-faces)


(defface hi-indian-red
  '((((class color)) (:background "IndianRed1")))
  "additional face for hi-lock."
  :group 'hi-lock-faces)

(defface hi-indian-red-fg
  '((((class color)) (:foreground "IndianRed1")))
  "additional face for hi-lock."
  :group 'hi-lock-faces)

;; (defun dave-next-error(args reset)
;;   ""
;;   (interactive)
;;   (message "you ahre here")
;;   (while (compilation-next-error-function args reset)
;;     (message "current buffer %s" (buffer-name (current-buffer)))))




(defun refontify(&optional beg end)
  ""
  (interactive)
  (message (format "beg: %s end: %s" beg end))
  (jit-lock-refontify beg end)) 

;;(require 'dired+)
;;(require 'w32-browser)

;;(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))
(eval-after-load "dired-x" 
  '(progn 
     (setq dired-omit-files-p t)
     (add-to-list 'dired-omit-extensions  "CVS")
     ;;(require 'dired-view)
     ;;(add-hook 'dired-mode-hook 'dired-view-minor-mode-on)
     (define-key dired-mode-map (kbd ";") 'dired-view-minor-mode-toggle)
     (define-key dired-mode-map (kbd ":") 'dired-view-minor-mode-dired-toggle)
     ))

(autoload 'make-code-statement "code-statement" "" t )
(autoload 'unmake-code-statement "code-statement" "" t)


(defun operate-on-matched-files ()
  ""
  (interactive))


(require 'goto-last-change)
(global-set-key "\C-x\C-\\" 'goto-last-change)

(defun scratch-create-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))  




;;(add-to-list 'load-path "~/emacs/packages/muse/lisp")
;;(add-to-list 'load-path "~/emacs/packages/planner")
;;(require 'planner)

;;(setq planner-directory "~/Plans")

;; (when (and (locate-library "planner")
;;            (locate-library "muse"))
;;   (setq planner-project "~/WikiPlans")
;;   (setq planner-directory "~/Plans")
;;   (setq planner-default-page "TaskPool")
;;   ;; Tell muse about planner.  We use add-hook instead of
;;   ;; add-to-list because muse might not be loaded yet.
;;   (add-hook 'muse-project-alist
;;             (list planner-project
;;                   (list planner-directory
;;                         :default planner-default-page
;;                         :major-mode 'planner-mode
;;                         :visit-link 'planner-visit-link))))

;; (setq planner-project "WikiPlanner")
;; (setq muse-project-alist
;;       '(("WikiPlanner"
;;          ("~/Plans" ;; where your Planner pages are located
;;           :default "TaskPool" ;; use value of `planner-default-page'
;;           :major-mode planner-mode
;;           :visit-link planner-visit-link)
     
;;          ;; This next part is for specifying where Planner pages
;;          ;; should be published and what Muse publishing style to
;;          ;; use.  In this example, we will use the XHTML publishing
;;          ;; style.
     
;;          (:base "planner-xhtml"
;;                 ;; where files are published to
;;                 ;; (the value of `planner-publishing-directory', if
;;                 ;;  you have a configuration for an older version
;;                 ;;  of Planner)
;;                 :path "~/public_html/Plans"))))


(add-to-list 'load-path (expand-file-name "~/emacs/packages/icicles"))
(autoload 'icicle-mode "icicles" "icicle mode." t)

(require 'icicles)

;;(require 'icicles-iswitchb)

;;; I prefer modal cycling.
(setq icicle-cycling-respects-completion-mode-flag nil)
;;  I HATE arrow keys.
(setq icicle-modal-cycle-up-key "\C-p")
(setq icicle-modal-cycle-down-key "\C-n")
;;icicle-next-apropos-candidate
;;icicle-previous-apropos-candidate


(custom-set-variables  
 '(icicle-control-reminder-prompt nil)
 '(icicle-require-match-flag 'no-match-required) ;;'partial-match-ok)
 '(icicle-buffer-require-match-flag  'no-match-required)
 ;;'(icicle-touche-pas-aux-menus-flag t)
 '(icicle-completing-prompt-prefix "")
 ;;'(icicle-incremental-completion-delay 2.0)
 '(icicle-WYSIWYG-Completions-flag 4)
 '(icicle-prompt-suffix ""))

(custom-set-faces
 '(diredp-compressed-file-suffix ((t (:foreground "goldenrod" ))))
;;  '(diredp-date-time)
;;  '(diredp-deletion)
;;  '(diredp-deletion-file-name)
  '(diredp-dir-heading ((t (:foreground "goldenrod" ))))
  '(diredp-dir-priv ((t (:foreground "blue4" ))))
;;  '(diredp-display-msg)
 '(diredp-exec-priv ((t (:foreground "DarkGreen" ))))
;;  '(diredp-executable-tag)
  '(diredp-file-name ((t (:foreground "black" ))))
;;  '(diredp-file-suffix)
;;  '(diredp-flag-mark)
;;  '(diredp-flag-mark-line)
;;  '(diredp-ignored-file-name)
;;  '(diredp-link-priv)
  '(diredp-no-priv ((t (:foreground "black" ))))
 
  '(diredp-other-priv ((t (:foreground "blue4" ))))
;;  '(diredp-rare-priv)
  '(diredp-read-priv ((t (:foreground "blue4" ))))
;;  '(diredp-symlink)
 '(diredp-write-priv ((t (:foreground "blue4" ))) ))

(icicle-mode 1)                         ; Turn on Icicle mode.


(require 'isearch+)
(custom-set-variables
 '(isearchp-set-region-flag t))

(require 'replace+)
(custom-set-variables
 '(replace-w-completion-flag nil))
;;(global-set-key "\M-%" 'query-replace-w-options)
(global-set-key "\M-%" 'query-replace)

;; (require 'locate)
;; (icicle-define-command icicle-locate ; Command name
;;   "Run the program `locate', then visit files.
;; Unlike `icicle-locate-file' this command is a wrapper for the program `locate'." ; Doc string
;;   find-file                             ; Function to perform the action
;;   "File: " (mapcar #'list (split-string (shell-command-to-string (format "%s '%s'" locate-command query)) "\n" t))
;;   nil t nil 'locate-history-list nil nil
;;   ((query (read-string "Locate: "))))

;; (custom-set-faces
;;  '(icicle-complete-input ((((background dark)) nil)))
;;  '(icicle-completing-mustmatch-prompt-prefix ((((type x w32 mac     graphic) (class color)) (:foreground "Cyan"))))
;;  '(icicle-completing-prompt-prefix ((((type x w32 mac graphic) (class     color)) (:foreground "Red"))))
;;  '(icicle-current-candidate-highlight ((((background dark))     (:background "gray20"))))
;;  '(icicle-historical-candidate ((((background dark)) (:foreground     "White"))))
;;  '(icicle-prompt-suffix ((((type x w32 mac graphic) (class color)     (background dark)) (:foreground "LightSlateBlue"))))
;;  '(icicle-search-context-level-1 ((((background dark)) (:background     "gray20" :weight bold))))
;;  '(icicle-search-context-level-2 ((((background dark)) (:background     "gray15" :weight bold))))
;;  '(icicle-search-context-level-3 ((((background dark)) (:background     "gray10" :weight bold))))
;;  '(icicle-search-context-level-4 ((((background dark)) (:weight     bold))))
;;  '(icicle-search-context-level-5 ((((background dark)) (:weight     bold))))
;;  '(icicle-search-context-level-6 ((((background dark)) (:weight     bold))))
;;  '(icicle-search-context-level-7 ((((background dark)) (:weight     bold))))
;;  '(icicle-search-context-level-8 ((((background dark)) (:weight     bold))))
;;  '(icicle-search-current-input ((t (:foreground "green"))))
;;  '(icicle-search-main-regexp-current ((((background dark))     (:foreground "DodgerBlue"))))
;;  '(icicle-search-main-regexp-others ((((background dark)) (:foreground     "SeaGreenk"))))
;;  '(icicle-special-candidate ((((background dark)) (:foreground     "yellow"))))
;;  '(icicle-whitespace-highlight ((t (:background "#300")))))



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


(setq scroll-conservatively 10000)


;;http://www.emacswiki.org/cgi-bin/wiki/TransposeWindows
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
  

(defun swap-frames-2 ()
  "Delete all frames except FRAME.
If FRAME uses another frame's minibuffer, the minibuffer frame is
left untouched.  FRAME nil or omitted means use the selected frame."
  (interactive)
  (let* ((mini-frame (window-frame (minibuffer-window frame)))
	 (frames (delq mini-frame (delq frame (frame-list)))))
    ;; Delete mon-minibuffer-only frames first, because `delete-frame'
    ;; signals an error when trying to delete a mini-frame that's
    ;; still in use by another frame.
    (dolist (frame frames)
      (unless (eq (frame-parameter frame 'minibuffer) 'only)
        (delete-frame frame)))
    ;; Delete minibuffer-only frames.
    (dolist (frame frames)
      (when (eq (frame-parameter frame 'minibuffer) 'only)
	(delete-frame frame)))))
(message "dmb-main.el done.")
  



(defun md5-region (begin end) 
  ""
  (interactive "r")
  (while (re-search-forward ".*" end t)
   (replace-match (md5 (match-string)) end nil)))

  
             

;; ;; Author: Patrick Gundlach 
;; ;; nice mark - shows mark as a highlighted 'cursor' so user 'always' 
;; ;; sees where the mark is. Especially nice for killing a region.

;; (defvar pg-mark-overlay nil
;;   "Overlay to show the position where the mark is") 
;; (make-variable-buffer-local 'pg-mark-overlay)

;; (put 'pg-mark-mark 'face 'secondary-selection)
;; ;;(put 'pg-mark-mark 'face 'dmb-mark-face)

;; (defvar pg-mark-old-position nil
;;   "The position the mark was at. To be able to compare with the
;; current position")

;; (defun pg-show-mark () 
;;   "Display an overlay where the mark is at. Should be hooked into 
;; activate-mark-hook" 
;;   (unless pg-mark-overlay 
;;     (setq pg-mark-overlay (make-overlay 0 0))
;;     (overlay-put pg-mark-overlay 'category 'pg-mark-mark))
;;   (let ((here (mark t)))
;;     (when here
;;       (move-overlay pg-mark-overlay here (1+ here)))))

;; (defadvice  exchange-point-and-mark (after pg-mark-exchange-point-and-mark)
;;   "Show visual marker"
;;   (pg-show-mark))

;; (ad-activate 'exchange-point-and-mark)
;; (add-hook 'activate-mark-hook 'pg-show-mark)



;;(add-to-list 'load-path (expand-file-name "~/emacs/packages/predictive"))
;;(autoload 'predictive-mode "~/emacs/packages/predictive/predictive.el")

;;(global-set-key [(control ?x) (control ?r)] 'recentf-open-files-compl)

(setq message-log-max 2000)
(when (boundp 'mouse-1-click-follows-link) ; Do not use mouse-1 to follow links.
  (setq mouse-1-click-follows-link nil))   ; Other values to consider: 100, `double'.
                                        ;    The default is bad.





(defalias 'qrr 'query-replace-regexp)

;;align-regexp


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



(mouse-sel-mode -1)

;;(require 'winpoint)
;;(window-point-remember-mode 1)

;; setup fringe on the right, and not on the left make the size of the
;; fringe smaller than the default(8)
;;(set-fringe-mode (cons 0 5))

(require 'cygwin-mount)
(setq cygwin-mount-cygwin-bin-directory (concat cygwin-dir "/bin"))
(cygwin-mount-activate)





(defun mark-line ()
 "when `transient-mark-mode', push mark and highlight line. "
 (interactive)
 (if (not (and transient-mark-mode))
      (error "transient mark not available"))

 (push-mark (line-beginning-position) t t)
 (forward-line 1)
 ;;(goto-char (line-end-position)))
 (goto-char (line-beginning-position)))
 
(global-set-key "\C-cl" 'mark-line)
 



;;http://www.emacswiki.org/cgi-bin/wiki/PostScriptToPDF
;; (autoload 'ps2pdf "ps2pdf" "ps2pdf." t)
;; (setq-default 
;;  ps2pdf-gs-program
;;  (concat devtools-dir "/gs/gs8.53/bin/gswin32c.exe"))




;;
;; load my notes file and my todo file
;; put it last and put point at end of file.
;;
(defvar files-to-load (list (concat "~/.notes." system-name  "/notes")) (concat "~/.notes." system-name "/todo"))

(dolist (f files-to-load)
  (when (file-exists-p f)
    (find-file-noselect f)
    ;;i don't like that notes.txt is always the most recently opened file.
    (setq filename-history (delete f file-name-history))))


;; (when (file-exists-p "~/notes/notes.txt")
;;   (find-file-noselect "~/notes/notes.txt")
;;   ;;i don't like that notes.txt is always the most recently opened file.
;;   (setq filename-history (delete "~/notes/notes.txt" file-name-history)))
;;   ;;(goto-char (point-max))
;;   ;;(bury-buffer (get-buffer "notes.txt"))
;;   ;;(switch-to-buffer "*scratch*"))


;; enable the use of emacsclient on linux
(when running-gnu-emacs-on-linux
  (message "starting emacs server")
  (server-start))



;; use gnuclient on windows 
(when (string= "windows-nt" system-type)
  ;;instead of gnuserv use the emacsclient, will likely work as EDITOR
  ;; environment variable, which would solve "edit" in sqlplus and edit
  ;; commit messages from svn/cvs emacsclient -a runemacs
  ;; c:\\home\\notes.txt requires setting EMACS_SERVER_FILE
  ;;(require 'server)
  ;;(require 'gnuserv)
  ;;(setenv "GNUSERV_SHOW_EMACS" "1"); force gnuserv to show emacs window, always
  ;;(gnuserv-start)
  (message "starting emacs server")
  (server-start)
  )


;;
;; gnuserv
;;
;; (setq exec-path (cons (expand-file-name "~/emacs/gnuserv") exec-path))
;; (setq gnuserv-frame (selected-frame))
;; ;; (setq gnuserv-frame (next-frame))
;; (setenv "GNUSERV_SHOW_EMACS" "1"); force gnuserv to show emacs window, always
;; (require 'gnuserv)
;; (gnuserv-start)



;; (defconst animate-n-steps 30) 
;; (defun emacs-reloaded ()
;;   (animate-string (concat ";; " (subst-char-in-string ?\n ?\s (emacs-version)) ".\n")
;; 		  0 0))

;; (add-hook 'after-init-hook 'emacs-reloaded)  


(autoload 'make-code-statement "code-statement" "" t)
(autoload 'unmake-code-statement "code-statement" "" t)

(add-to-list 'vc-handled-backends 'SVN)
(add-to-list 'vc-handled-backends 'CVS)

(setenv "PATH" (concat "E:\\devtools\\MiKTeX 2.5\\miktex\\bin;E:\\devtools\\gs\\gs8.53\\bin" ";" (getenv "PATH")))

;;(defun join-following-line nil
;;  (interactive)
;;  (join-line 1))

(defun zap-upto-char (arg char)
  "like `zap-to-char', but doesn't zap char"
  (interactive "p\ncZap up to char: ")
  (zap-to-char arg char)
  (insert char)
  (backward-char))


(defun test-prefix-arg (arg)
  ""
  (interactive "P")
  (message (format "%s" (prefix-numeric-value arg))))


(defun normalize-html () 
  ""
  (interactive)
  (if (not (and mark-active))
      (error "region not selected"))

  (setq begin (copy-marker (region-beginning)))  
  (setq limit (copy-marker (region-end)))  
  (goto-char (region-beginning))
  
  (while (re-search-forward "td" limit t)
    (replace-match (downcase (match-string))))


)

;;(load-library "hideshow")
;;(add-hook 'java-mode-hook 'hs-minor-mode)
;;(add-hook 'perl-mode-hook 'hs-minor-mode)
;;(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)



;; ;; save a list of open files in ~/.emacs.desktop
;; ;; save the desktop file automatically if it already exists
(setq desktop-save 'if-exists)
;;(desktop-save-mode 1)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                ;;(file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                (svn-diff-with-list       . 10)
                ;;tags-file-name
                register-alist)))


;;recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
;;(global-set-key [(meta f12)] 'recentf-open-files)

;;some oddness with ido, turn it on then off to make sure it doesn't
;;replace default key bindings
(ido-mode 1)
(ido-mode -1)
(setq ido-enable-regexp t)
;;(ido-mode 'buffer)
(setq ido-enable-flex-matching t)


;;M-F12 opens a buffer that contains the recent opened buffers
;; Use C-s to search for a filename and hit RET to open the file. Bind M-F11 to a function that uses ido on the recently opened files

(defun dmb-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))
(global-set-key [(meta f11)] 'dmb-ido-choose-from-recentf)



(defun delete-whitespace ()
  "Delete characters from point up to next non-whitespace char"
  (interactive)
  (let ((here (point)))
    (skip-syntax-forward "-")
    (if (/= (point) here)
	(delete-region (point) here))))

(defadvice just-one-space (around delete-whitespace (&optional n) activate compile)
  "redefined the behavior of just-one-space, allow for universal
argument to delete only unneeded whitespace according to major
mode.."
  (interactive "*P")
  (if current-prefix-arg       (delete-whitespace)
    ad-do-it))



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



;;; find file at point
(require 'ffap)
;; rebind C-x C-f and others to the ffap bindings (see variable ffap-bindings)
(ffap-bindings)
;; C-u C-x C-f finds the file at point
(setq ffap-require-prefix t)
;; browse urls at point via w3m
(setq ffap-url-fetcher 'w3m-browse-url)


(setq completion-show-help  nil)
(setq comint-completion-fignore
      (list "~" "CVS"))


(mouse-avoidance-mode 'proteus)  ;; same + change the pointer shape


;;http://www.emacswiki.org/cgi-bin/wiki/IswitchBuffers
(defun iswitchb-local-keys ()
  (mapc (lambda (K) 
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun))) 
        '(("<right>" . iswitchb-next-match)
          ("<left>"  . iswitchb-prev-match)
          ("<up>"    . ignore             )
          ("<down>"  . ignore             )))) 

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

(custom-set-variables
 '(iswitchb-use-frame-buffer-list t)
 '(iswitchb-max-to-show 8)
 '(iswitchb-buffer-ignore '("^ " "^\\*Minibuf-1\\*$" "^\\*Ibuffer\\*$" "^\\*anything\\*$" "^TAGS$" "^\\*Calc Trail\\*$" "^\\*jde-log\\*$" "^\\*Dired log\\*$" "^\\*Completions\\*$" "^\\*WoMan-Log\\*$" )))

(require 'uniquify)
(setq uniquify-buffer-file-name-style 'post-forward)



;;; (defadvice iswitchb-kill-buffer (after rescan-after-kill activate)
;;;   "*Regenerate the list of matching buffer names after a kill.
;;;     Necessary if using `uniquify' with `uniquify-after-kill-buffer-p' 
;;;     set to non-nil."
;;;   (setq iswitchb-buflist iswitchb-matches)
;;;   (iswitchb-rescan))
;;(ad-disable-advice 'iswitchb-kill-buffer 'after 'rescan-after-kill)

(defun iswitchb-rescan ()
  "*Regenerate the list of matching buffer names."
  (interactive)
  (iswitchb-make-buflist iswitchb-default) 
  (setq iswitchb-rescan t)) 







(autoload 'mass-replace-marked-files "mass-replace" "" t)
(autoload 'mass-replace-pattern-in-files "mass-replace" "" t)


(require 'mouse)

(custom-set-variables
 '(display-time-24hr-format t))
(display-time-mode 1)



;;vc-svn-find-version
(defvar svn-diff-with-list nil
  "recently used paths/revisions.")

(defun svn-diff-with (path)
  "compare working copy against another svn url"
  (interactive (list (ido-completing-read "svn url: " svn-diff-with-list nil nil buffer-file-name '(svn-diff-with-list . 0) nil )))
  (vc-ensure-vc-buffer)
  (let ((buf (get-buffer-create path)))
    (vc-do-command buf 0 vc-svn-program-name nil "cat" path)
    (let ((xyz-major-mode major-mode))
      (with-current-buffer buf
        (funcall xyz-major-mode)))
    (ediff-buffers (current-buffer) buf)))


(defun svn-diff-with2 (path)
  "compare working copy against another svn url"
  (interactive (list (completing-read "svn url: " svn-diff-with-list nil nil buffer-file-name '(svn-diff-with-list . 0) nil )))
  (let ((buf (get-buffer-create path)))
    (vc-do-command buf 0 vc-svn-program-name nil "cat" path)
    (let ((xyz-major-mode major-mode))
      (with-current-buffer buf
        (funcall xyz-major-mode)))
    (ediff-buffers (current-buffer) buf)))
(global-set-key [C-f9] 'svn-diff-with)


;;(setq inferior-lisp-program "/usr/bin/clisp")
(setq inferior-lisp-program "f:/devtools/clisp-2.43/clisp.exe"
      lisp-ident-function 'common-lisp-indent-function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-startup-animation nil)

(add-to-list 'load-path "~/emacs/packages/slime-2.0")
(require 'slime)
(slime-setup)

(add-hook 'lisp-mode-hook (lambda() (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda() (inferior-slime-mode t)))



(defun new-notes-day() 
  "write a new notes entry, using ^L and a date"
  (interactive)
  (let ((buf (get-buffer "notes")))
    (with-current-buffer buf
      (goto-char (point-max))
      ;;(princ "\n\n" buf)
      (insert "\n*" (format-time-string "%A %Y-%m-%d %T%z") "\n"))))
(defalias 'nnd 'new-notes-day)


(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
 (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))


;;(add-to-list 'load-path "~/emacs/packages/jabber-0.7.1")
;;(require 'jabber)


(add-to-list 'load-path "~/emacs/packages/emacs-jabber-0.7.1")
(require 'jabber)


(setq
 jabber-username "david.boon"
 jabber-password nil
 jabber-connection-type 'ssl
 jabber-server "gmail.com"
 jabber-network-server "talk.google.com"
 jabber-port "5223"
)




;;
;; http://www.emacswiki.org/cgi-bin/wiki/ProtectingBuffers
;;
(require 'protbuf)
(protect-buffer-from-kill-mode nil (get-buffer "*scratch*"))
(protect-buffer-from-kill-mode nil (get-buffer "notes"))
(protect-buffer-from-kill-mode nil (get-buffer "*Messages*"))
(setq-default 
 protect-buffer-bury-p  t) 


(setq tags-table-list nil)

(require 'highlight-current-line)
(add-to-list 'dmb-highlight-current-line-modes "HTML")
(add-to-list 'dmb-highlight-current-line-modes "Outline")
(add-to-list 'dmb-highlight-current-line-modes "XML")

;;(toggle-highlight-current-line)



;; ;;;
;; ;; Move to beginning of word before yanking word in isearch-mode.
;; ;; Make C-s C-w and C-r C-w act like Vim's * and #, keeping Emacs'
;; ;; C-s C-w [C-w] [C-w]... behaviour.
    
;; (require 'thingatpt)
;; (defadvice isearch-yank-word-or-char (before isearch-backward-word-before)
;;   "Move to beginning of word before yanking word in isearch-mode."
;;   (beginning-of-thing 'word)
;;   ;; Disable advice for subsequent calls
;;   (ad-disable-advice 'isearch-yank-word-or-char
;;                      'before 'isearch-backward-word-before)
;;   (ad-activate 'isearch-yank-word-or-char))
    
;; (add-hook 'isearch-mode-hook
;;           (lambda ()
;;             (ad-enable-advice 'isearch-yank-word-or-char
;;                               'before 'isearch-backward-word-before)
;;             (ad-activate 'isearch-yank-word-or-char)))


(require 'etags)

(defun isearch-yank-regexp (regexp)
  "Pull REGEXP into search regexp." 
  (let ((isearch-regexp nil)) ;; Dynamic binding of global.
    (isearch-yank-string regexp))
  (if (not isearch-regexp)
      (isearch-toggle-regexp))
  (isearch-search-and-update))

(defun isearch-yank-symbol ()
  "Put symbol at current point into search string."
  (interactive)
  (let ((sym (find-tag-default)))
    (if (null sym)
        (message "No symbol at point")
      (isearch-yank-regexp
       (concat "\\_<" (regexp-quote sym) "\\_>")))))

;;(define-key isearch-mode-map "\C-\M-w" 'isearch-yank-symbol)
(define-key isearch-mode-map "\C-w" 'isearch-yank-symbol)



;; Display a coloured square indicating the vc status of the current file
(defun vc-icon ()
  ;;(message "vc-icon")
  (let ((icon (if (vc-workfile-unchanged-p (buffer-file-name))
                  "c:/Users/dboon/emacs/in_vc.png"
                "c:/Users/dboon/emacs/modified_vc.png"))
        (bg-colour (face-attribute 'mode-line :background)))
    (propertize
     ""
     'display (find-image `((:type png
                             :file ,icon
                             :ascent center
                             :background ,bg-colour))))))


;; (defun vc-icon2 ()
;;   (let ((icon (cond 
;;                ((vc-workfile-unchanged-p (buffer-file-name))
;;                 "c:/Users/dboon/emacs/in_vc.png")
;;                ( "c:/Users/dboon/emacs/modified_vc.png"))))
;;         (bg-colour (face-attribute 'mode-line :background)))
;;     (propertize
;;      " x "
;;      'display (find-image `((:type png
;;                              :file ,icon
;;                              :ascent center
;;                              :background ,bg-colour))))))



(define-key occur-mode-map "n" 'next-error-no-select)
(define-key occur-mode-map "p" 'previous-error-no-select)

(define-key grep-mode-map "n" 'next-error-no-select)
(define-key grep-mode-map "p" 'previous-error-no-select) 

;;next-error-follow-minor-mode

;;(defun turn-on-next-error-follow-minor-mode ()
;;  ""
;;  (next-error-follow-minor-mode 1) )
;;(add-hook 'occur-mode-hook 'turn-on-next-error-follow-minor-mode)



(load "~/emacs/packages/nxml/autostart.el")
(add-to-list 'magic-mode-alist '("<\\?xml \\(?:.\\|<br/> \\)+?>\\(?:\\|<br/> \\)<!DOCTYPE html " . nxhtml-mode))
(defun my-nxml-mode-hook ()
  (make-local-variable 'tab-width)
  (setq tab-width 2))
(add-hook 'nxml-mode-hook 'my-nxml-mode-hook)

;; (require 'mmm-mode)
;;     ;; set up an mmm group for fancy html editing
;;     (mmm-add-group
;;      'fancy-html
;;      '(
;;        (html-php-tagged
;;         :submode php-mode
;;         :face mmm-code-submode-face
;;         :front "<[?]php"
;;         :back "[?]>")
;;        (html-css-attribute
;;         :submode css-mode
;;         :face mmm-declaration-submode-face
;;         :front "styleNO=\""
;;         :back "\"")
;;        (jsp-code
;;         :submode java
;;         :match-face (("<%!" . mmm-declaration-submode-face)
;;     		 ("<%=" . mmm-output-submode-face)
;;     		 ("<%"  . mmm-code-submode-face))
;;         :front "<%[!=]?"
;;         :back "%>"
;;         :insert ((?% jsp-code nil @ "<%" @ " " _ " " @ "%>" @)
;;     	     (?! jsp-declaration nil @ "<%!" @ " " _ " " @ "%>" @)
;;     	     (?= jsp-expression nil @ "<%=" @ " " _ " " @ "%>" @))
;;         )
;;        (jsp-directive
;;         :submode text-mode
;;         :face mmm-special-submode-face
;;         :front "<%@"
;;         :back "%>"
;;         :insert ((?@ jsp-directive nil @ "<%@" @ " " _ " " @ "%>" @))
;;         )
;;        ))


;; `C-c <left>' / `C-c <right>' undo/redo window config changes
;;(winner-mode)


;;(add-hook 'c-mode-hook 'imenu-add-menubar-index)
(require 'dired-sort-menu)


(require 'mic-paren)
(paren-activate)
(setf paren-priority 'both
      paren-highlight-at-point nil)
      

(add-hook 'c-mode-common-hook
          (function (lambda ()
                      (paren-toggle-open-paren-context 1))))


(defun lms1 () 
  "start an shell/comint process name it *lms1*"
  (interactive)
  (setf tmp-buffer (get-buffer-create "*lms1*"))
  (shell tmp-buffer))

(defun lms2 () 
  "start an shell/comint process name it *lms2*"
  (interactive)
  (setf tmp-buffer (get-buffer-create "*lms2*"))
  (shell tmp-buffer))

(add-to-list 'same-window-regexps "^\\*lms[0-9]\\*")


(add-to-list 'same-window-regexps "^\\*WoMan.*\\*")
;;(add-to-list 'same-window-regexps "\\*WoMan.*\\*\(<[0-9]>\)+\\")
(setq woman-manpath (list (concat cygwin-dir "/usr/man") 
                          (concat cygwin-dir "/usr/share/man")
                          (concat cygwin-dir "/usr/mann"))
      woman-path (list (concat cygwin-dir "/usr/share/man/man1")))

(autoload 'woman "woman" "Browse man pages." t)

;;(require 'icomplete)
;;(require 'icomplete+)

; requires ansi-color.el


;; (setq-default
;;  ansi-color-names-vector 
;;  (vector "black" "sienna" "sea green" "yellow" "blue" "magenta" "cyan" "white"))

;;; (custom-set-variables
;;;  ;;'(ansi-color-names-vector (vector "black" "sienna" "DeepSkyBlue4" "yellow" "NavyBlue" "magenta" "cyan" "white"))
;;;  ;;'(ansi-color-names-vector (vector "black" "sienna" "black" "yellow" "MidnightBlue" "magenta" "cyan" "white"))
;;;  '(ansi-color-names-vector (vector "black" "sienna" "black" "yellow" "MidnightBlue" "DarkRed" "DarkSlateBlue" "white"))
;;;  )


(require 'ansi-color)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;not supported for emacs 23, try the development version
;;; (add-to-list 'load-path "~/emacs/packages/emacs-w3m-1.4.4")
;;; ;;(require 'w3m-load)
;;; (require 'w3m)
;;; ;;(require 'mime-w3m)

;;;  (setq browse-url-browser-function 'w3m-browse-url)
;;;  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

;;;     (setq w3m-coding-system 'utf-8
;;;           w3m-file-coding-system 'utf-8
;;;           w3m-file-name-coding-system 'utf-8
;;;           w3m-input-coding-system 'utf-8
;;;           w3m-output-coding-system 'utf-8
;;;           w3m-terminal-coding-system 'utf-8)
;;; ;; (standard-display-ascii ?\225 [?+])

;;(setq ffap-alist (cons '(".*\\.java$" . ffap-c-mode) ffap-alist ))


(add-hook 'conf-javaprop-mode-hook 
          '(lambda () (conf-quote-normal nil)))

;;(8 16 24 32 40 48 56 64 72 80 88 96 104 112 120)
(custom-set-variables
 '(tab-stop-list '(4 8 12 16 20 24 28 32  40 48 56 64 72 80 88 96 104 112 120)))


(defun dmb-set-font-deja-bold() 
  ""
  (interactive)
  (set-default-font "-outline-dejaVu Sans mono-bold-r-normal-normal-10-*-96-96-c-*-iso8859-1"))


(defun dmb-set-font-deja-normal() 
  ""
  (interactive)
  (set-default-font "-outline-dejaVu Sans mono-normal-r-normal-normal-10-*-96-96-c-*-iso8859-1"))



(defun dmb-set-font-courier-normal() 
  ""
  (interactive)
  (set-default-font "-outline-Courier New-normal-r-normal-normal-11-82-96-96-c-*-iso8859-1"))




(setq 
 newsticker-url-list '(("/dev/websphere"  "http://feeds.feedburner.com/dev/websphere")
                       ("Planet JDK" "http://planetjdk.org/feed.rss")
                       ("Discover physics and math" "http://discovermagazine.com/topics/physics-math/rss.xml")))

(autoload 'newsticker-start "newsticker" "Emacs Newsticker" t)
(autoload 'newsticker-show-news "newsticker" "Emacs Newsticker" t)



(defun dmb-fix-multi-line-log(begin end) 
  ""
  (interactive "r")
  (while (re-search-forward "log\.info(.*?\"" end t)
   (replace-match "n" end nil)))


(defun debug-tomcat-1 ()
""
(interactive )
(cd "f:/devtools/Tomcat5.0/logs")
(jdb "jdb -Xmx1200m -XX:+PrintHeapAtGC  -verbose:gc -Djava.endorsed.dirs=f:\\devtools\\Tomcat5.0\\common\\endorsed -sourcepathf:\\wca_dmb\\WCA_Server_Indexer\\src -classpathc:\\devtools\\jdk1.5.0_14\\lib\\tools.jar;f:\\devtools\\Tomcat5.0\\bin\\bootstrap.jar;f:\\devtools\\Tomcat5.0\\bin\\commons-logging-api.jar;f:\\wca_dmb\\WCA_Server_Indexer\\src;f:\\wca_dmb\\WCA_Server_Servlet\\src;f:\\wca_dmb\\WCA_Server_Indexer\\etc -Dcatalina.base=f:\\devtools\\Tomcat5.0 -Dcatalina.home=f:\\devtools\\Tomcat5.0 -Djava.io.tmpdir=f:\\devtools\\Tomcat5.0\\temp org.apache.catalina.startup.Bootstrap start"
))

(defun debug-tomcat-2 ()
""
(interactive )
(cd "f:/devtools/Tomcat5.0/logs")
(jdb "jdb -Xmx1200m -XX:+PrintHeapAtGC  -verbose:gc -Xloggc:./gc.log -Djava.endorsed.dirs=f:\\devtools\\Tomcat5.0\\common\\endorsed -sourcepathf:\\wca\\WCA_Server_Indexer\\src -classpathc:\\devtools\\jdk1.5.0_14\\lib\\tools.jar;f:\\devtools\\Tomcat5.0\\bin\\bootstrap.jar;f:\\devtools\\Tomcat5.0\\bin\\commons-logging-api.jar;f:\\wca\\WCA_Server_Indexer\\src;f:\\wca\\WCA_Server_Servlet\\src;f:\\wca\\WCA_Server_Indexer\\etc -Dcatalina.base=f:\\devtools\\Tomcat5.0 -Dcatalina.home=f:\\devtools\\Tomcat5.0 -Djava.io.tmpdir=f:\\devtools\\Tomcat5.0\\temp org.apache.catalina.startup.Bootstrap start"
))

(require 'highlight-current-line)
(require 'highline)
(setq highline-face 'dmb-current-line-face)

;;(global-highline-mode 1)


(require 'color-moccur)

(autoload 'javascript-mode "javascript" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))

(defun rhino ()
  ""
  (interactive)
  (make-comint-in-buffer "rhino" nil "java" nil "-classpath" "F:/devtools/rhino1_6R7/js.jar" "org.mozilla.javascript.tools.shell.Main")
  (switch-to-buffer "*rhino*"))


(defun explorer-from-here ()
  "start an explorer session from the current buffers working directory."
 (interactive)
 (setq xyz (directory-file-name default-directory))
 (setq xyz (replace-regexp-in-string "\/" "\\\\" (directory-file-name xyz)))
 (shell-command (concat "explorer /n,/root, \"" xyz "\"")))


;; Require the code
(require 'control-lock)
;; Make C-z turn on control lock
;;(control-lock-keys)
;;(global-set-key "\C-z\C-z" 'control-lock-enable)

(setq skeleton-pair t)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)


(load-library "dired-x")


(require 'whitespace)
 (setq whitespace-display-mappings
       '((?\    [?\xB7]     [?.])		; space
         (?\xA0 [?\xA4]     [?_])		; hard space
         (?\n   [?\xB6 ?\n] [?$ ?\n])	; end-of-line
            ))
(autoload 'whitespace-mode           "whitespace" "Toggle whitespace visualization."        t)
(autoload 'whitespace-toggle-options "whitespace" "Toggle local `whitespace-mode' options." t)


;;Close the compilation window if there was no error at all.
(setq compilation-exit-message-function
      (lambda (status code msg)
        ;; If M-x compile exists with a 0
        (when (and (eq status 'exit) (zerop code))
          ;;then bury the *compilation* buffer, so that C-x b doesn't go there
  	  (bury-buffer "*compilation*") 
 	  ;; and return to whatever were looking at before 
 	  (replace-buffer-in-windows "*compilation*"))
          ;;Always return the anticipated result of compilation-exit-message-function
  	(cons msg code)))


(defun line-to-top-of-window ()
  "Move the line point is on to top of window."
  (interactive) 
  (recenter 0))
(global-set-key [f6] 'line-to-top-of-window)


(defun m-shell-command ()
  "Launch a shell command."
  (interactive)
  (let ((command (read-string "Command: ")))
    (shell-command (concat command " &") (concat "*" command "*"))))

(defun autocompile nil
  "compile itself if ~/.emacs"
  (interactive)
  (require 'bytecomp)
  (if (string= (buffer-file-name) (expand-file-name (concat default-directory ".emacs")))
      (byte-compile-file (buffer-file-name))))

(add-hook 'after-save-hook 'autocompile)

(require 'ps2pdf)
(setq ps2pdf-gs-program "f:/devtools/gs/gs8.53/bin/gswin32c.exe")


  (require 'speedbar)

  (defconst my-speedbar-buffer-name "SPEEDBAR")
  ; (defconst my-speedbar-buffer-name " SPEEDBAR") ; try this if you get "Wrong type argument: stringp, nil"

  (defun my-speedbar-no-separate-frame ()
    (interactive)
    (when (not (buffer-live-p speedbar-buffer))
      (setq speedbar-buffer (get-buffer-create my-speedbar-buffer-name)
            speedbar-frame (selected-frame)
            dframe-attached-frame (selected-frame)
            speedbar-select-frame-method 'attached
            speedbar-verbosity-level 0
            speedbar-last-selected-file nil)
      (set-buffer speedbar-buffer)
      (speedbar-mode)
      (speedbar-reconfigure-keymaps)
      (speedbar-update-contents)
      (speedbar-set-timer 1)
      (make-local-hook 'kill-buffer-hook)
      (add-hook 'kill-buffer-hook
                (lambda () (when (eq (current-buffer) speedbar-buffer)
                             (setq speedbar-frame nil
                                   dframe-attached-frame nil
                                   speedbar-buffer nil)
                             (speedbar-set-timer nil)))))
    (set-window-buffer (selected-window) 
                       (get-buffer my-speedbar-buffer-name)))



(autoload 'remember "remember" nil t)
(autoload 'remember-region "remember" nil t)

;;(define-key global-map (kbd "<f9> r") 'remember)
;;(define-key global-map (kbd "<f9> R") 'remember-region)



;;(add-to-list 'load-path "~/emacs/packages/pmwiki-mode")
;;(require 'pmwiki-mode)


(require 'smooth-scrolling)
(custom-set-variables
 '(smooth-scroll-margin 7))


(custom-set-variables
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 5) ((control))))))


(add-to-list 'load-path "~/emacs/packages/weblogger")
(require 'weblogger)


(require 'replace+)

;;http://www.emacswiki.org/cgi-bin/wiki/RubikitchIciclesConfiguration
(defalias 'icicle-other-window-or-frame 'other-window)


;;  (defun occur-mode-clean-buffer ()
;;    "Removes all commentary from the *Occur* buffer, leaving the
;;  unadorned lines."
;;    (interactive)
;;    (if (get-buffer "*Occur*")
;;        (save-excursion
;;          (set-buffer (get-buffer "*Occur*"))
;;          (goto-char (point-min))
;;          (toggle-read-only 0)
;;          (if (looking-at "^[0-9]+ lines matching \"")
;;              (kill-line 1))
;;          (while (re-search-forward "^[ \t]*[0-9]+:"
;;                                    (point-max)
;;                                    t)
;;            (replace-match "")
;;            (forward-line 1)))
;;      (message "There is no buffer named \"*Occur*\".")))


(message "dmb-main.el done")



