;;; `pmwiki-mode.el' regression suite


(defcustom pmwiki-tests-page-noauth
  ;;"http://wiki.lyx.org/Playground/TestPmwiki-mode"
  "http://www.ii.uni.wroc.pl/~lukstafi/pmwiki/index.php?n=PmWikiMode.TestNoPassw"
  "URL of page with no authentication on which to perform tests."
  :group 'pmwiki-tests :type 'string)

(defcustom pmwiki-tests-page-passwd
  "http://www.ii.uni.wroc.pl/~lukstafi/pmwiki/index.php?n=PmWikiMode.TestPassw"
  "URL of page with password on which to perform tests."
  :group 'pmwiki-tests :type 'string)

(defvar pmwiki-tests-cur-buf nil
  "Buffer on which test operates.")

(defvar pmwiki-tests-buf (get-buffer-create "pmwiki-mode-tests")
  "Buffer with test results.")

(defun pmwiki-tests ()
  "pmwiki-mode regression suite"
  (interactive)
  (pmwiki-tests-osr pmwiki-tests-page-noauth)
  (let ((pmwiki-session-passwords
	 '(((edit
	     "http://www.ii.uni.wroc.pl/~lukstafi/pmwiki/index.php"
	     "PmWikiMode" "TestPassw") "secret")))
	(pmwiki-always-edit t))
    (pmwiki-tests-osr pmwiki-tests-page-passwd))
  )

(defun pmwiki-tests-start (str)
  "Report start of new test."
  (setq debug-on-error t)
  (setq pmwiki-tests-buf (get-buffer-create "pmwiki-mode-tests"))
  (with-current-buffer pmwiki-tests-buf
    (goto-char (point-max))
    (insert "\n\n")
    (insert str)))

(defun pmwiki-tests-progress (str)
  "Report start of new test."
  (with-current-buffer pmwiki-tests-buf
    (goto-char (point-max))
    (insert "...ok\n  ")
    (insert str)))

(defun pmwiki-tests-finish ()
  "Report start of new test."
  (with-current-buffer pmwiki-tests-buf
    (goto-char (point-max))
    (insert "...ok\n  ")
    (insert "done\n"))
  (switch-to-buffer pmwiki-tests-buf))

(defun pmwiki-tests-osr (page)
  "open - save - reload"
  (interactive "sGive page to test: ")
  (setq follow-intercept-processes nil)
  (pmwiki-tests-start (concat "open - save - reload page: " page))
  (let (running count text pos)
    (pmwiki-tests-progress
     (concat
      "should load " page " into a buffer"))
    (setq running
	  (pmwiki-open page))
    (while (symbol-value (car running)) (sleep-for 0 100))
    (sleep-for 0 100)
    (setq pmwiki-tests-cur-buf (cdr running))
    (pmwiki-tests-progress
     (concat
     "checking results of opening; remember to keep \
\"Autoregression test NUM.\" in " page))
    (switch-to-buffer pmwiki-tests-cur-buf t)
    (goto-char (point-min))
    ;;(insert "Been here -- Tony Halik\n")
    (or
     (re-search-forward "Autoregression test \\([0-9]+\\)\\." (point-max) t)
     (error
      (concat
       "Could not match \"Autoregression test \\([0-9]+\\)\\.\" in "
       (buffer-string))))
    (setq count (1+ (string-to-number
		     (or (match-string 1) (match-string 2)))))
    (pmwiki-tests-progress "should save with incremented test number")
    (goto-char (point-min))
    (replace-regexp
     "Autoregression test \\([0-9]+\\)\\."
     (concat "Autoregression test " (number-to-string count) "."))
    (setq text (buffer-string))
    (let ((pmwiki-no-summary t))
      (setq running (pmwiki-save)))
    (while (symbol-value running) (sleep-for 0 100))
    (sleep-for 0 100)
    (pmwiki-tests-progress
     (concat "should reload " page))
    (delete-region (point-min) (point-max))
    (insert "regression temporal contents to be replaced")
    (goto-char 7)
    (setq pos (point))
    (setq running (pmwiki-reload))
    (while (symbol-value (car running)) (sleep-for 0 100))
    (sleep-for 0 100)
    (pmwiki-tests-progress "should keep the point after reloading")
    (assert (equal pos (point)))
    (pmwiki-tests-progress	   
     "should remember the modified test number")
    (goto-char (point-min))
    (re-search-forward "Autoregression test \\([0-9]+\\)\\.")
    (assert (equal count (string-to-number (match-string 1))))
    (pmwiki-tests-progress "should rember exact buffer contents")
    (assert (string= text (buffer-string)))
    (kill-buffer (current-buffer))
    (pmwiki-tests-finish)))
