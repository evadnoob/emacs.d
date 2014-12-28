(require 'anything)
(require 'anything-config)

(defvar anything-match-face 'bold)

;; (defun anything-insert-match (match insert-function)
;;   "Insert MATCH into the anything buffer. If MATCH is a list then
;; insert the string inteneded to appear on the display and store
;; the real value in a text property."
;;   (let ((beg (line-beginning-position))
;;         (end (line-end-position))
;;         (string (if (listp match) (car match) match)))
;;     (funcall insert-function string)

;;     (if (listp match)
;;         (put-text-property beg end
;;                            'anything-realvalue (cdr match)))
;;     (if (and anything-match-face
;;              (string-match anything-pattern string))
;;         (put-text-property (+ beg (match-beginning 0))
;;                            (+ end (match-end 0))
;;                            'face anything-match-face))

;;     (funcall insert-function "\n")))

;;(if anything-sql-plus-tnsnames 
;;    (setq anything-sql-plus-tnsnames nil))

;; (defvar anything-sql-plus-tnsnames
;;   `((name . "sqlplus")
;;     (candidates . ,(let ((newlist nil))
;;                      (dolist (item known-schemas-alist)
;;                        (setq newlist (cons (car item) newlist)))
;;                      newlist))
;;    (action . (("" . (lambda (selected)
;;                       (message (format "%s type %s" selected (type-of selected )))
;;                       (sql-oracle-any-schema selected)))))
;;  "names of sqlplus connection"))


(defvar anything-sql-plus-tnsnames)
(setq anything-sql-plus-tnsnames
  `((name . "sqlplus")
    (candidates . ,(let ((newlist nil))
                     (dolist (item known-schemas-alist)
                       (setq newlist (cons (car item) newlist)))
                     newlist))
   (action . (("" . (lambda (selected)
                      (message (format "%s type %s" selected (type-of selected )))
                      (sql-oracle-any-schema selected)))))
 "names of sqlplus connection"))


(defun anything-semantic-construct-candidates (tags depth)
  (apply 'append (mapcar '(lambda (tag)
                            (when (and (listp tag)
                                       (or (equal (semantic-tag-type tag) "class")
                                           (eq (semantic-tag-class tag) 'variable)
                                           (eq (semantic-tag-class tag) 'function)))
                              (cons (cons (concat (make-string (* depth 2) ?\s)
                                                  (semantic-format-tag-summarize tag nil t)) tag)
                                    (anything-semantic-construct-candidates (semantic-tag-components tag) (1+ depth)))))
                         tags)))


 (defvar anything-c-source-semantic
    '((name . "Semantic Tags")
      (init . (lambda ()
                (setq anything-semantic-candidates
                      (anything-semantic-construct-candidates (semantic-fetch-tags) 0))))
      (candidates . (lambda ()
                      (mapcar 'car anything-semantic-candidates)))
      (action . (("Goto tag" . (lambda (candidate)
                                 (let ((tag (cdr (assoc candidate anything-semantic-candidates))))
                                   (semantic-go-to-tag tag))))))))


;;(setq anything-c-locate-options `("locate" "-d" ,anything-c-locate-db-file "-i" "-r" ))
;;(setq anything-c-locate-options `("locate"  "-i" "-r" ))


(defvar anything-kill-ring-threshold 10)

(defvar anything-c-source-kill-ring
  '((name . "Kill Ring")
    (init . (lambda ()
              (setq anything-kill-ring-buffer (current-buffer))))
    (candidates . (lambda ()
                    (remove-if
                     (lambda (kill)
                       (or (< (length kill) anything-kill-ring-threshold)
                           (string-match "^[\\s\\t]+$" kill)))
                     kill-ring)))
    (action . (("Insert" . (lambda (candidate)
                             (with-current-buffer anything-kill-ring-buffer
                               (insert candidate))))))
    ;(requires-pattern . 3)
    (multiline)))

(defvar anything-c-source-occur
  '((name . "Occur")
    (init . (lambda ()
              (setq anything-occur-current-buffer
                    (current-buffer))))
    (candidates . (lambda ()
                    (let ((anything-occur-buffer (get-buffer-create "*Anything Occur*")))
                      (with-current-buffer anything-occur-buffer
                        (occur-mode)
                        (erase-buffer)
                        (let ((count (occur-engine anything-pattern
                                                   (list anything-occur-current-buffer) anything-occur-buffer
                                                   list-matching-lines-default-context-lines case-fold-search
                                                   list-matching-lines-buffer-name-face
                                                   nil list-matching-lines-face
                                                   (not (eq occur-excluded-properties t)))))
                          (when (> count 0)
                            (setq next-error-last-buffer anything-occur-buffer)
                            (cdr (split-string (buffer-string) "\n" t))))))))
    (action . (("Goto line" . (lambda (candidate)
                                (with-current-buffer "*Anything Occur*"
                                  (search-forward candidate))
                                (goto-line (string-to-number candidate) anything-occur-current-buffer)))))
    (requires-pattern . 3)
    (volatile)
    (delayed)))


(defvar anything-c-ctags-modes
  '( c-mode c++-mode awk-mode csharp-mode java-mode javascript-mode lua-mode
            makefile-mode pascal-mode perl-mode cperl-mode php-mode python-mode
            scheme-mode sh-mode slang-mode sql-mode tcl-mode jde-mode ))

(defun anything-c-source-ctags-init ()
  (when (and buffer-file-name
             (memq major-mode anything-c-ctags-modes)
             (anything-current-buffer-is-modified))
    (with-current-buffer (anything-candidate-buffer 'local)
      (call-process-shell-command
       (if (string-match "\\.el\\.gz$" anything-buffer-file-name)
           (format "ctags -e -u -f- --language-force=lisp --fields=n =(zcat %s) " anything-buffer-file-name)
         (format "ctags -e -u -f- --fields=n %s " anything-buffer-file-name))
       nil (current-buffer))
      (goto-char (point-min))
      (forward-line 2)
      (delete-region (point-min) (point))
      (loop while (and (not (eobp)) (search-forward "\001" (point-at-eol) t))
            for lineno-start = (point)
            for lineno = (buffer-substring lineno-start (1- (search-forward "," (point-at-eol) t)))
            do
            (beginning-of-line)
            (insert (format "%5s:" lineno))
            (search-forward "\177" (point-at-eol) t)
            (delete-region (1- (point)) (point-at-eol))
            (forward-line 1)))))

(defvar anything-c-source-ctags
  '((name . "Exuberant ctags")
    (init
     . anything-c-source-ctags-init)
    (candidates-in-buffer)
    (adjust)
    (type . line)))


(defun anything-at-point (arg)
  (interactive "P")
  (if arg
      (anything nil (concat "\\_<" (thing-at-point 'symbol) "\\_>"))
    (anything)))



(require 'anything-etags)
(setq anything-etags-tag-file-name "f:/3_0_integration2/TAGS")


;;anything-source-locate
;;anything-source-emacs-commands
(setq
 anything-sources 
 (list
  anything-c-source-semantic
  ;;anything-c-source-occur
  anything-c-source-buffers
  ;;anything-sql-plus-tnsnames
  anything-c-source-mac-spotlight
  anything-c-source-bookmarks
  anything-c-source-recentf
  ;;anything-source-emacs-commands
  anything-c-source-file-name-history
  ;;anything-c-source-imenu
  ;;anything-c-source-man-pages
  ;;anything-c-source-complex-command-history
  ;;anything-c-source-locate
  ;;anything-c-source-kill-ring
  anything-c-source-ctags
;;  anything-c-source-auto-install
 ;; anything-etags-c-source-etags-select
  
  )
 anything-c-locate-options `("locate"  "-i" "-r" )
 anything-idle-delay 0.02
 anything-samewindow t ;;nil
 )


;; (setq anything-type-actions (list anything-actions-buffer
;;                                   anything-actions-file))



(require 'anything)
(require 'anything-config)
(require 'anything-match-plugin)

(global-set-key "\C-x\C-a"   'anything)
(global-set-key "\M-a"       'anything)


(provide 'setup-anything)
