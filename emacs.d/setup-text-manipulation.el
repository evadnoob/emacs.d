

;; (defun custom-goto-match-beginning ()
;;   "Use with isearch hook to end search at first char of match."
;;   (when isearch-forward
;;     (unless (null isearch-other-end)
;;       (goto-char isearch-other-end))))

;; ;;
;; ;; Always end searches at the beginning of the matching expression.
;; ;;
;; (add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)

;;http://www.emacswiki.org/cgi-bin/wiki/SebastienRoccaSerraKeyBindings
(defun move-line (&optional n)
  "Move current line N (1) lines up/down leaving point in place."
  (interactive "p")
  (when (null n)
    (setq n 1))
  (let ((col (current-column)))
    (beginning-of-line)
    (forward-line)
    (transpose-lines n)
    (forward-line -1)
    (forward-char col))
  (indent-according-to-mode))

(defun move-line-up (n)
  "Moves current line N (1) lines up leaving point in place."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Moves current line N (1) lines down leaving point in place."
  (interactive "p")
  (move-line (if (null n) 1 n)))



(defadvice just-one-space (around delete-whitespace (&optional n) activate compile)
  "redefined the behavior of just-one-space, allow for universal
argument to delete only unneeded whitespace according to major
mode.."
  (interactive "*P")
  (if current-prefix-arg (delete-whitespace)
    ad-do-it))



(defadvice tidy-html-for-js (around tidy-buffer-xml (&optional n) activate compile)
  ""
  (interactive "*P")
  (unmake-code-statement)
  (if current-prefix-arg (tidy-buffer-xml)
    ad-do-it))


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
    "tidy -config ~/Dropbox/.emacs.x/.emacs.d/.tidyrc" t t)
  ;; (shell-command-on-region (point-min) (point-max)
  ;;   "tidy -xml -f /tmp/tidy-errs -q -i -wrap 72 -utf8 -omit" t t)
  (find-file-other-window "/tmp/tidy-errs")
  (other-window 1)
  (delete-file "/tmp/tidy-errs")
  (message "buffer tidy'ed")
 )


(defun make-mongo-object-id ()
  "shell to mongo shell and create an object id"
  (interactive)
  (insert (replace-regexp-in-string "ObjectId(\"\\(.*?\\)\")\n" "\\1" 
                            (shell-command-to-string "/usr/local/bin/mongo --nodb --norc --quiet --eval 'ObjectId()'"))))


(defun test-return-value ()
""
(interactive)
"x")




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



;;;###autoload
(defun zap-up-to-char (arg char)
  "like `zap-to-char', but doesn't zap char"
  (interactive "p\ncZap up to char: ")
  (zap-to-char arg char)
  (insert char)
  (backward-char))


;;
;; open line below
;; http://emacsredux.com/blog/2013/03/26/smarter-open-line/
;; 
(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key [(shift return)] 'smart-open-line)

;;
;; open line above
;; http://emacsredux.com/blog/2013/06/15/open-line-above/
;;
(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(control shift return)] 'smart-open-line-above)

(provide 'setup-text-manipulation)



