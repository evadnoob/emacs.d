

(if is-emacs-23
    (require 'grep))

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
(if is-emacs-23
    (set-face-underline-p 'compilation-line-number nil))

(add-hook 
 'grep-mode-hook 
 (lambda () 
   "hook to rename buffer to include find args"
   (if (string-match "\*grep\*" (buffer-name (current-buffer)))
       (progn
         (setq truncate-lines t)
         (rename-buffer (format "*grep* [%s]" command-args) 1)))))

(add-hook 
 'dired-mode-hook 
 (lambda () 
   "hook to rename buffer to include find args"
   (if (string-match "\*Find\*" (buffer-name (current-buffer)))
       (progn (rename-buffer (format "*find* [%s]" find-args) 1)))))


(defun simple-grep (dir options)
  "a slightly more improved grep-find"
  ;;(interactive "Dgrep (directory): \nsgrep (directory): %s (like this): \ns ")
  (interactive (list (read-directory-name "grep (directory): ")
                     (read-from-minibuffer "grep (args): " '("grep --include=\"*.*\" -nrH -e  ./*" . 30) nil nil 'grep-history)))
  (save-excursion
    (setq buffer (get-buffer-create "*grep*"))
    
    (with-current-buffer buffer
      (cd dir)
      (setenv "GREP_OPTIONS" "--exclude=*/TAGS --exclude=.#* --exclude=*\~ --exclude=*/CVS/* --exclude=*.svn-base")
      ;;fake out the grep-mode-hook by setting command-args to options
      (setq command-args options)
      ;;run grep like: grep -Hnr --include="*.java" "OfferBroker" c:/khub/ReqRecSearchFilter/java/
      ;;--exclude=\".#*\" --exclude=\"*\\~\"

      (compilation-start options 'grep-mode nil t)
      ;;(setq next-error-function 'dave-next-error)
)))



(provide 'setup-grep)


