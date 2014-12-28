(defun finder-from-here (&optional arg)
  "start an explorer session from the current buffers working directory."
 (interactive "P")
 ;;(setq xyz (expand-file-name (directory-file-name default-directory)))
 ;;(setq xyz (replace-regexp-in-string "\/" "\\\\" (directory-file-name xyz)))
 ;;(w32-shell-execute "open" "explorer" (concat "/e,/select," xyz)))
 ;;(shell-command (concat "explorer /n,/root, \"" xyz "\"")))
 
;; (if current-prefix-arg 
;;     (w32-shell-execute "open" "explorer" (concat "/e,/select," xyz))
;;   (w32-shell-execute "open" "explorer" (concat "/n,/root," xyz))))
(shell-command "open ."))

(provide 'setup-finder-from-here)

