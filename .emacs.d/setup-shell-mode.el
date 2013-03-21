

;;(require 'ansi-color)

;;(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-off)

(setq dirtrack-list '("|Pr0mPT|\\([^|]*\\)|" 1 nil))

;; (if (not is-darwin)
;;     (progn
;;       (load-library "dmb-shell.el")
;;       (load-library "dmb-cygwin.el")))

;; (when is-darwin
;;     (load-library "dmb-shell-apple.el"))

;;;###autoload
(defun truncate-shell-contents() 
  "remove the entire shell contents"
  (interactive)
  (set (make-local-variable 'comint-buffer-maximum-size) 0)  
  (comint-truncate-buffer))


(setq auto-mode-alist
      (cons '("\\.zsh-theme$" . shell-script-mode) auto-mode-alist))

(provide 'setup-shell-mode)
