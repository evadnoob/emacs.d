

(require 'ansi-color)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq dirtrack-list '("|Pr0mPT|\\([^|]*\\)|" 1 nil))

;; (if (not is-darwin)
;;     (progn
;;       (load-library "dmb-shell.el")
;;       (load-library "dmb-cygwin.el")))

;; (when is-darwin
;;     (load-library "dmb-shell-apple.el"))



(provide 'setup-shell-mode)
