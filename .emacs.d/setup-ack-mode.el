
;;
;; setup the awesome ack program
;;
(add-to-list 'load-path "~/Dropbox/.emacs.x/.emacs.p/full-ack")

(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)
(setq ack-executable "~/bin/ack")
(custom-set-variables
 '(ack-prompt-for-directory "Prompt"))

(provide 'setup-ack-mode)
