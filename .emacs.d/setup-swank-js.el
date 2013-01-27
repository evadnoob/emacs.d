(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/swank-js"))
(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/slime")); your SLIME directory

(setq inferior-lisp-program "/usr/local/bin/sbcl") ; your Lisp system
(require 'slime)

(slime-setup)
;;brew install clisp


(add-hook 'js2-mode-hook
                  (lambda ()
                    (slime-js-minor-mode 1)))


(provide 'setup-swank-js)
