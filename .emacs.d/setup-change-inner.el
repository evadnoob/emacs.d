(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/change-inner.el"))

(global-set-key (kbd "C-c ci") 'change-inner)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)

(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/expand-region.el"))
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(provide 'setup-change-inner)
