(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/expand-region.el"))
(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/change-inner.el"))



(require 'expand-region)
(require 'change-inner)

(global-set-key (kbd "C-c ci") 'change-inner)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)

(global-set-key (kbd "C-=") 'er/expand-region)
;; Expand region (increases selected region by semantic units)
;;(global-set-key (kbd "C-@") 'er/expand-region)

(provide 'setup-change-inner)
