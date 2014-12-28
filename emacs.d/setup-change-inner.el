(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/expand-region.el"))
(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/change-inner.el"))

(require 'expand-region)
(require 'change-inner)

(global-set-key (kbd "C-c ci") 'change-inner)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)

;; vim's ci and co commands
;;(global-set-key (kbd "M-I") 'change-inner)
;;(global-set-key (kbd "M-O") 'change-outer)

(global-set-key (kbd "s-i") 'copy-inner)
(global-set-key (kbd "s-o") 'copy-outer)

;; Expand region (increases selected region by semantic units)
;;(global-set-key (kbd "C-=") 'er/expand-region)
;;(global-set-key (kbd "C-=") 'er/expand-region)
;;(global-set-key "\C-=" 'er/expand-region)
(global-set-key [C-=]  'er/expand-region)
;;(global-set-key (kbd "C-SPC") 'set-mark-command)

(provide 'setup-change-inner)
