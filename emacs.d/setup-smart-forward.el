(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/smart-forward.el"))

(require 'smart-forward)
(global-set-key (kbd "M-<up>") 'smart-up)
(global-set-key (kbd "M-<down>") 'smart-down)
(global-set-key (kbd "M-<left>") 'smart-backward)
(global-set-key (kbd "M-<right>") 'smart-forward)


(provide 'setup-smart-forward)
