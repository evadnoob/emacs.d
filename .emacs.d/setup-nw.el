;; Enable mouse support
(unless (display-graphic-p)
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)

(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/pbcopy.el"))
(require 'pbcopy)

(turn-on-pbcopy)

(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)
(global-set-key "\C-w" 'clipboard-kill-region)

;; copying and pasting from the clipboard work under screen or normal terminal, however under tmux they do not!
;; 
;; https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard.git

)


(custom-set-variables
 '(save-interprogram-paste-before-kill nil))

(provide 'setup-nw)
