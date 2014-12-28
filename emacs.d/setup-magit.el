
;;(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/magit"))

(require 'magit)



(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

;; Subtler highlight
;;(set-face-background 'magit-item-highlight "#121212")
(set-face-background 'magit-item-highlight "deep sky blue")
(set-face-foreground 'diff-context "#666666")
(set-face-foreground 'diff-added "#00cc33")
(set-face-foreground 'diff-removed "#ff0000")

(global-set-key (kbd "C-x g") 'magit-status)
;;(global-set-key "\C-x g" 'magit-status)
(provide 'setup-magit)
