;;
;; helm replaces anything mode: https://github.com/emacs-helm/helm
;;
(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/helm"))
(require 'helm-config)


(require 'helm-files)
;;(setq helm-idle-delay 0.1)
;;(setq helm-input-idle-delay 0.1)
(custom-set-variables 
 '(helm-c-locate-command "locate-with-mdfind %.0s %s")
 '(helm-full-frame t))

;;(loop for ext in '("\\.swf$" "\\.elc$" "\\.pyc$")
;;      do (add-to-list 'helm-c-boring-file-regexp-list ext))

(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "M-a") 'helm-mini)

;;(helm-mode 1)
(provide 'setup-helm)
