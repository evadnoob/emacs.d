(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/python-mode"))

(setq auto-mode-alist (cons '("\\.py" . python-mode) auto-mode-alist))

(custom-set-variables 
 '(setq python-indent-offset 2))

(provide 'setup-python-mode)
