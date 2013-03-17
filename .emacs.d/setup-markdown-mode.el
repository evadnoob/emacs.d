;;git clone git://jblevins.org/git/markdown-mode.git
(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/markdown-mode"))

(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

(require 'markdown-mode)

