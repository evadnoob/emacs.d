(require 'cedet)
(semantic-load-enable-minimum-features) ;; or enable more if you wish
(require 'malabar-mode)
(setq malabar-groovy-lib-dir "~/.emacs.x/.emacs.p/malabar-mode/lib")
(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
