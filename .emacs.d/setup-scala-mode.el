;;
;; setup scala mode and ensime mode
;;
(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/scala-mode2/"))
(require 'scala-mode2)

(unless (package-installed-p 'scala-mode2)
  (package-refresh-contents) (package-install 'scala-mode2))

(setq auto-mode-alist (cons '("\\.scala" . scala-mode) auto-mode-alist))
;; set the first element of a list: (setcar auto-mode-alist '("\\.scala" . scala-mode))

;; load the ensime lisp code...
(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/ensime/dist/elisp/"))
(require 'ensime)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
;;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


;;(setq yas/my-directory (*emacs ".emacs.x/.emacs.p/scala-mode/contrib/yasnippet/snippets"))
;;(yas/load-directory yas/my-directory)

(provide 'setup-scala-mode)




