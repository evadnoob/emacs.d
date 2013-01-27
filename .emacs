;;
;; main entry point into my .emacs.d 
;;

(setq dmb-dotemacs-start-time (current-time))

(defconst running-xemacs (string-match "XEmacs\\|Lucid" (emacs-version)))
(defconst running-gnu-emacs (string-match "GNU Emacs" (emacs-version)))
(defconst running-gnu-emacs-on-linux (and running-gnu-emacs (string= "gnu/linux" system-type)))
(defconst running-emacs-on-cygwin (string-match "-cygwin" (emacs-version)))
(defconst is-emacs-23 (string-match "GNU Emacs 23\." (emacs-version)))
(defconst is-darwin (string-match "darwin" (emacs-version)))

(defun *emacs (path)
  (if is-darwin 
      (expand-file-name (concat "~/Dropbox/" path))
    (expand-file-name (concat "f:/Dropbox/" path))))

(add-to-list 'load-path (*emacs ".emacs.x/.emacs.d"))
(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p"))

;; Reminder to self: replacing dmb-main with more modular setup
;; files...(load-library "dmb-main.el") (load-library "dmb-var.el")
(load-library "setup-sensible-defaults")
(load-library "setup-appearance")
(load-library "setup-grep")
(load-library "setup-org-mode")
(load-library "setup-shell-mode")
(load-library "setup-erc")
(load-library "setup-nxml-mode")
(load-library "setup-js-mode")
(load-library "setup-lorem-ipsum")
(load-library "setup-keybindings")
(load-library "setup-helm")
(load-library "setup-text-manipulation")
(load-library "setup-isearch")
(load-library "setup-server")
(load-library "setup-ido")
(load-library "setup-ido")

;;brew install emacs --cocoa
;;brew install emacs --cocoa --use-git-head --HEAD
;; To open it with Alfred or Quicksilver, you have to copy Emacs.app into /Applications instead of the symlink that brew places there.

(setq dmb-dotemacs-end-time (current-time))
(message 
 (format "emacs done %s seconds." (time-to-seconds (time-subtract dmb-dotemacs-end-time dmb-dotemacs-start-time))))
