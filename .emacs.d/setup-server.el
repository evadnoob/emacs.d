;;
;; server start stuff in here
;;

;; enable the use of emacsclient on linux
(when (or running-gnu-emacs-on-linux is-darwin)
  (message "starting emacs server")
  (server-start))

(provide 'setup-server)
