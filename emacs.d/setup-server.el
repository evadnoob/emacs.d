;;
;; server start stuff in here
;;

(when (not (display-graphic-p))
  (when (or running-gnu-emacs-on-linux is-darwin)
    (message "starting emacs server")
    (server-start)))

(provide 'setup-server)
