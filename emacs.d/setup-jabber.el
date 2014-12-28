
(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/emacs-jabber"))

(require 'jabber)


(setq
 jabber-username "david.boon"
 jabber-password nil
 jabber-connection-type 'ssl
 jabber-server "gmail.com"
 jabber-network-server "talk.google.com"
 jabber-port "5223"
)


(provide 'setup-jabber)
