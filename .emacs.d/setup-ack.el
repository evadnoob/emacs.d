(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/ack-and-a-half"))
(require 'ack-and-a-half)
;; Create shorter aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)


(custom-set-variables 
 '(ack-and-a-half-executable "/usr/local/bin/ack"))

(provide 'setup-ack)
