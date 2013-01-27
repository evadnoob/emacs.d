(setq gnus-select-method '(nnimap "gmail"
				  (nnimap-address "imap.gmail.com")
				  (nnimap-server-port 993)
				  (nnimap-stream ssl)))

;; (add-to-list 'gnus-secondary-select-methods '(nnimap "gmail"
;;                                   (nnimap-address "imap.gmail.com")
;;                                   (nnimap-server-port 993)
;;                                   (nnimap-stream ssl)))

;;with a line like the following one in your ~/.authinfo 
;;machine imap.gmail.com login username@gmail.com password secret port 993

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "david.boon@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "localhost.localdomain")

;; NOTE: You will need starttls, which is used to set up the SSL encrypted connection to the GMail server. This is contained in the “gnutls-bin” package on debian and ubuntu. If you don’t have this installed, you’ll get a (rather generic) “smtpmail-send-it: Sending failed; SMTP protocol error” error message.

(setq gnus-permanently-visible-groups "mail")
 	
(setq ispell-program-name "aspell")

 	
;;(require 'bbdb)
;;(bbdb-initialize 'gnus 'message)
