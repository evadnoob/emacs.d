;;
;; http://www.emacswiki.org/cgi-bin/wiki/ProtectingBuffers
;;
(require 'protbuf)
(protect-buffer-from-kill-mode nil (get-buffer "*scratch*"))
(protect-buffer-from-kill-mode nil (get-buffer "notes"))
(protect-buffer-from-kill-mode nil (get-buffer "*Messages*"))
(setq-default 
 protect-buffer-bury-p  t) 


(provide 'setup-buffer-protection)
