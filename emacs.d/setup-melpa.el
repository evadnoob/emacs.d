(when (> emacs-major-version 23)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives 
               '("melpa" . "http://melpa.milkbox.net/packages/")
               'APPEND))


(provide 'setup-melpa)

