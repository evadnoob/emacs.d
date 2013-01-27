
;;
;; setup 'buffer show'
;;
(require 'bs)
(add-to-list 'bs-configurations
             '("files-and-shell" "^\\*shell\\*$" nil nil bs-visits-non-file
              bs-sort-buffer-interns-are-last))

(custom-set-variables '(bs-default-configuration "files-and-shell"))


(provide 'setup-buffer-switching)
