
(defun isearch-yank-regexp (regexp)
  "Pull REGEXP into search regexp." 
  (let ((isearch-regexp nil)) ;; Dynamic binding of global.
    (isearch-yank-string regexp))
  (if (not isearch-regexp)
      (isearch-toggle-regexp))
  (isearch-search-and-update))

(defun isearch-yank-symbol ()
  "Put symbol at current point into search string."
  (interactive)
  (let ((sym (find-tag-default)))
    (if (null sym)
        (message "No symbol at point")
      (isearch-yank-regexp
       (concat "\\_<" (regexp-quote sym) "\\_>")))))

;;(define-key isearch-mode-map "\C-\M-w" 'isearch-yank-symbol)
(define-key isearch-mode-map "\C-w" 'isearch-yank-symbol)
(custom-set-variables 
 '(isearch-allow-scroll t))

(provide 'setup-isearch)
