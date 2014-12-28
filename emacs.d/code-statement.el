(defconst is-debug-enabled t)

(setq alist-of-unmake-patterns 
      '(("^ *\\+ *?\"" . "")
        ("^ *\\+ *?'" . "")
        ("\" ?\\+" . "")
        ("^ *?\"" . "")
        ("\";$" . "")
        ("\"$" . "")
        ("'$" . "")
        ("\" ?\\+$" . "")
        ("\\\\n" . "")
        ))

(setq alist-of-make-patterns
      '(("^\\(.*\\)$" .  "+ \'\\1\'")
        ))

(setq alist-of-line-break-patterns
      (list "AND" "FROM"))

;; (defun debug(msg) 
;;   ""
;;   (if is-debug-enabled
;;       (princ msg)))

(defun make-code-statement() 
  "operate on region, add java string formatting"
  (interactive)
  
  (if (not (and mark-active))
      (error "region not selected"))
  
  (setq begin (copy-marker (region-beginning)))  
  (setq limit (copy-marker (region-end)))  
  (goto-char begin)


  ;;(debug (format "\n%s\n" alist-of-make-patterns ))

  (dolist (p alist-of-make-patterns)
    (goto-char begin)
    ;;(debug (format "cdr: %s  car: %s" (car p) (cdr p)))
    (while (re-search-forward (car p) limit t)
      (replace-match (cdr p))))
  (indent-region begin limit)
  (deactivate-mark))


(defun unmake-code-statement() 
  "operate on region, remove java string formatting"
  (interactive)
  
  (if (not (and mark-active))
      (error "region not selected"))

  ;;(debug (format "begin: %s end: %s" (region-beginning) (region-end)))

  (setq begin (copy-marker (region-beginning)))  
  (setq limit (copy-marker (region-end)))  
  (goto-char (region-beginning))

  
  ;;(debug (format "\n%s\n" alist-of-unmake-patterns ))

  (dolist (p alist-of-unmake-patterns)
    (goto-char begin)
    (while (re-search-forward (car p) limit t)
      (replace-match (cdr p)))
    (deactivate-mark))
  
  
  (defun normalize-select-statement()
    "perform some indentation and line breaking on a unformatted sql string"
    (interactive)
    (if (not (and mark-active))
        (error "region not selected"))

    (setq begin (copy-marker (region-beginning)))  
    (setq limit (copy-marker (region-end)))  

    (goto-char (region-beginning))

    (while (re-search-forward "\n" limit t)
      ;;(debug (format "  match: %s " (match-beginning 0)) )
      (replace-match ""))

    (goto-char (region-beginning))

    (while (re-search-forward "\," limit t)
      ;;(debug (format "  match: %s " (match-beginning 0)) )
      (replace-match ",\n"))
    )
)
  
;;;###autoload
  (defun strip-line-numbers-from-sql() 
    "operate on region, remove prefixed line number"
    (interactive)
    
    (if (not (and mark-active))
        (error "region not selected"))
    
    (setq begin (copy-marker (region-beginning)))  
    (setq limit (copy-marker (region-end)))  
    (message (format "you are here %s %s" begin limit))
    (goto-char begin)

    (while (re-search-forward "^ \\{1,2\\}[0-9]\\{1,2\\}" limit t)
      (message (format "  match: %s " (match-beginning 0)) )
      (replace-match ""))
    (deactivate-mark))


  (provide 'code-statement)
