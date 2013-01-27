(require 'sql)

;;(const :tag "oracle9i" oracle9i)

(defvar sql-mode-oracle9i-font-lock-keywords nil
      "XyzDB SQL keywords used by font-lock.")

(defun sql-highlight-oracle9i-keywords ()
  "Highlight XyzDB keywords."
      (interactive)
      (sql-set-product 'oracle))

(push 
 '(oracle9i
     :font-lock sql-mode-oracle-font-lock-keywords
     :sqli-login nil
     :sqli-connect sql-connect-oracle
     :sqli-prompt-regexp "^SQL> "
     :sqli-prompt-length 5
     :syntax-alist ((?$ . "w") (?# . "w"))) sql-product-alist)

(defvar known-schemas-alist nil 
  "an alist of property lists for schema info")
 (setq 
 known-schemas-alist
 '(
  ("dw_admin@ct05-scan.int.westgroup.com"
    :username "dw_admin"
    :password "pubrecdev201106"
    :database "ct05-scan.int.westgroup.com/pwh_read_prod.int.westgroup.com")

  ("dw_admin@ct03-scan.int.westgroup.com"
    :username "dw_admin"
    :password "pubrecdev201106"
    :database "ct03-scan.int.westgroup.com/dw_prod.int.westgroup.com")

  ("dm_admin@ct03-scan.int.westgroup.com"
    :username "dm_admin"
    :password "dm_admin"
    :database "ct03-scan.int.westgroup.com/dw_prod.int.westgroup.com")

  ("dw@ct01-scan.int.westgroup.com"
    :username "dw"
    :password "pubrecdev!"
    :database "ct01-scan.int.westgroup.com/dw_dev.int.westgroup.com")

  ("dw_admin@ct01-scan.int.westgroup.com"
    :username "dw_admin"
    :password "dw_admin"
    :database "ct01-scan.int.westgroup.com/dw_qa.int.westgroup.com")

  ("sys@ora1"
    :username "sys"
    :password "dmbpw"
    :database "ora1"
    :options "as sysdba")
   ))




;;; (add-to-list 'known-schemas-alist 
;;;              '("bogus@dpisis"
;;;               :username "readuser_perf"
;;;               :password "spiffy"
;;;               :database "dpisis"))
;;(setenv "SQLPATH" (concat (getenv "USERPROFILE") "/sql" ";" (getenv "USERPROFILE") "/sql/wayne"))
(setenv "SQLPATH" (concat (getenv "HOME") "/sql" ";" (getenv "HOME") "/sql/wayne"))
(defun sql-oracle-any-schema (schema)
  "given one of the names in `known-schemas-alist' launch sql or 
switch to buffer if it already exists."
  (setq 
   sql-database (plist-get (cdr (assoc schema known-schemas-alist)) :database)
   sql-user (plist-get (cdr (assoc schema known-schemas-alist)) :username)
   sql-password (plist-get (cdr (assoc schema known-schemas-alist)) :password))
  
   (if (plist-member (cdr (assoc schema known-schemas-alist)) :options)
       (setq 
       sql-oracle-options (list (plist-get (cdr (assoc schema known-schemas-alist)) :options)))
     (setq sql-oracle-options nil))

  (setq tmp-buffer-name (concat "sql [" sql-user "@" sql-database "]"))
  (if (get-buffer tmp-buffer-name)
      (pop-to-buffer tmp-buffer-name)
    (sql-product-interactive 'oracle9i)))

(defun select-oracle-schema ()
  "prompt for schema, use completion to show options.
Press SPACE for options, TAB to complete."
  (interactive)
  (setq chosen 
        (completing-read "select schema: " known-schemas-alist nil t nil))
  (message (format "%s type %s" chosen (type-of chosen )))
  (sql-oracle-any-schema chosen))

(defalias 'oracle 'select-oracle-schema)




;;
;; sql-mode
;;
(require 'sql)
(defun my-sql-mode-hook () 
  "sql mode hook that renames *SQL* buffer to reflect connection string"
  (message (format "entering sql mode, current buffer name is %s" (buffer-name (current-buffer))))
  (define-key sql-mode-map [delete] 'delete-char)
  ;;(define-key sql-mode-map [tab] 'indent-relative-maybe)
  (define-key sql-mode-map [tab] 'indent-according-to-mode)

  (setq 
   comint-scroll-to-bottom-on-input 'this
   new-buffer-name (concat "sql [" (downcase sql-user) "@" (downcase sql-database) "]")
   tab-width 8
   ;;sql-electric-stuff (quote semicolon)
   ;;sql-electric-stuff nil
   comint-input-ignoredups t
   sql-input-ring-file-name (concat "~/.sql_history" "." (downcase sql-user) "@" (downcase (replace-regexp-in-string "/" "_" sql-database)))
   ;;comint-input-ring-separator "\n--"
   ;;sql-input-ring-separator "--"
   ;;comint-input-ring-separator "\n"
   ;;comint-input-ring-file-name sql-input-ring-file-name
   comint-input-ring-size 300
   
   sql-pop-to-buffer-after-send-region t)
  (rename-buffer new-buffer-name t)
  
  (comint-read-input-ring)
  (abbrev-mode 1)
  (cd "~/sql")
  (sql-set-product 'oracle9i))

;(add-hook 'sql-mode-hook 'my-sql-mode-hook)
(add-hook 'sql-interactive-mode-hook 'my-sql-mode-hook)
(add-to-list 'same-window-regexps "^\\*SQL\\*.*")
(add-to-list 'same-window-regexps "^\\*SQL\\*")
(add-to-list 'same-window-regexps "^sql \\[.*?\\]$")

(setenv "NLS_LANG" "AMERICAN_AMERICA.UTF8");

(add-to-list 'auto-mode-alist '("\\.pks\\'" . sql-mode))
(add-to-list 'auto-mode-alist '("\\.pkb\\'" . sql-mode))
(add-to-list 'auto-mode-alist '("\\.ddl\\'" . sql-mode))
(add-to-list 'auto-mode-alist '("\\.dml\\'" . sql-mode))


;;(setq auto-mode-alist
;;     (append
;;       '(("\\.\\(p\\(?:k[bg]\\|ls\\)\\|sql\\)\\'" . plsql-mode))
;;       auto-mode-alist))

(eval-after-load "sql-mode"
  '(load-library "sql-indent"))

(custom-set-variables 
   '(sql-indent-offset 3)
   '(sql-indent-debug t))
  

;;(sql-add-product-keywords 'oracle
;;  '(("VARCHAR2" . font-lock-type-face)))


(defadvice just-one-space (around delete-whitespace (&optional n) activate compile)
  "redefined the behavior of just-one-space, allow for universal
argument to delete only unneeded whitespace according to major
mode.."
  (interactive "*P")
  (if current-prefix-arg       (delete-whitespace)
    ad-do-it))


(defadvice sql-oracle (around sql-oracle (&optional n) activate compile)
  "redefined the behavior of sql-oracle, run as sysdba"
  (interactive "*P")
  (if current-prefix-arg
      (setq sql-oracle-options (list "as sysdba")))
  ad-do-it
  (setq sql-oracle-options nil))



(setq sql-oracle-options (list "as sysdba"))
(setq sql-oracle-options nil)

(require 'tempo)

(define-abbrev sql-mode-abbrev-table "selcnt" ""
      (tempo-define-template
       "selcnt-template"
       '("select count(*) from")))

(define-abbrev sql-mode-abbrev-table "todate" ""
      (tempo-define-template
       "to_date-template"
       '("to_date('" ~ "', 'YYYY/MM/DD')")))


(define-key sql-mode-map "\d" 'backward-delete-char-untabify)
