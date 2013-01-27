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
 '(("rtcr_user@ort152a"
     :username "rtcr_user"
     :password "rtcr_user"
     :database "ort152a")
   ("rtcr_user@wca-qa"
     :username "rtcr_user"
     :password "rtcr_user"
     :database "wca-qa")
   ("wca@u0103223-xpa"
     :username "rtcr_ucdb"
     :password "rtcr_ucdb"
     :database "wca@u0103223-xpa")
   ("system@wca"
     :username "system"
     :password "admin"
     :database "wca"
     :options "as sysdba")
   ("sys@wca"
     :username "sys"
     :password "dmbpw"
     :database "wca"
     :options "as sysdba")
   ("sys@dmb10g"
     :username "sys"
     :password "dmbpw"
     :database "dmb10g"
     :options "as sysdba")
   ("rtcr_ucdb@dmb10g"
     :username "rtcr_ucdb"
     :password "rtcr_ucdb"
     :database "dmb10g")
   ("rtcr_ucdb@wca"
     :username "rtcr_ucdb"
     :password "rtcr_ucdb"
     :database "wca")
   ("rtcr_ucdb@wca-dev"
     :username "rtcr_ucdb"
     :password "rtcr_ucdb"
     :database "wca-dev")
   ("rtcr_user@wca-prod"
    :username "rtcr_user"
     :password "rtcr_user"
     :database "wca-prod")))

(defvar known-schemas-alist-old nil 
  "an alist of property lists for schema info")
 (setq 
 known-schemas-alist-old
 '(("devaaa@devkhub"
    :username "devaaa"
    :password "cedev"
    :database "devkhub")
   ("devcc@devindia"
    :username "devcc"
    :password "bball"
    :database "devindia")
   ("devggg@devkhub"
    :username "devggg"
    :password "geewiz"
    :database "devkhub")
   ("devjjj@devkhub"
    :username "devjjj"
    :password "jay"
    :database "devkhub")
   ("devlll@devkhub"
    :username "devlll"
    :password "devel"
    :database "devkhub")
   ("devkkk@devkhub"
    :username "devkkk"
    :password "newyear"
    :database "devkhub")
   ("deveee@devkhub"
    :username "deveee"
    :password "deve"
    :database "devkhub")
   ("ivbb@ivtkhub"
    :username "ivbb"
    :password "grouper"
    :database "ivtkhub")
   ("ivaa@ivtkhub"
    :username "ivaa"
    :password "minnow"
    :database "ivtkhub")
   ("ivcc@ivtkhub"
    :username "ivcc"
    :password "sunfish"
    :database "ivtkhub")
   ("ivdd@ivtkhub"
    :username "ivdd"
    :password "shark"
    :database "ivtkhub")
   ("ivff@ivtkhub"
    :username "ivff"
    :password "frankly"
    :database "ivtkhub")
   ("ivgg@ivtkhub"
    :username "ivgg"
    :password "lobster"
    :database "ivtkhub")
   ("ivii@ivtkhub"
    :username "ivii"
    :password "inky"
    :database "ivtkhub")
   ("ivkk@ivtkhub"
    :username "ivkk"
    :password "another"
    :database "ivtkhub")
   ("ivjj@ivtkhub"
    :username "ivjj"
    :password "jayjay"
    :database "ivtkhub")
   ("ivtt@ivtkhub"
    :username "ivtt"
    :password "blinky"
    :database "ivtkhub")
   ("ivnn@ivtkhub"
    :username "ivnn"
    :password "guppie"
    :database "ivtkhub")
   ("khub@pakhub"
    :username "khub"
    :password "monks"
    :database "pakhub")
   ("khub@pbkhub"
    :username "khub"
    :password "bubbleboy"
    :database "pbkhub")
   ("perfstat@pbkhub"
    :username "perfstat"
    :password "perfstat_pbkhub"
    :database "pbkhub")
   ("khub@pakhub_shake"
    :username "khub"
    :password "monks"
    :database "pakhub_shake")
   ("kbuild@pakhub_shake"
    :username "kbuild"
    :password "pakhub_kb"
    :database "pakhub_shake")
   ("kcamp@pakhub_shake"
    :username "kcamp"
    :password "pakhub_kc"
    :database "pakhub_shake")
   ("khub@pakhub_frylock"
    :username "khub"
    :password "monks"
    :database "pakhub_frylock")
   ("readuser_perf@dpisis"
    :username "readuser_perf"
    :password "spiffy"
    :database "dpisis")
   ("readuser@ivroc02"
    :username "readuser"
    :password "readuser_ivroc02"
    :database "ivroc02")
   ("readuser@ivroc05"
    :username "readuser"
    :password "readuser_ivroc05"
    :database "ivroc05")
   ("readuser@ivroc07"
    :username "readuser"
    :password "readuser_ivroc07"
    :database "ivroc07")
   ("ivroc08"
    :username "khub"
    :password "starling"
    :database "ivroc08")
   ("readuser@ivroc09"
    :username "readuser"
    :password "readuser_ivroc09"
    :database "ivroc09")
   ("readuser@ivroc11"
    :username "readuser"
    :password "readuser_ivroc11"
    :database "ivroc11")
   ("devfff"
    :username "devfff"
    :password "yellow"
    :database "devkhub")
   ("readuser@ivroc01"
    :username "readuser"
    :password "readuser_ivroc01"
    :database "ivroc01")
   ("khub@ivroc09"
    :username "khub"
    :password "spiffy"
    :database "ivroc09")
   ("khub@ivroc13"
    :username "khub"
    :password "crossbill"
    :database "ivroc13")
   ("khub@ivroc03"
    :username "khub"
    :password "cardinal"
    :database "ivroc03")
   ("khub@ivroc04"
    :username "readuser"
    :password "readuser_ivroc04"
    :database "ivroc04")))




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
   sql-input-ring-file-name "~/.sql_history"
   ;;comint-input-ring-separator "\n"
   ;;sql-input-ring-separator "\n"
   comint-input-ring-file-name sql-input-ring-file-name
   sql-pop-to-buffer-after-send-region t)
  (rename-buffer new-buffer-name t)
  (comint-read-input-ring)
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



;;(sql-add-product-keywords 'oracle
;;  '(("VARCHAR2" . font-lock-type-face)))