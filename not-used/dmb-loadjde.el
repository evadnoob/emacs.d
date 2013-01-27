; loadjde.el


;; ;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/jde-2.3.5/lisp"))
;; ;;(add-to-list 'load-path (expand-file-name "~/.emacs.p/jde-2.3.5.1/lisp"))
;; (add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/jdee.trunk/jde/dist/lisp"))

;; ;;(add-to-list 'load-path (expand-file-name "~/.emacs.p/jdee-2.3.6/jde/lisp"))
;; ;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/cedet-1.0pre3"))
;; ;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/cedet-1.0pre4"))
;; (add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/cedet-1.0pre6"))
;; ;;(add-to-list 'load-path (expand-file-name "~/dev/cedet"))
;; (add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/elib-1.0"))

;; (setq semantic-load-turn-useful-things-on t)
;; ;;(semantic-decoration-mode 1)
;; ;;(set-face-attribute 'semantic-tag-boundary-face nil :slant 'italic)


;; ;;(load-file "~/dev/cedet/common/cedet.el")
;; ;;(load (expand-file-name "~/dev/cedet/common/cedet") nil t)
;; (load-file (*emacs ".emacs.x/.emacs.p/cedet-1.0pre6/common/cedet.el"))
;; ;;(require 'wisent-java)

;; ;;(semantic-load-enable-excessive-code-helpers)
;; (;;require 'semantic-ia)

;; (require 'jde)


;; Use the full Java 1.5 grammar to parse Java files
;;($a$u$toload 'wisent-java-default-setup "wisent-java"
;;  "Hook run to setup Semantic in `java-mode'." nil nil)



(defun my-build-function()
   "my hack for building"
   (interactive)
   (setq explicit-shell-file-name shell-file-name)
   (jde-ant-build)
   (setq explicit-shell-file-name shell-file-name))


;; If you want Emacs to defer loading the JDE until you open a 
;; Java file, edit the following line
;;(setq defer-loading-jde nil)
;; to read:
;;
(setq defer-loading-jde t)
;;




(if defer-loading-jde
    (progn
      (autoload 'jde-mode "jde" "JDE mode." t)
      (setq auto-mode-alist
	    (append
	     '(("\\.java\\'" . jde-mode))
	     auto-mode-alist)))
  (require 'jde)
  (require 'jde-jalopy))



;; (setq font-lock-maximum-decoration 
;;       '((jde-mode . 3) 
;;         (c++-mode . 3)
;;         (c-mode . 3))

;;       c-doc-comment-style
;;       '((jde-mode . javadoc)))




(c-add-style "dmb-gnu"
  '("gnu" (c-basic-offset . 3)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist .
                            ;;'((substatement-open before after)))
                            '((substatement-open after)))
    (c-offsets-alist
     (substatement-open . 0)
     (inexpr-class . 0)
     (inher-cont     . c-lineup-java-inher)
     (arglist-intro  . +)
     ;;(arglist-close  . c-lineup-arglist)
     (arglist-close  . 0)
     
     (comment-intro  . 0)
     (case-label        . 4)
     (func-decl-cont . 0)) ;;c-lineup-java-throws))
    (c-cleanup-list . (scope-operator
                       empty-defun-braces
                       brace-catch-brace
                       comment-close-slash
                       defun-close-semi))
    (c-special-indent-hook . c-gnu-impose-minimum)
    (c-block-comment-prefix . "")) nil)

(c-add-style "my-c-style"
  '((c-tab-always-indent        . t)
    (c-comment-only-line-offset . 4)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (inexpr-class-close '-5)
                                   (inexpr-class-open '--)
                                   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (substatement-open . 0)
                                   (case-label        . 4)
                                   (block-open        . 0)
                                   (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t)) nil)





(eval-after-load "jde"
  '(progn 

     (require 'flymake)
     ;;(require 'jde-eclipse-compiler-server)

     ;;(global-unset-key "\C-x m d")
     ;;(define-key jde-mode-map "C-x m d" 'senator-mark-defun)
     
     (define-key jde-mode-map [f11] 'flymake-mode)
     
     (define-key jde-mode-map "<"      'c-electric-lt-gt)
     (define-key jde-mode-map ">"      'c-electric-lt-gt)
     
     (define-key jde-mode-map "("      'skeleton-pair-insert-maybe)
     
     
     (define-key jde-mode-map [f8] 'senator-next-tag)
     (define-key jde-mode-map [C-f8] 'senator-previous-tag)
     (define-key jde-mode-map "\C-\M-h" 'senator-mark-defun)
     (define-key jde-mode-map "\M-h" 'mark-paragraph)
     (define-key jde-mode-map [C-f8] '(lambda() 
                                        (interactive) 
                                        (senator-previous-tag)))
     (define-key jde-mode-map "\C-\c\io" 'jde-import-organize)
     (define-key jde-mode-map [C-f5] 'jde-ant-build)
     
     (define-key jde-mode-map [f8] '(lambda() 
                                      (interactive) 
                                      (senator-next-tag)))
     (require 'decompile)
     (define-key jde-mode-map (kbd "C-x n d") 'senator-narrow-to-defun)

     ;;remove these two hooks, so that compilation doesn't jump to first error.
     (remove-hook 'jde-ant-build-hook 'jde-compile-finish-refresh-speedbar)
     (remove-hook 'jde-ant-build-hook 'jde-compile-finish-update-class-info)
     
     (define-key jde-mode-map "\C-x\md" 'senator-mark-defun)
     (message "keybindings after load jde.el")
     
     (setq jdc-parameter (quote ("-space" "-b" "-p" "-dead" "-ff" "-i" "-nonlb" "-lnc")))
     
     
     (load-library "maven-call")
     
     ;; avoids 'Buffer Somejavafile.java was not set up for parsing' errors
     (setq global-senator-minor-mode t)

     (setq-default 

      jde-imenu-enable nil
      jde-which-method-max-length 150
      jde-make-program "mvn"
      jde-which-method-mode t
      jde-ant-read-target t
      jde-ant-args " -emacs -Djavac.debug=true"
      jde-ant-enable-find nil
      ;;jde-enable-senator nil
      jde-auto-parse-enable nil
      ;;jde-project-filename ".prj.el"

      )
     
     (setq c-indent-tabs-mode t ; Pressing TAB should cause indentation
           c-indent-level 3     ; A TAB is equivilent to four spaces
           c-argdecl-indent 0   ; Do not indent argument decl's extra
           c-tab-always-indent t
           c-echo-syntactic-information-p nil ;; print syntactic info
           c-report-syntactic-errors nil ;; 
           backward-delete-function nil) ; do NOT expand tabs when deleting them
     
     ;;(c-add-style "my-c-style" '((c-continued-statement-offset 2))) ; If a statement continues on the next line, indent the continuation by 4


     (require 'semantic-tag-folding)
     (custom-set-variables
      '(jde-project-file-name ".jde.project")
      '(jde-import-excluded-classes 
        '(
          ("^bsh\\..*")
          ("^com\\.sun\\..*")
          ("^java.awt\\..*")
          ("^java\\.lang\\.[^.]*$" . t)
          ("^java\\.security\\.Timestamp")
          ("^org\\.apache\\.activemq\\.util\\.TransactionTemplate")
          ("^org\\.apache\\.tools\\.ant\\..*")
          ("^sun\\..*")
          ("edu\\.emory\\.mathcs\\..*")
          ("java.util.Arrays.ArrayList")
          ("java.util.logging.Logger")
          ("javax.swing.Spring")
          ("javax.swing.text.html.Map")
          ("javax.swing.tree.RowMapper")
          ("javax.xml.transform.Source")
          ("javax\\.naming\\..*")
          ("junit.framework.Test")
          ("org.apache.camel.processor.Logger")
          ("org.apache.camel.util.ResolverUtil.Test")
          ("org.junit.Test")
          ("org.testng.log4testng.Logger")
          ("org\\.apache\\.xerces\\.impl\\..*")
          ("org\\.springframework\\.orm\\.toplink\\..*")

          (jde-import-current-package-p . t)))

      '(jde-global-classpath (split-string classpath jde-classpath-separator))
      '(jde-import-blank-line-between-groups t)
      '(jde-import-group-of-rules (quote (("^java?\\." . "A") ("^javax?\\." . "B") ("^junit?\\." . "C") ("^org\\.apache?\\." . "E") ("^org\\.hibernate?\\." . "F")  ("^org\\.springframework?\\." . "G")  ("^org?\\." . "D")  ("^edu?\\." . "H") ("^com\\.google\\." . "I") ("^com\\.gwtext\\." . "J") ("^com\\.thomson\\." . "K"))))
      '(jde-import-sorted-groups (quote gor))
      '(jde-jdk (quote ("1.6.0")))
      '(jde-jdk-registry (quote (("1.6.0" . "f:/devtools/jdk1.6.0_14"))));
      '(jde-which-method-max-length 150)

      '(semantic-tag-folding-allow-folding-of (quote ((type) (function) (variable) (include . t) (comment) (package)))))
     
     (message "defining my-jde-mode-hook")
     (defun my-jde-mode-hook ()
       ""
       ;;(message "in my-jde-mode-hook")
       (setq 
        tab-width 3
        jde-which-method-mode t)
       
       ;;(c-set-style "java")
       ;;(c-set-style "k&r")
       
       ;; to see syntax info while indenting
       ;;
       ;;(setq c-basic-offset 3
       ;;      c-echo-syntactic-information-p t)
       
       (jde-annotations-setup)
       ;;(c-set-offset 'substatement-open '0) ; brackets should be at same indentation level as the statements they open
       ;;(c-set-offset 'inexpr-class-open '--)  ;;kinda fixes indent for inner class close, but also affects outter class
;;;       (c-set-offset 'inexpr-class  
;;;                     (lambda (syntax)
;;;                       (if (eq syntax 'class-open)
;;;                           0
;;;                         0)));;kinda fixes indent for inner class close, but also affects outter class
       
       ;;((inexpr-class) (inclass 4047) (inline-open)), indent: 12

       ;;(c-set-offset 'arglist-intro 2) ;;first argument on new line, how much to indent
       ;;(c-set-offset 'brace-list-entry after) ;;first argument on new line, how much to indent
       ;;(c-set-offset 'block-open 1)
       ;;(flymake-mode-on)
       (c-set-style "dmb-gnu")
       (c-toggle-electric-state 1)

       ;;; (setq skeleton-pair t)
;;;        (global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;;;        (global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
;;;        (global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
       
       (c-toggle-auto-newline -1)

       (defalias 'show-block 'semantic-tag-folding-show-block)
       (defalias 'show-all 'semantic-tag-folding-show-all )

       (defalias 'hide-all 'semantic-tag-folding-fold-all)
       (defalias 'hide-block 'semantic-tag-folding-fold-block )
       (define-key jde-mode-map "\C-\c\is" 'show-block)
       (define-key jde-mode-map "\C-\c\ih" 'hide-block)
       
       (define-key jde-mode-map "\C-c\C-v" 'jde-complete-minibuf)

       (senator-minor-mode 1)

       (jde-electric-return-mode 1)
       )
     
     (message "setting up semantic tag folding")
     (add-hook 'semantic-init-hooks 
               (lambda() 
                 (semantic-tag-folding-mode 1)
                 ;;(semantic-load-enable-gaudy-code-helpers)
                 ;;(semantic-load-enable-code-helpers)
                 ;;(semantic-decoration-mode 1)
                 ))
     
     
     ;;(c-set-offset 'block-open '+)
     ;;(c-set-offset 'brace-list-open '+)   ; all "opens" should be indented by the c-indent-level

     
     ;;(c-set-style "PERSONAL")

     (custom-set-variables 
      '(c-echo-syntactic-information-p nil)
      '(flymake-allowed-file-name-masks 
        (cons '("\\.java\\'"  . (jde-ecj-server-flymake-init jde-ecj-flymake-cleanup))
              flymake-allowed-file-name-masks  ))
      '(flymake-no-changes-timeout 2.0)
      ;;'(flymake-log-level -1)
      '(flymake-log-level 0) ;;3 0 = ERROR -1 = NONE 
      '(flymake-start-syntax-check-on-newline nil)
      '(jde-compiler '(("eclipse java compiler server" "c:/Documents and Settings/u0103223/.emacs.x/.emacs.d/ecj-3.3M6.jar")))
      ;;'(jde-compiler '(("eclipse java compiler server" "~/.emacs.d/ecj.jar")))
      ;;'(jde-compiler '(("eclipse java compiler server" "~/.emacs.d/ecj-3.3M6.jar")))
      ;;'(jde-compiler '(("eclipse java compiler server" (expand-file-name "~/.emacs.d/ecj-3.3M6.jar")))))
      
      
      '(jdibug-connect-host "localhost")
      '(jdibug-connect-port 6001)

      '(jde-ecj-command-line-args (quote ("-d" "none" "-1.6" "-warn:unused,semicolon" "-maxProblems" "50" "-g")))
      ;; 1) use this value 't' to scan class, doesn't work with -attached sessions
      ;;'(gud-jdb-use-classpath t)
      ;;
      ;; use this method when attaching to remote vm
      '(gud-jdb-use-classpath nil)
      '(gud-jdb-directories 
        (list 
         "f:/wca_internal_forward/WCA_Server_Servlet/src/main/java" "f:/wca_internal_forward/WCA_Server_Servlet/src/main/java"))
      )

     (custom-set-faces
      '(flymake-errline ((((class color)) (:underline "OrangeRed"))))
      '(flymake-warnline (( ((class color)) (:underline "DarkSlateBlue")))) ;;yellow
      '(semantic-tag-boundary-face (( ((class color)) (:slant italic)))))
     
     (add-hook 'c-mode-common-hook
               (lambda ()
                 (c-set-style "dmb-gnu")))
     
     (add-hook 'jde-mode-hook 'my-jde-mode-hook)

     
     (define-abbrev jde-mode-abbrev-table "ifst" "" 'jde-gen-if)


     )) 

;;
;; load this outside of the defered load on purpose
;;

;(require 'jde-ant)

;;(require 'jde)
;;narrow-to-def doesn't work in jde mode, use senator verion instead




(defun dmb-add-current-class-gud-jdb-directories()
  "add the current buffers directory "
  (interactive)
  (if (not (eq major-mode 'jde-mode))
      (error "Must be a jde-mode buffer to add it to the debugger"))
  (add-to-list 'gud-jdb-directories (file-name-directory (buffer-file-name (current-buffer))))
  )

;;(setenv "PATH" (concat devtools-dir "/apache-maven-2.0.8/bin" ";" (getenv "PATH")))

(setenv "JAVA_HOME" (getenv "JAVA_SUN_HOME"))


(defun dmb-gen-get-set-methods()
  ""
  (interactive)
  (save-excursion 
    (let ((tmp-tag-type (semantic-tag-type (senator-current-tag)))
          (tmp-tag-name (semantic-tag-name (senator-current-tag)))
          (tmp-saved-tag (senator-current-tag)))
      ;;(senator-kill-tag (senator-current-tag))
      (end-of-line)
      (jde-gen-get-set-methods (concat tmp-tag-type "," tmp-tag-name ))
      (message "%s" tmp-saved-tag )
      (senator-jump tmp-saved-tag)
      (semantic-momentary-highlight-tag tmp-saved-tag))))
      ;;(senator-kill-tag))))

(custom-set-variables 
 '(jde-jalopy-option-path "f:/devtools/jalopy-console-0.1-1.5rc3")
 '(jde-jalopy-option-encoding "utf-8")
 '(jde-jalopy-option-preferences-file "f:/patentTracker/patentTrackerDocumentation/5-Implementation/codes-eclipse-jalopy.xml")
 '(jde-jalopy-option-command-line-args "-l WARN"))



(message "done. ( loadjde.el)")

