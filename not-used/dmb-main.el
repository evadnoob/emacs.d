(require 'saveplace)

(when running-xemacs
  (paren-set-mode)
  (setq 
   progress-feedback-use-echo-area t 
   progress-feedback-style "small"
   lazy-lock-minimum-size 1024
   lazy-lock-hide-invisible nil
   lazy-lock-defer-on-scrolling t
   lazy-lock-defer-time 15
   lazy-lock-defer-on-the-fly t
   lazy-lock-defer-contextually t
   lazy-lock-walk-windows nil
   font-lock-always-fontify-immediately nil
   mswindows-alt-by-itself-activates-menu nil
   mwheel-follow-mouse t))



;; (require 'locate)
;; (icicle-define-command icicle-locate ; Command name
;;   "Run the program `locate', then visit files.
;; Unlike `icicle-locate-file' this command is a wrapper for the program `locate'." ; Doc string
;;   find-file                             ; Function to perform the action
;;   "File: " (mapcar #'list (split-string (shell-command-to-string (format "%s '%s'" locate-command query)) "\n" t))
;;   nil t nil 'locate-history-list nil nil
;;   ((query (read-string "Locate: "))))

;; (custom-set-faces
;;  '(icicle-complete-input ((((background dark)) nil)))
;;  '(icicle-completing-mustmatch-prompt-prefix ((((type x w32 mac     graphic) (class color)) (:foreground "Cyan"))))
;;  '(icicle-completing-prompt-prefix ((((type x w32 mac graphic) (class     color)) (:foreground "Red"))))
;;  '(icicle-current-candidate-highlight ((((background dark))     (:background "gray20"))))
;;  '(icicle-historical-candidate ((((background dark)) (:foreground     "White"))))
;;  '(icicle-prompt-suffix ((((type x w32 mac graphic) (class color)     (background dark)) (:foreground "LightSlateBlue"))))
;;  '(icicle-search-context-level-1 ((((background dark)) (:background     "gray20" :weight bold))))
;;  '(icicle-search-context-level-2 ((((background dark)) (:background     "gray15" :weight bold))))
;;  '(icicle-search-context-level-3 ((((background dark)) (:background     "gray10" :weight bold))))
;;  '(icicle-search-context-level-4 ((((background dark)) (:weight     bold))))
;;  '(icicle-search-context-level-5 ((((background dark)) (:weight     bold))))
;;  '(icicle-search-context-level-6 ((((background dark)) (:weight     bold))))
;;  '(icicle-search-context-level-7 ((((background dark)) (:weight     bold))))
;;  '(icicle-search-context-level-8 ((((background dark)) (:weight     bold))))
;;  '(icicle-search-current-input ((t (:foreground "green"))))
;;  '(icicle-search-main-regexp-current ((((background dark))     (:foreground "DodgerBlue"))))
;;  '(icicle-search-main-regexp-others ((((background dark)) (:foreground     "SeaGreenk"))))
;;  '(icicle-special-candidate ((((background dark)) (:foreground     "yellow"))))
;;  '(icicle-whitespace-highlight ((t (:background "#300")))))

(defalias 'qrr 'query-replace-regexp)


(require 'saveplace)

(when running-xemacs
  (paren-set-mode)
  (setq 
   progress-feedback-use-echo-area t 
   progress-feedback-style "small"
   lazy-lock-minimum-size 1024
   lazy-lock-hide-invisible nil
   lazy-lock-defer-on-scrolling t
   lazy-lock-defer-time 15
   lazy-lock-defer-on-the-fly t
   lazy-lock-defer-contextually t
   lazy-lock-walk-windows nil
   font-lock-always-fontify-immediately nil
   mswindows-alt-by-itself-activates-menu nil
   mwheel-follow-mouse t))


(if is-emacs-23
    (require 'grep))

;;(require 'grep+)
(setq 
  grep-window-height 25
  grep-hit-face 'dmb-grep-hit-face
  ;;grep-match-face 'dmb-grep-match-face
  grep-match-face 'dmb-face-three
  grep-scroll-output t
  grep-highlight-matches t
  grep-use-null-device nil)
(set-face-underline-p 'dmb-grep-match-face nil)
(if is-emacs-23
    (set-face-underline-p 'compilation-line-number nil))

(add-hook 
 'grep-mode-hook 
 (lambda () 
   "hook to rename buffer to include find args"
   (if (string-match "\*grep\*" (buffer-name (current-buffer)))
       (progn
         (setq truncate-lines t)
         (rename-buffer (format "*grep* [%s]" command-args) 1)))))

(add-hook 
 'dired-mode-hook 
 (lambda () 
   "hook to rename buffer to include find args"
   (if (string-match "\*Find\*" (buffer-name (current-buffer)))
       (progn (rename-buffer (format "*find* [%s]" find-args) 1)))))


(defun simple-grep (dir options)
  "a slightly more improved grep-find"
  ;;(interactive "Dgrep (directory): \nsgrep (directory): %s (like this): \ns ")
  (interactive (list (read-directory-name "grep (directory): ")
                     (read-from-minibuffer "grep (args): " '("grep --include=\"*.*\" -nrH -e  ./*" . 30) nil nil 'grep-history)))
  (save-excursion
    (setq buffer (get-buffer-create "*grep*"))
    
    (with-current-buffer buffer
      (cd dir)
      (setenv "GREP_OPTIONS" "--exclude=*/TAGS --exclude=.#* --exclude=*\~ --exclude=*/CVS/* --exclude=*.svn-base")
      ;;fake out the grep-mode-hook by setting command-args to options
      (setq command-args options)
      ;;run grep like: grep -Hnr --include="*.java" "OfferBroker" c:/khub/ReqRecSearchFilter/java/
      ;;--exclude=\".#*\" --exclude=\"*\\~\"

      (compilation-start options 'grep-mode nil t)
      ;;(setq next-error-function 'dave-next-error)
)))



;;
;; Replace "yes or no" with y or n
;;
(defun yes-or-no-p (arg)
  "An alias for y-or-n-p, because I hate having to type 'yes' or 'no'."
  (y-or-n-p arg))




;;
;; load python mode
;;
(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))

(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))

(setq interpreter-mode-alist
      (cons '("python2.4" . python-mode) interpreter-mode-alist))

(autoload 'python-mode "python-mode" "Python editing mode." t)



;;
;; recent
;;
(when running-gnu-emacs
  (setq-default
   recentf-max-saved-items 500
   recentf-max-menu-items 60
   recentf-menu-path nil
   recentf-menu-title "Recent"
   backup-inhibited nil
   delete-old-versions t
   version-control t
   indent-tabs-mode nil)   
  (require 'recentf)
  (recentf-mode 1))





;;
;; fix indentation and some other stuff
;;
;; (add-hook
;; 'c-mode-common-hook
;; (lambda ()
;;    (setq c-basic-offset 3
;;          c-comment-only-line-offset '(0 . 0)
;;          c-block-comments-indent-p nil
;;          c-recognize-knr-p nil
;;          )
;;    (define-key c-mode-base-map "\C-m" 'newline-and-indent)
;;    ;(c-set-offset 'label -1000)
;;    (c-set-offset 'substatement-open 0)
;;    (c-set-offset 'access-label -2)
;;    (c-set-offset 'func-decl-cont 0)
;;    (c-set-offset 'arglist-intro '++)
;;    (c-set-offset 'arglist-close '+)
;;    (c-set-offset 'block-open '+)
;;    (c-set-offset 'case-label '+)
;;    (c-set-offset 'statement-case-open 0)
;;    '(lambda () (c-toggle-auto-hungry-state 1))
;;    '(lambda () (c-toggle-auto-newline-state 1))
;;    ))


;;
;; Printing
;;
(setq ps-printer-name t)
(require 'ps-print)
(setq ps-lpr-command "F:/devtools(x86)/Ghostgum/gsview/gsprint.exe")
;; THis line causes ghostscript to query which printer to
;; use - which you may not need if, for example, you only
;; have one printer.
(setq ps-lpr-switches '("-query"))
(custom-set-variables
   ;'(blink-cursor nil)
   '(ps-header-font-size 6)
   '(ps-line-number t)
   '(ps-header-title-font-size '(6 . 6))
   '(ps-header-offset 10)
   '(ps-header-line-pad 0.10)
   ;'(ps-top-margin 2)
   ;'(ps-landscape-mode t)
   ;'(ps-n-up-printing 4)
   ;'(ps-n-up-filling (left-top))
 )

;;
;; strip ^M from compilation mode
;;
(defun my-compilation-mode-hook ()
  (setq truncate-lines nil)
  (font-lock-mode t)) 
(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)
;(add-hook 'compilation-mode-hook 'comint-strip-ctrl-m); generates error: incremental parser error: "Wrong type argument: consp, nil"






(require 'ediff)
;; ;; turn off the gray highlighting
;; (add-hook
;;  'ediff-startup-hook
;;  '(lambda ()
;;     (ediff-toggle-hilit)))

;(setq ediff-keep-variants t)
;; Some custom configuration to ediff
(defvar my-ediff-bwin-config nil "Window configuration before ediff.")
(defcustom my-ediff-bwin-reg ?b
  "*Register to be set up to hold `my-ediff-bwin-config'
    configuration.")

(defun my-ediff-bsh ()
  "Function to be called before any buffers or window setup for
    ediff."
  (remove-hook 'ediff-quit-hook 'ediff-cleanup-mess)
  (window-configuration-to-register my-ediff-bwin-reg))

(defun my-ediff-aswh ()
"setup hook used to remove the `ediff-cleanup-mess' function.  It causes errors."
  (remove-hook 'ediff-quit-hook 'ediff-cleanup-mess)
  ;;(semantic-tag-folding-mode -1)
   )

(defun my-ediff-qh ()
  "Function to be called when ediff quits."
  (remove-hook 'ediff-quit-hook 'ediff-cleanup-mess)
  (ediff-cleanup-mess)
  ;;(semantic-tag-folding-mode -1)
  (jump-to-register my-ediff-bwin-reg))

(add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'my-ediff-aswh);
(add-hook 'ediff-quit-hook 'my-ediff-qh)


;;getting an error when this hook is registered here: Debugger entered--Lisp error: (wrong-type-argument number-or-marker-p nil)
;; so removing this hook.
;(remove-hook 'ediff-quit-hook 'ediff-cleanup-mess)
;(remove-hook 'ediff-quit-hook 'my-ediff-qh)
;(add-hook 'ediff-quit-hook 'ediff-cleanup-mess)





(require 'mic-paren)
(paren-activate)
(custom-set-variables
 '(paren-priority 'both)
 '(paren-highlight-at-point nil))
      

(add-hook 'c-mode-common-hook
          (function (lambda ()
                      (paren-toggle-open-paren-context 1))))


(require 'ansi-color)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


(add-hook 'conf-javaprop-mode-hook 
          '(lambda () (conf-quote-normal nil)))


(require 'smooth-scrolling)
(custom-set-variables
 '(smooth-scroll-margin 7)
 '(scroll-conservatively 10000)
 '(cache-long-line-scans nil))


(custom-set-variables
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 5) ((control))))))






;;
;; load my notes file and my todo file
;; put it last and put point at end of file.
;;
(defvar dropbox-location "f:/Dropbox")
(if is-darwin
    (setq dropbox-location "~/Dropbox"))

(defvar files-to-load 
  nil "files to load at startup" )

(setq files-to-load (list (concat dropbox-location "/.notes." system-name  "/notes") (concat dropbox-location "/.notes." system-name "/todo")))

(dolist (f files-to-load)
  (when (file-exists-p f)
    (find-file-noselect f)
    ;;i don't like that notes.txt is always the most recently opened file.
    (setq filename-history (delete f file-name-history))))





;; use gnuclient on windows 
(when (string= "windows-nt" system-type)
  ;;instead of gnuserv use the emacsclient, will likely work as EDITOR
  ;; environment variable, which would solve "edit" in sqlplus and edit
  ;; commit messages from svn/cvs emacsclient -a runemacs
  ;; c:\\home\\notes.txt requires setting EMACS_SERVER_FILE
  ;;(require 'server)
  ;;(require 'gnuserv)
  ;;(setenv "GNUSERV_SHOW_EMACS" "1"); force gnuserv to show emacs window, always
  ;;(gnuserv-start)
  (message "starting emacs server")
  (server-start)
  )

;; ;; save a list of open files in ~/.emacs.desktop
;; ;; save the desktop file automatically if it already exists
(setq desktop-save 'if-exists)
;;(desktop-save-mode 1)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                ;;(file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                (svn-diff-with-list       . 10)
                ;;tags-file-name
                register-alist)))




(defadvice just-one-space (around delete-whitespace (&optional n) activate compile)
  "redefined the behavior of just-one-space, allow for universal
argument to delete only unneeded whitespace according to major
mode.."
  (interactive "*P")
  (if current-prefix-arg       (delete-whitespace)
    ad-do-it))


(custom-set-variables
 '(display-time-24hr-format t))
(display-time-mode 1)


;;
;; http://www.emacswiki.org/cgi-bin/wiki/ProtectingBuffers
;;
(require 'protbuf)
(protect-buffer-from-kill-mode nil (get-buffer "*scratch*"))
(protect-buffer-from-kill-mode nil (get-buffer "notes"))
(protect-buffer-from-kill-mode nil (get-buffer "*Messages*"))
(setq-default 
 protect-buffer-bury-p  t) 


(define-key occur-mode-map "n" 'next-error-no-select)
(define-key occur-mode-map "p" 'previous-error-no-select)

(if is-emacs-23 
    (progn 
      (define-key grep-mode-map "n" 'next-error-no-select)
      (define-key grep-mode-map "p" 'previous-error-no-select)))

(if (featurep 'nxml-mode)
    (progn 
      (require 'nxml-mode)
      ;;(load "~/.emacs.d/packages/nxml/autostart.el")
      ;;(add-to-list 'magic-mode-alist '("<\\?xml \\(?:.\\|<br/> \\)+?>\\(?:\\|<br/> \\)<!DOCTYPE html " . nxhtml-mode))
      (setq magic-mode-alist
            (cons '("<\\?xml " . nxml-mode)
                  magic-mode-alist))
      (fset 'xml-mode 'nxml-mode)


      (add-to-list 'auto-mode-alist
                   (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
                         'nxml-mode))


(defun my-nxml-mode-hook ()
  (make-local-variable 'tab-width)
  (setq tab-width 2))
(add-hook 'nxml-mode-hook 'my-nxml-mode-hook)
(custom-set-variables 
 '(rng-validate-chunk-size 4000)
 '(rng-validate-quick-delay 1)
 '(rng-validate-delay 2))

(fset 'html-mode 'nxml-mode)

(require 'rng-nxml)
(add-to-list 'rng-schema-locating-files (expand-file-name "~/.emacs.x/.emacs.p/schemas/schemas.xml"))
))



      
;;; find file at point
(require 'ffap)
;; rebind C-x C-f and others to the ffap bindings (see variable ffap-bindings)
(ffap-bindings)
;; C-u C-x C-f finds the file at point
(setq ffap-require-prefix t)
;; browse urls at point via w3m
(setq ffap-url-fetcher 'w3m-browse-url)

;;http://www.emacswiki.org/cgi-bin/wiki/InfoPlus
;;(require 'info+)

;;http://www.emacswiki.org/cgi-bin/wiki/thingatpt%2b.el
(require 'thingatpt+)


(setq skeleton-pair t)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)


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


(setq completion-show-help  nil)
(setq comint-completion-fignore
      (list "~" "CVS"))


;;;###autoload
(defun new-notes-day() 
  "write a new notes entry, using ^L and a date"
  (interactive)
  (let ((buf (get-buffer "notes")))
    (with-current-buffer buf
      (goto-char (point-max))
      ;;(princ "\n\n" buf)
      (insert "\n* " (format-time-string "%A %Y-%m-%d %T%z") "\n"))))
(defalias 'nnd 'new-notes-day)



(substitute-key-definition 'eval-last-sexp
                           'pp-eval-last-sexp global-map)
(substitute-key-definition 'eval-expression
                           'pp-eval-expression global-map)


;; (require 'isearch+)

(custom-set-faces
   '(buffer-menu-read-only-mark ((t (:foreground "burlywood" )))))

;; (add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
;; (defun my-goto-match-beginning ()
;;   (when isearch-forward (goto-char isearch-other-end)))

;; (defadvice isearch-exit (after my-goto-match-beginning activate)
;;   "Go to beginning of match."
;;   (when isearch-forward (goto-char isearch-other-end)))



(require 'mouse+)

(global-set-key [down-mouse-2]        'mouse-flash-position-or-M-x)
(global-set-key [S-down-mouse-2]      'mouse-scan-lines-or-M-:)
(global-set-key [mode-line C-mouse-1] 'mouse-tear-off-window)


(require 'thingatpt+)
(require 'thing-cmds)
(global-set-key [(control meta ? )] 'mark-thing) ; vs `mark-sexp'
(global-set-key [(meta ?@)] 'cycle-thing-region) ; vs `mark-word'


;;(require 'highlight-current-line)
;;(highlight-current-line-mode 1)

;;(require 'sunrise-commander)


(if is-emacs-23 (and (not running-emacs-on-cygwin))
    (progn 
      ;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/jdibug"))
      (require 'jdibug)
      (custom-set-variables 
       '(jdibug-connect-host "localhost")
       '(jdibug-connect-port "6001"))))
      

;; (require 'second-sel)
;; (require 'menu-bar+)


;; ;;(setq inferior-lisp-program "/usr/bin/clisp")
;; (setq inferior-lisp-program "f:/devtools/clisp-2.43/clisp.exe"
;;       lisp-ident-function 'common-lisp-indent-function
;;       slime-complete-symbol-function 'slime-fuzzy-complete-symbol
;;       slime-startup-animation nil)

;; (add-to-list 'load-path "~/.emacs.d/packages/slime-2.0")
;; (require 'slime)
;; (slime-setup)

;; (add-hook 'lisp-mode-hook (lambda() (slime-mode t)))
;; (add-hook 'inferior-lisp-mode-hook (lambda() (inferior-slime-mode t)))




;; (require 'yasnippet) ;; not yasnippet-bundle
;; (yas/initialize)
;; (yas/load-directory "~/.emacs.d/packages/snippets")


;; somehow this value is getting set and not including the default
;; info directory of emacs, set to nil so that
;; Info-default-directory-list is used instead
;;(require 'info)
;;(setq Info-directory-list nil)
;;(add-to-list Info-default-directory-list "~/dev/cedet/semantic/wisent")

;;(require 'w32-browser)
(require 'files+)
(require 'ls-lisp+)
;;(require 'dired+)

(require 'mb-depth+)



;; (defun custom-goto-match-beginning ()
;;   "Use with isearch hook to end search at first char of match."
;;   (when isearch-forward
;;     (unless (null isearch-other-end)
;;       (goto-char isearch-other-end))))

;; ;;
;; ;; Always end searches at the beginning of the matching expression.
;; ;;
;; (add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)

;;http://www.emacswiki.org/cgi-bin/wiki/SebastienRoccaSerraKeyBindings
(defun move-line (&optional n)
  "Move current line N (1) lines up/down leaving point in place."
  (interactive "p")
  (when (null n)
    (setq n 1))
  (let ((col (current-column)))
    (beginning-of-line)
    (forward-line)
    (transpose-lines n)
    (forward-line -1)
    (forward-char col))
  (indent-according-to-mode))

(defun move-line-up (n)
  "Moves current line N (1) lines up leaving point in place."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Moves current line N (1) lines down leaving point in place."
  (interactive "p")
  (move-line (if (null n) 1 n)))


;; my eclipse CTRL+ALT+G replacement
(defun grep-selected (start end)
  (interactive "r") 
  (grep (concat "grep -nH -e \"" 
                (buffer-substring start end)
                "\" * .*")))
(global-set-key "\C-\M-g" 'grep-selected)



(defun edit-dot-emacs ()
  "Load the .emacs file into a buffer for editing."
  (interactive)
  (find-file "~/.emacs"))


(defun my-terminal-mode ()
  (interactive)
  (ansi-term "zsh"))



(require 'parenface)

(add-hook 'sql-interactive-mode-hook (paren-face-add-support sql-mode-oracle-font-lock-keywords))

;;(require 'highlight-parentheses)


;;(require 'auto-install)
;;(setq auto-install-directory "~/.emacs.d/auto-install/")
;;(require 'anything-auto-install)



;;(require 'auto-complete)
;;(global-auto-complete-mode t)


(mouse-avoidance-mode 'proteus)  ;; same + change the pointer shape

(custom-set-variables 
 '(blink-cursor-delay 15.5))
(blink-cursor-mode 1)

;;
;; Emacs Code Browser ( ECB )
;;
(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/ecb-snap"))
(require 'ecb-autoloads)
;;(ecb-activate)


(if (boundp 'global-visible-mark-mode)
    (progn
      (global-visible-mark-mode 1)
      (custom-set-faces
       '(visible-mark-face ((t (:underline "blue" )))))))

;;
;; 
;;
(require 'pmd)
(custom-set-variables 
 '(pmd-java-home (concat  dmb-java-16-home "/bin/java"))
 '(pmd-home (*emacs ".emacs.x/.emacs.p/pmd-4.2.5" ))
 '(pmd-ruleset-list (list "basic" "braces" "clone" "codesize" "controversial" "coupling" 
                                  "design" "finalizers" "imports" "javabeans" "junit" "logging-java" 
                                  "naming" "optimizations" "strictexception" "strings" "sunsecure" 
                                  "unusedcode" "logging-jakarta-commons")))

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))


(custom-set-variables 
 '(gradle-home "f:/devtools/gradle-0.6.1")
 '(gradle-command "gradle"))

(require 'modeline-posn)
(column-number-mode 1)
(size-indication-mode 1)




(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/scala-mode"))
(require 'scala-mode-auto)


;;(require 'xpath)

(defalias 'oracle 'sql-oracle)

;;transparent emacs
;; (set-frame-parameter (selected-frame) 'alpha '(85 50))
;; (add-to-list 'default-frame-alist '(alpha 87 50))

;;(set-frame-parameter (selected-frame) 'alpha '(85 50))
;;(add-to-list 'default-frame-alist '(alpha 85 50))


;; 
;; toggle transparency
;;
;; (eval-when-compile (require 'cl))
;;  (defun toggle-transparency ()
;;    (interactive)
;;    (if (/=
;;         (cadr (find 'alpha (frame-parameters nil) :key #'car))
;;         100)
;;        (set-frame-parameter nil 'alpha '(100 100))
;;      (set-frame-parameter nil 'alpha '(85 60))))
;;  (global-set-key (kbd "C-c t") 'toggle-transparency)  


(eval-when-compile (require 'cl))

(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (find 'alpha (frame-parameters nil) :key #'car))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(85 60))))

(defun transparency-on ()
  (interactive)
  (set-frame-parameter nil 'alpha '(85 60)))

(defun transparency-off ()
  (interactive)
  (set-frame-parameter nil 'alpha '(100 100)))


;;(set-frame-parameter (selected-frame) 'alpha '(96 96))
;;(add-to-list 'default-frame-alist '(alpha 96 96))

(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))

;;
;; work around issues with subversion status -v
;;  
;;
(if (not is-darwin)
    (setenv "LANG" "en_US.utf8"))



;;git clone git://jblevins.org/git/markdown-mode.git
;; markdown mode

(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/markdown-mode"))
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.text" . markdown-mode) auto-mode-alist))



;; work around some path startup problems on mac osx
;;
(setenv "PATH" (concat (getenv "PATH") ":/usr/bin"))

;; setup highlight current line mode, the one I wrote, it's faster than the hiline mode
;;
(load-library "highlight-current-line")



;;
;; setup the awesome ack program
;;
(add-to-list 'load-path "~/Dropbox/.emacs.x/.emacs.p/full-ack")

(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)
(setq ack-executable "~/bin/ack")
(custom-set-variables
 '(ack-prompt-for-directory "Prompt"))



;;
;; setup 'buffer show'
;;
(require 'bs)
(add-to-list 'bs-configurations
             '("files-and-shell" "^\\*shell\\*$" nil nil bs-visits-non-file
              bs-sort-buffer-interns-are-last))

(custom-set-variables '(bs-default-configuration "files-and-shell"))


(require 'mercurial)



;; ;; adjust this path:
;; (add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/emacs-jabber"))
;; (require 'jabber-autoloads)

;; (setq jabber-account-list
;;     '(("david.boon@gmail.com" 
;;        (:network-server . "talk.google.com")
;;        (:connection-type . ssl))
;;       ("david.boon@matrixinsights.com" 
;;        (:network-server . "talk.google.com")
;;        (:connection-type . ssl)))
;;     jabber-activity-count-in-title t
;;     jabber-vcard-avatars-retrieve nil
;;     jabber-chat-buffer-show-avatar nil)


;; ;;flyspell-mode to jabber-chat-mode-hook.
;; ;;jabber-keepalive-start



(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/less-css-mode"))
(setq auto-mode-alist (cons '("\\.less" . less-css-mode) auto-mode-alist))
(add-hook 'css-mode-hook (lambda() 
                           (custom-set-variables '(css-indent-offset 2))))

(require 'flymake)


(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/expand-region.el"))
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/yasnipet"))
(require 'yasnippet)
(setq yas/snippet-dirs (*emacs ".emacs.x/snippets"))
(setq yas/wrap-around-region t)
(setq yas/prompt-functions '(yas/ido-prompt yas/completing-prompt))
;;(yas-global-mode 1)
(yas/reload-all)
(add-hook 'js2-mode-hook
         '(lambda ()
            (yas/minor-mode)))




