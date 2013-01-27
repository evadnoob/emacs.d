

(setq default-frame-alist
      '(
        ;;(cursor-type bar . 2)
        (cursor-type bar . 3)
        ;;(cursor-type bar . 4)
        ;;(cursor-type bar . 7)
        ;;(cursor-type bar . 6)
        ;;(cursor-type hbar . 2)
        ;;(cursor-type bar . 2)
        ;;(cursor-type bar . 6)
        ;;(cursor-type bar . 8)
        ;; (top . 0) (left . 150) 
        ;;(width . 85) (height . 63)
        ;;(cursor-color . "light slate blue")
        (cursor-color . "blue")
        ;;(cursor-color . "red4")
        ;;(cursor-color . "#7706F6")
        ;;(cursor-color . "deep sky blue")
        (left-fringe . 0)
        (right-fringe . 0)
        
        ;;  (blink-cursor-delay 0)
        (menu-bar-lines  .  0)
        (scroll-bar-width  .  0)
        (vertical-scroll-bars  .  nil)
        (internal-border-width  .  0)

        ;;(font . "-*-Lucida Console-normal-r-*-*-11-82-96-96-c-*-iso8859-1")))
        ;;(font . "-outline-Bitstream Vera Sans Mono-normal-r-normal-normal-11-82-96-96-c-*-iso8859-1")))
        ;;(font . "-outline-Bitstream Vera Sans Mono-normal-r-*-*-11-82-96-96-c-*-iso8859-1")))
))
(setq initial-frame-alist default-frame-alist)



(defconst running-xemacs (string-match "XEmacs\\|Lucid" (emacs-version)))
(defconst running-gnu-emacs (string-match "GNU Emacs" (emacs-version)))
(defconst running-gnu-emacs-on-linux (and running-gnu-emacs (string= "gnu/linux" system-type)))


(defvar devtools-dir 
  "f:/devtools" 
  "a directory of development tools")

(defvar cygwin-dir
  "c:/cygwin" 
  "the directory where cygwin is installed")

(defvar dmb-java-15-home 
  "c:/devtools/jdk1.5.0_14" 
  "java home directory of java 1.5")

(defvar dmb-java-16-home 
  "c:/devtools/jdk1.6.0_03" 
  "java home directory of java 1.6")

(defvar dmb-java-home 
  dmb-java-15-home
  "java default java home")

(setenv "PATH" (concat dmb-java-home "/bin" ";" (getenv "PATH")))


(defalias 'turn-on-font-lock-if-enabled 'turn-on-font-lock-if-desired
  "work around issue in jdee 2.3.5.1, uses function deprecated/removed ")


(setq
;;  ;blink-cursor-interval 0.40
;;  ;blink-cursor-delay 0.15
;;  delete-key-deletes-forward t
;;  comint-buffer-maximum-size 200000
;;  frame-title-format "emacs %b %+%+  %f"
  ;;icon-title-format "%b"
  ;;frame-title-format "emacs [%b %+%+  %f]"
  ;;icon-title-format "%b"
;;  icon-title-format "%b %+%+  %f"
 ;; frame-title-format "%b %+%+  %f"
  ;;frame-title-format "%f %+%+"
  ;;frame-title-format "%b %f"
;;  icon-title-format  "%b"
 frame-title-format "%b %+%+"
 icon-title-format  "%b"
 ;;frame-title-format "%b %+%+  %f"
 ;;icon-title-format  "%b %f"
;;  system-name ""
;;  make-backup-files nil
;;  scroll-step 1
;;  suggest-key-bindings 5
;;  hscroll-step 1 
;;  inhibit-startup-message t
  inhibit-startup-screen t
;;  next-line-add-newlines nil
  shell-font-lock-keywords nil
  compilation-window-height 25
;;  visible-bell t
  indent-tabs-mode nil
  compilation-scroll-output t
  read-file-name-completion-ignore-case t
;;  default-directory "~/"
;;  track-eol t
;;  ;;archive-zip-use-pkzip nil
;;  bookmark-save-flag 1
  ediff-window-setup-function 'ediff-setup-windows-plain
  semanticdb-default-save-directory "~/.semantic.cache"
;;  semanticdb-global-mode nil
;;  global-semantic-auto-parse-mode -1
;;  global-semantic-show-unmatched-syntax-mode -1
;;  comint-input-ring-size 150
;;  find-args "-name \"*.java\" -exec egrep -q \"\\.jsp\" {} \\;"
;;  global-semantic-show-parser-state-mode nil
;;  ;require-final-newline 't
;;  c-hungry-delete-key t
  classpath ""
  jde-classpath-separator ";"
  jit-lock-stealth-time nil
  ;;tool-bar-mode nil
  diff-switches "-u"
  visible-bell nil
  enable-recursive-minibuffers t
  ;;show-paren-style 'mixed
;;show-paren-style 'parenthesis
)

(setq-default
 ;;tab-width 3
 save-place t
 ;;save-place t nil (saveplace)
 dired-listing-switches "-lhgG"
;; vc-diff-switches "-BwbE"
vc-diff-switches nil
;;  '(minibuffer-prompt ((t (:foreground "blue"))))
;;  '(mode-line ((t (:background "#e1ded9" :foreground "DarkSlateGrey" :box (:line-width -1 :color "grey90" :style released-button) :height 80 :width normal :family "outline-lucida console"))))
;;  '(mode-line-buffer-id ((t (:foreground "#284B73" :slant normal :weight bold :width normal :family "outline-verdana"))))
;;  '(mode-line-inactive ((t (:background "#e1ded9" :foreground "gray" :box (:line-width -1 :color "grey90" :style released-button) :height 80 :width normal :family "outline-lucida console"))))
;;  '(region ((nil (:background "lavender"))))
 mouse-yank-at-point t
 overflow-newline-into-fringe t
 ps-header-font-size 6
 ps-header-line-pad 0.1
 ps-header-offset 10
 ps-header-title-font-size (quote (6 . 6))
 ps-line-number t
 recentf-exclude (quote ("semantic\\.cache" "\\.emacs-places" "^/plink.*"))
 recentf-max-saved-items 200
 scroll-preserve-screen-position t
 ;;show-paren-delay 0.0
 ;;show-paren-mode t
 sql-electric-stuff nil
 sql-input-ring-file-name "~/.sql_history"
 sql-pop-to-buffer-after-send-region t
 transient-mark-mode -1
 user-full-name "David M. Boon"
 user-mail-address "david_boon@elementk.com"
 woman-path (concat devtools-dir "/cygwin/usr/man")
 case-fold-search nil
 column-number-mode t
 comint-input-ring-file-name "~/.bash_history"
 javascript-indent-level 3
 find-ls-option (quote ("-exec ls -ldh {} \\;" . "-ld"))
 ;;fringe-mode 0 nil (fringe)
 grep-command "grep -nrH -e "
 grep-use-null-device nil
 ;;icicle-reminder-prompt-flag 19
 icicle-reminder-prompt-flag nil
 isearch-allow-scroll t
 ispell-program-name "aspell"
 jde-global-classpath (split-string classpath jde-classpath-separator)
 jde-import-blank-line-between-groups t
 jde-import-group-of-rules (quote (("^java?\\." . "A") ("^javax?\\." . "B") ("^junit?\\." . "C") ("^org?\\." . "D") ("^edu?\\." . "E") ("^com\\.google\\." . "F") ("^com\\.google\\." . "G") ("^com\\.gwtext\\." . "H") ("^com\\.thomson\\." . "I")))
 jde-import-sorted-groups (quote gor)
 jde-jdk (quote ("1.5.0"))
 ;;jde-jdk-registry (quote (("1.5.0" . "c:/devtools/jdk1.5.0_14") ("1.6.0" . "c:/devtools/jdk1.6.0_03")))
 ;;jde-mode-line-format (quote (#("-" 0 1 (auto-composed t)) mode-line-mule-info mode-line-modified mode-line-frame-identification mode-line-buffer-identification #(" %I " 0 4 (auto-composed t)) mode-line-position global-mode-string #(" %[" 0 3 (auto-composed t)) "%n" "%]" (vc-mode vc-mode) mode-line-modes (jde-which-method-mode (#(" " 0 1 (auto-composed t)) jde-which-method-format))))
 jde-which-method-max-length 150
 ;;mode-line-format (quote ("%e" mode-line-mule-info mode-line-modified mode-line-frame-identification mode-line-buffer-identification #(" %I " 0 4 (auto-composed t)) mode-line-position (vc-mode vc-mode) #(" " 0 1 (auto-composed t)) mode-line-modes #(" " 0 1 (auto-composed t)) (which-func-mode ("" which-func-format)) (global-mode-string "" global-mode-string))))
 mode-line-format 
 (quote 
  ("%e" 
   #("-" 0 1 (auto-composed t help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))
   mode-line-mule-info
   " " (:propertize default-directory face dmb-mode-line-directory-face) ""
   ;;#("     " 0 5 (auto-composed t help-echo " " (:propertize default-directory face info-node) ""))
   mode-line-buffer-identification 
;;   mode-line-frame-identification 
   mode-line-modified
   #(" " 0 1 (auto-composed t help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0")) 
   mode-line-position
;;    (:eval (vc-icon))
   (vc-mode vc-mode) 

   #("  " 0 2 (auto-composed t help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))
   mode-line-modes 
   (which-func-mode ("" which-func-format #("  " 0 2 (auto-composed t help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0")))) 
   (global-mode-string (#("  " 0 2 (auto-composed t help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0")) global-mode-string))))
 jde-mode-line-format (quote (#("-" 0 1
                                (auto-composed t))
                              mode-line-mule-info
                              ;;" " (:propertize default-directory dmb-mode-line-directory-face) ""
                              ;;mode-line-frame-identification 
                              mode-line-buffer-identification
                              mode-line-modified
                              
 #(" " 0 1
   (auto-composed t))
 mode-line-position 
 ;;(:eval (vc-icon))
 (vc-mode vc-mode) 
 #(" " 0 1
   (auto-composed t))
 global-mode-string
 #("   %[(" 0 6
   (auto-composed t))
 mode-name mode-line-process minor-mode-alist "%n"
 #(")%]" 0 3
   (auto-composed t))
 (jde-which-method-mode
  (#(" " 0 1
     (auto-composed t))
   jde-which-method-format
   #(" " 0 1
     (auto-composed t))))
 #("" 0 0
   (auto-composed t)))
 )
)

(custom-set-variables
 '(  compilation-skip-threshold 2 )
 ;;most people probably want this on, I like to have it off
 '(completion-auto-help nil)
 '(partial-completion-mode nil)
 '(icomplete-mode t)
 '(initial-buffer-choice t)
 '(yank-pop-change-selection t)
 '(color-theme-is-cumulative t)
 '(calc-display-trail nil)  ;;turn back on with t d
 )


;;run for ntemacs, but not xemacs
(when running-gnu-emacs 
  (set-scroll-bar-mode nil)
  ;;(hscroll-global-mode 1)
  ;;(show-paren-mode 1)
  ;(global-font-lock-mode t)
  ;;(message (format "%s" (emacs-version)))
  (ding)
  ;;(transient-mark-mode nil)
  (fringe-mode 0)
  )

;;
;; make copy/paste work intuitively with X
;;
(when running-gnu-emacs-on-linux
  (message (format "running emacs on %s" system-type))
    (setq x-select-enable-clipboard t)
    (menu-bar-enable-clipboard))


(setq-default
 iswitchb-buffer-ignore (quote ("^ " "\\*buffer-selection\\*" "\\*Completions\\*" "^TAGS" "\\*JDEE bsh\\*" "\\*Ediff Registry\\*" "\\*Help\\*" "^\\*Customize" "\\*jde-log\\*"))
 iswitchb-max-to-show 5
 iswitchb-newbuffer t
 iswitchb-prompt-newbuffer nil
 iswitchb-regexp t
 iswitchb-case t
 ibuffer-saved-filter-groups (quote (("dave-default" ("java" (mode . jde-mode)) ("xml" (mode . sgml-mode)) ("python" (mode . python-mode)) ("elisp" (mode . emacs-lisp-mode)) ))))

(setq ibuffer-formats 
      '(
        (mark modified read-only " "
              (name 35 18 :left :elide)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark modified read-only " "
              (name 35 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)

        (mark " "
              (name 30 -1)
              " " filename)))

(iswitchb-mode 1)
(blink-cursor-mode -1)
(column-number-mode 1)
;;(pending-delete-mode 1)
(delete-selection-mode 1)
(transient-mark-mode -1)

(if (boundp 'tooltip-mode 
     tooltip-mode -1))

(prefer-coding-system 'utf-8)


 
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)




 ;; kill-ring-save and kill-region defadvice hacks
    
 ;; Implement slickedit's default copy and cut behavior.
 ;; If region is inactive, copy and cut will operate on the current line.
 ;; (As if the current line is currently selected.)
 ;;
 ;; Please send comments to emailmac (at) gmail com
 ;;
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy 
 a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position) 
           (line-end-position 1)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill 
 a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position) 
           (line-end-position)))))





(defun count-buffer-by-mode (p-mode)
  "Count the number of buffers that match a mode"
  (setq buffer-matches -1)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      ;(princ (format "%s %s " (buffer-name buffer) mode-name))
      (if (and (not (string-match "\*etags -.*$" (buffer-name buffer))) (string-match p-mode mode-name))
          (setq buffer-matches (1+ buffer-matches)))))           ;(princ (format "adding buffer %s to buffer-matches new size %s " (buffer-name buffer) buffer-matches))     (princ "\n"))
  ;(princ (format "buffer matches: %s\n" buffer-matches))
  (1+ buffer-matches))



(defun mark-line ()
 "when `transient-mark-mode', push mark and highlight line. "
 (interactive)
 (if (not (and transient-mark-mode))
      (error "transient mark not available"))

 (push-mark (line-beginning-position) t t)
 (forward-line 1)
 ;;(goto-char (line-end-position)))
 (goto-char (line-beginning-position)))
 
(global-set-key "\C-cl" 'mark-line)


(if (functionp 'global-hi-lock-mode)
    (global-hi-lock-mode 1)
  (hi-lock-mode 1))



(savehist-mode 1)

