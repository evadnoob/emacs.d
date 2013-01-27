(when is-darwin
  (setq default-frame-alist
        '(
          ;;(font . "-apple-DejaVu_Sans_Mono-medium-normal-normal-*-10-*-*-*-m-0-iso10646-1")
          ;;(font . "-apple-Consolas-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1")
          ;;(font . "-apple-Menlo-medium-normal-normal-*-10-*-*-*-m-0-iso10646-1")
	  (font . "-apple-Monaco-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1")
          ;;(cursor-type bar . 3)
          ;;(cursor-type bar . 8)
          ;;(cursor-color . "#1E16ED")
          (left-fringe . 0)
          (right-fringe . 0)
          (menu-bar-lines  .  0)
          (scroll-bar-width  .  0)
          (vertical-scroll-bars  . nil)
          (internal-border-width  .  1))))

(when (not is-darwin)
  (setq default-frame-alist
        '(
          ;;(font . "-outline-Consolas-normal-normal-normal-mono-11-*-*-*-c-*-iso8859-1")
	  ;;(cursor-type bar . 8)
          (cursor-type bar . 3)
          (cursor-color . "blue")
          (left-fringe . 0)
          (right-fringe . 0)
          (menu-bar-lines  .  0)
          (scroll-bar-width  .  0)
          (vertical-scroll-bars  .  nil)
          (internal-border-width  .  0))))


;; (setq default-frame-alist
;;       '(
;;         ;;(cursor-type bar . 2)
;;         ;;(cursor-type bar . 3)
;;         ;;(cursor-type bar . 4)
;;         ;;(cursor-type bar . 5)
;;         ;;(cursor-type bar . 7)
;;         ;;(cursor-type bar . 6)
;;         ;;(cursor-type hbar . 2)
;;         ;;(cursor-type bar . 2)
;;         ;;(cursor-type bar . 6)
;;         ;;(cursor-type bar . 8)
;;         ;; (top . 0) (left . 150) 
;;         ;;(width . 85) (height . 63)
;;         ;;(cursor-color . "light slate blue")
;;         ;;(cursor-color . "blue")
;;         ;;(cursor-color . "orange3")
;;         ;;(cursor-color . "red4")
;;         ;;(cursor-color . "orange3")
;;         ;;(cursor-color . "green3")
;;         ;;(cursor-color . "red4")
;;         ;;(cursor-color . "yellow3")
;;         (cursor-color . "red3")
;;         ;;(cursor-color . "black")
;;         ;;(cursor-color . "yellow green")
;;         ;;(cursor-color . "#7706F6")
;;         ;;(cursor-color . "deep sky blue")
;;         (left-fringe . 0)
;;         (right-fringe . 0)
;;         ;;(icon-type        . t)      ; gnu picture as Emacs icon        
;;         ;;  (blink-cursor-delay 0)
;;         (menu-bar-lines  .  0)
;;         (scroll-bar-width  .  0)
;;         (vertical-scroll-bars  .  nil)
;;         (internal-border-width  .  0)
;;         (font . "-apple-Consolas-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1")
;;         ;;(font . "-outline-Consolas-normal-normal-normal-mono-11-*-*-*-c-*-iso8859-1")
;;         ;;(font . "-*-Lucida Console-normal-r-*-*-11-82-96-96-c-*-iso8859-1")))
;;         ;;(font . "-outline-Bitstream Vera Sans Mono-normal-r-normal-normal-11-82-96-96-c-*-iso8859-1")))
;;         ;;(font . "-outline-Bitstream Vera Sans Mono-normal-r-*-*-11-82-96-96-c-*-iso8859-1")))
;; ))

(setq initial-frame-alist default-frame-alist)

;; make command key be the meta key too
(when is-darwin
  (setq ns-command-modifier (quote meta)))

(setq
  ;;icon-title-format "%b"
  ;;frame-title-format "emacs [%b %+%+  %f]"
  ;;icon-title-format "%b"
 ;; frame-title-format "%b %+%+  %f"
  ;;frame-title-format "%f %+%+"
  ;;frame-title-format "%b %f"
 frame-title-format "%b %+%+ %f"
 icon-title-format  "%b"
 ;;frame-title-format "%b %+%+  %f"
 ;;icon-title-format  "%b %f"
  inhibit-startup-screen t
  initial-scratch-message nil
  shell-font-lock-keywords nil
  compilation-window-height 25
  indent-tabs-mode nil
  compilation-scroll-output t
  read-file-name-completion-ignore-case t
  ediff-window-setup-function 'ediff-setup-windows-plain
  semanticdb-default-save-directory "~/.semantic.cache"
  classpath ""
  jde-classpath-separator ";"
  jit-lock-stealth-time nil
  
  diff-switches "-u"
  visible-bell nil
  enable-recursive-minibuffers t
  ;;show-paren-style 'mixed
)



(custom-set-variables 
 '(jde-which-method-format
   `("[" (:propertize jde-which-method-current face which-func help-echo "mouse-1: go to beginning, mouse-2: toggle rest visibility, mouse-3: go to end"))))

    ;;'("[" (:propertize jde-which-method-current face font-lock-function-name-face) "]")))

(setq-default
 ;;tab-width 3
 save-place t
 ;;save-place t nil (saveplace)
 ;;dired-listing-switches "-ghGBa --ignore=CVS --group-directories-first" 
 ls-lisp-use-insert-directory-program t
 vc-diff-switches nil
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
 user-mail-address "david.boon@gmail.com"
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
 
 ;;mode-line-format (quote ("%e" mode-line-mule-info mode-line-modified mode-line-frame-identification mode-line-buffer-identification #(" %I " 0 4 (auto-composed t)) mode-line-position (vc-mode vc-mode) #(" " 0 1 (auto-composed t)) mode-line-modes #(" " 0 1 (auto-composed t)) (which-func-mode ("" which-func-format)) (global-mode-string "" global-mode-string))))
 mode-line-format 
 (quote 
  ("%e" 
   #("-" 0 1 (auto-composed t help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))
   mode-line-mule-info
   ;;" " (:propertize default-directory face dmb-mode-line-directory-face help-echo default-directory) ""
   ;;" " (:propertize  mode-line-buffer-identification face mode-line-buffer-id help-echo default-directory) ""
   ;;#("     " 0 5 (auto-composed t help-echo " " (:propertize default-directory face info-node) ""))
   mode-line-buffer-identification 
   mode-line-modified
   #(" " 0 1 (auto-composed t help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0")) 
   mode-line-position
   (vc-mode vc-mode) 

   #("  " 0 2 (auto-composed t help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))
   mode-line-modes 
   (which-func-mode ("" which-func-format #("  " 0 2 (auto-composed t help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0")))) 
   (global-mode-string (#("  " 0 2 (auto-composed t help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0")) global-mode-string))))
 jde-mode-line-format (quote (#("-" 0 1
                                (auto-composed t))
                              mode-line-mule-info
                              ;;" " (:propertize default-directory face dmb-mode-line-directory-face) ""
                              ;;#("^" 0 1 (auto-composed t help-echo default-directory))
                              ;;" " (:propertize default-directory dmb-mode-line-directory-face) ""
                              ;;mode-line-frame-identification 
                      ;;;         (#("%12b" (auto-composed t help-echo default-directory)))
                              (:propertize mode-line-buffer-identification help-echo default-directory)
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
   "[" (:propertize jde-which-method-current face which-func) "]"
   #(" " 0 1
     (auto-composed t))))
 #("" 0 0
   (auto-composed t)))
 )
)

(custom-set-variables
 ;;  '(  compilation-skip-threshold 2 )
 ;;  ;;most people probably want this on, I like to have it off
 ;;  '(completion-auto-help nil)
 
;;;; causes issues with 'anything and 'icicles
;;;;'(partial-completion-mode nil)
;;;;   '(icomplete-mode 1)
 '(initial-buffer-choice t)
 '(yank-pop-change-selection t)
 ;;  '(color-theme-is-cumulative t)
 '(track-eol t)
 '(find-file-confirm-nonexistent-file t)
 )


;; (when running-gnu-emacs 
;;   (set-scroll-bar-mode nil)
;;   (fringe-mode 0)  )

;; (when running-gnu-emacs-on-linux
;;   (message (format "running emacs on %s" system-type))
;;     (setq x-select-enable-clipboard t)
;;     (menu-bar-enable-clipboard))


;; (setq-default
;;  iswitchb-buffer-ignore (quote ("^ " "\\*buffer-selection\\*" "\\*Completions\\*" "^TAGS" "\\*JDEE bsh\\*" "\\*Ediff Registry\\*" "\\*Help\\*" "^\\*Customize" "\\*jde-log\\*"))
;;  iswitchb-max-to-show 5
;;  iswitchb-newbuffer t
;;  iswitchb-prompt-newbuffer nil
;;  iswitchb-regexp t
;;  iswitchb-case t

;;(iswitchb-mode 1)
;;(iswitchb-default-keybindings)

(blink-cursor-mode -1)
(column-number-mode 1)
(delete-selection-mode 1)
(transient-mark-mode -1)
(if (featurep 'tooltip-mode) 
    (tooltip-mode -1))
(if (featurep 'tool-bar-mode)
    tool-bar-mode nil)

(prefer-coding-system 'utf-8)

(custom-set-variables 
 '(iswitchb-buffer-ignore (list "^ " "^\\*Completions\\*" "^\\*anything\\*" "^\\*Minibuf-.*" "^\\*Ibuffer\\*" "^\\*JDEE bsh\\*" "^\\*WoMan-Log\\*"  "^\\*JDEE bsh\\*" )))

 
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)




;;  ;; kill-ring-save and kill-region defadvice hacks
    
;;  ;; Implement slickedit's default copy and cut behavior.
;;  ;; If region is inactive, copy and cut will operate on the current line.
;;  ;; (As if the current line is currently selected.)
;;  ;;
;;  ;; Please send comments to emailmac (at) gmail com
;;  ;;
;; (defadvice kill-ring-save (before slickcopy activate compile)
;;   "When called interactively with no active region, copy 
;;  a single line instead."
;;   (interactive
;;    (if mark-active (list (region-beginning) (region-end))
;;      (list (line-beginning-position) 
;;            (line-end-position 1)))))

;; (defadvice kill-region (before slickcut activate compile)
;;   "When called interactively with no active region, kill 
;;  a single line instead."
;;   (interactive
;;    (if mark-active (list (region-beginning) (region-end))
;;      (list (line-beginning-position) 
;;            (line-end-position)))))





;; (defun count-buffer-by-mode (p-mode)
;;   "Count the number of buffers that match a mode"
;;   (setq buffer-matches -1)
;;   (dolist (buffer (buffer-list))
;;     (with-current-buffer buffer
;;       ;(princ (format "%s %s " (buffer-name buffer) mode-name))
;;       (if (and (not (string-match "\*etags -.*$" (buffer-name buffer))) (string-match p-mode mode-name))
;;           (setq buffer-matches (1+ buffer-matches)))))           ;(princ (format "adding buffer %s to buffer-matches new size %s " (buffer-name buffer) buffer-matches))     (princ "\n"))
;;   ;(princ (format "buffer matches: %s\n" buffer-matches))
;;   (1+ buffer-matches))



;; (defun mark-line ()
;;  "when `transient-mark-mode', push mark and highlight line. "
;;  (interactive)
;;  (if (not (and transient-mark-mode))
;;       (error "transient mark not available"))

;;  (push-mark (line-beginning-position) t t)
;;  (forward-line 1)
;;  ;;(goto-char (line-end-position)))
;;  (goto-char (line-beginning-position)))
 
;; (global-set-key "\C-cl" 'mark-line)


;; (if (functionp 'global-hi-lock-mode)
;;     (global-hi-lock-mode 1)
;;   (hi-lock-mode 1))


(if (fboundp 'savehist-mode)
    (savehist-mode 1))
(which-function-mode 1)

(when (boundp 'mouse-1-click-follows-link) ; Do not use mouse-1 to follow links.
  (setq mouse-1-click-follows-link nil))   ; Other values to consider: 100, `double'.
                                        ;    The default is bad.

;; removed at emacs 24.2.91
;;(mouse-sel-mode -1)      

;;
;; allow moving between buffers using M-up M-down M-right M-left
;;
(if (not running-emacs-on-cygwin)
    (windmove-default-keybindings 'meta))


(custom-set-variables
 '(Buffer-menu-buffer+size-width 40))

(if (not running-emacs-on-cygwin)
    (progn (require 'visible-mark)
	    (visible-mark-mode 1)))


;;emacs 23 redefined C-l from recenter to recenter-top-bottom
;; looked to isearch to see how it causes commands to allow scrolling, this fixes it
(put 'recenter-top-bottom 'isearch-scroll t)


(custom-set-variables 
 '(nxhtml-global-minor-mode t)
 '(nxhtml-skip-welcome t)
 '(completion-ignore-case t)
 '(org-cycle-global-at-bob t))


;;counteract what is done in info+, add back same window....
(add-hook 'same-window-regexps "\\*info\\*\\(\\|<[0-9]+>\\)")


;; (setq url-proxy-services '(("no_proxy" . "localhost")
;;                            ("http" . "webproxy.int.westgroup.com:80")))



