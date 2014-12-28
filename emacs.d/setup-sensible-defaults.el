
;;
;; Replace "yes or no" with y or n
;;
(defun yes-or-no-p (arg)
  "An alias for y-or-n-p, because I hate having to type 'yes' or 'no'."
  (y-or-n-p arg))

(require 'saveplace)

(defalias 'qrr 'query-replace-regexp)


(require 'modeline-posn)
(column-number-mode 1)
(size-indication-mode 1)

(require 'show-point-mode)
(show-point-mode 1)
(show-paren-mode 1)



;; to install gnu ls and other commands, the +with_default_names makes ls instead of gls
;; sudo port install coreutils +with_default_names
(custom-set-variables 
 '(dired-listing-switches "-ghG"))

;;
;; open some buffers into the same window, don't pop open new stuff...
;; 
(add-hook 'same-window-regexps "\\*info\\*\\(\\|<[0-9]+>\\)")


;; this is a hack...without it transient makr mode stays on
;(transient-mark-mode 1) ;(transient-mark-mode 1)
;(setq mark-even-if-inactive nil)
;(delete-selection-mode) ;; also known as pending-delete-mode
;(transient-mark-mode 1)
;;(setq mark-even-if-inactive t)
;; end hack

(defvar devtools-dir 
  "f:/devtools" 
  "a directory of development tools")

(defvar cygwin-dir
  "f:/cygwin" 
  "the directory where cygwin is installed")
(if running-emacs-on-cygwin
    (setq cygwin-dir "/cygdrive/f/cygwin"))

(defvar dmb-java-15-home 
  "c:/devtools/jdk1.5.0_14" 
  "java home directory of java 1.5")

(defvar dmb-java-16-home 
  "f:/devtools/jdk1.6.0_11" 
  "java home directory of java 1.6")

(defvar dmb-java-home 
  dmb-java-16-home
  "java default java home")


(when is-darwin 
  (progn
    (setq dmb-java-16-home "/usr")
    (setq dmb-java-home dmb-java-16-home)))

;;(setenv "PATH" (concat dmb-java-home "/bin" ";" (getenv "PATH")))
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

(when running-emacs-on-cygwin
    (defalias 'turn-on-font-lock-if-enabled 'turn-on-font-lock-if-desired))

;; 
;; I like to have the mark activate explicity when I double C-SPC
;; otherwise stay out of my way.
;;

(pending-delete-mode 1)
(transient-mark-mode -1)
(blink-cursor-mode -1)

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)



(if (fboundp 'savehist-mode)
    (savehist-mode 1))
(which-function-mode 1)

(when (boundp 'mouse-1-click-follows-link) ; Do not use mouse-1 to follow links.
  (setq mouse-1-click-follows-link nil))   ; Other values to consider: 100, `double'.
                                           ; The default is bad.

;; removed at emacs 24.2.91
;;(mouse-sel-mode -1)      

;;
;; allow moving between buffers using M-up M-down M-right M-left
;;
(if (not running-emacs-on-cygwin)
    (windmove-default-keybindings 'meta))


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
 '(org-cycle-global-at-bob t)
 '(indent-tabs-mode nil))


;;counteract what is done in info+, add back same window....
(add-hook 'same-window-regexps "\\*info\\*\\(\\|<[0-9]+>\\)")


(setq
 frame-title-format "%b %+%+ %f"
 icon-title-format  "%b"
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
 show-paren-style 'parenthesis
 ;;show-paren-style 'mixed
 ;;show-paren-style 'expression
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
  user-full-name "David M. Boon"
 user-mail-address "david.boon@gmail.com"
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
 
 )


(custom-set-variables
 '(Buffer-menu-buffer+size-width 40)
 '(truncate-lines 1)
 '(backup-inhibited t))

 ;; Don't open files from the workspace in a new frame
  (setq ns-pop-up-frames nil)

;; (setq mac-option-key-is-meta nil
;; mac-command-key-is-meta t
;; mac-command-modifier 'meta
;; mac-option-modifier 'none)



;;
;; the main change from default mode-line-format here is to remove the "dashes" at the end of the modeline.
;;
(custom-set-variables
 '(mode-line-format '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
                     (vc-mode vc-mode)
                     "  " mode-line-modes mode-line-misc-info)))



;; auto insert parens
(electric-pair-mode -1)


;; ringing all the time, please stop, seriously on ctrl-G?
;; http://www.emacswiki.org/emacs/AlarmBell
;; (defun my-bell-function ()
;;   (unless (memq this-command
;;     	'(isearch-abort abort-recursive-edit exit-minibuffer
;;               keyboard-quit mwheel-scroll down up next-line previous-line
;;               backward-char forward-char))
;;     (ding)))
;; (setq ring-bell-function 'my-bell-function)
(setq ring-bell-function 'ignore)


(iswitchb-mode 1)

;; setup sensible window splitting defaults for more info see
;; `split-window-sensibly the following two settings cause
;; split-window-sensibly to prefer splitting horizontially and avoid
;; vertical splits.  By setting both to nil, there will always be a
;; horizontal split, for help, info, grep, pretty much everything
;; etc.
(custom-set-variables
 '(split-height-threshold nil)
 '(split-width-threshold 450))

(provide 'setup-sensible-defaults)



