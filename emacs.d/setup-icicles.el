;;(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/icicles"))
;;(autoload 'icicle-mode "icicles" "icicle mode." t)

(require 'icicles)

;;(require 'icicles-iswitchb)

;;; I prefer modal cycling.
;;(setq icicle-cycling-respects-completion-mode-flag nil)
;;  I HATE arrow keys.
;; (setq icicle-modal-cycle-up-key "\C-p")
;; (setq icicle-modal-cycle-down-key "\C-n")
;;icicle-next-apropos-candidate
;;icicle-previous-apropos-candidate


;; (custom-set-variables  
;;  '(icicle-control-reminder-prompt nil)
;;  '(icicle-require-match-flag 'no-match-required) ;;'partial-match-ok)
;;  '(icicle-buffer-require-match-flag  'no-match-required)
;;  '(icicle-test-for-remote-files-flag nil)
;;  '(icicle-download-dir "~/emacs/packages/icicles")
;;  '(icicle-prompt-suffix "")
;;  '(icicle-buffer-no-match-regexp "^\\*Completions\\|Ibuffer\\|Pp Eval Output\\|Process List\\*$")
;;  ;;work around bug in icicles+emacs 23
;;  '(icicle-generic-S-tab-keys '([S-tab] [S-iso-lefttab]))
;;  ;;icicle-Completions-window-max-height
;;  '(icicle-search-cleanup-flag nil) ;; remove manually using icicle-search-hightlight-cleanup C-.
;;  '(icicle-Completions-frame-at-right-flag nil)
;;  '(insert-default-directory t) ;; remove the default directory from the minibuffer when opening a file.
;;  '(icicle-incremental-completion-flag nil)
;;  '(icicle-show-Completions-initially-flag nil)
;; )


(custom-set-faces
 '(diredp-compressed-file-suffix ((t (:foreground "goldenrod" ))))
 ;;  '(diredp-date-time)
 ;;  '(diredp-deletion)
 ;;  '(diredp-deletion-file-name)
 '(diredp-dir-heading ((t (:foreground "goldenrod" ))))
 '(diredp-dir-priv ((t (:foreground "blue4" ))))
 ;;  '(diredp-display-msg)
 '(diredp-exec-priv ((t (:foreground "DarkGreen" ))))
 ;;  '(diredp-executable-tag)
 '(diredp-file-name ((t (:foreground "black" ))))
 ;;  '(diredp-file-suffix)
 ;;  '(diredp-flag-mark)
 ;;  '(diredp-flag-mark-line)
 ;;  '(diredp-ignored-file-name)
 ;;  '(diredp-link-priv)
 '(diredp-no-priv ((t (:foreground "black" ))))
  
 '(diredp-other-priv ((t (:foreground "blue4" ))))
 ;;  '(diredp-rare-priv)
 '(diredp-read-priv ((t (:foreground "blue4" ))))
 ;;  '(diredp-symlink)
 '(diredp-write-priv ((t (:foreground "blue4" )))))

;; for some reason, turning on icy-mode here, causes 
;; anything mode to fail, so I turn it on very last, 
;; after anything(see ~/.emacs)
;;(icicle-mode 1)                         ; Turn on Icicle mode.


;;(require 'isearch+)
;;(custom-set-variables
;; '(isearchp-set-region-flag t))

;;(require 'replace+)
;;(custom-set-variables
;; '(replace-w-completion-flag nil))
;;(global-set-key "\M-%" 'query-replace-w-options)
;;(global-set-key "\M-%" 'query-replace)

;;for some reason my S-TAB is not invoking apropos completion
;; trying this off for now:
;;;(define-key minibuffer-local-completion-map  [S-tab] 'icicle-apropos-complete)


(icicle-mode 1)

(provide 'setup-icicles)
