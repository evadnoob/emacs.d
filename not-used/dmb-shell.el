
(defvar cycle-buffer-by-mode-ignore-list 
  (list "\*etags -.*$"))

(defun dave-test1 ()
  (if (string-match "Lisp" mode-name)
      (princ (format "match\n"))
    (princ "not lisp")))

(defun cycle-buffer-by-mode (p-mode)
  "Cycle buffers by mode name"
  (interactive "s cycle to buffer matching regexp: ")
  ;(message (format "will look for mode %s\n" p-mode)) ;regexp-quote
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      ;;(princ (format "%s %s\n" (buffer-name buffer) mode-name))i
      ;;(regexp-quote p-mode)
      (when (buffer-live-p buffer)
        (if (and (not (string-match "\*etags -.*$" (buffer-name buffer))) (string-match p-mode mode-name))
            (setq switch2buffer buffer)))))
  (when (boundp 'switch2buffer)
    (switch-to-buffer switch2buffer)))

  ;; special handling for Shell buffers, most likely
  ;; you'll want to be at a usable prompt upon entering buffer.
;;  (if (switch-to-buffer switch2buffer)
;;       (if (string-match "Shell$" mode-name)
;;           (end-of-buffer)))
  ;(princ (format "swithed to buffer %s." switch2buffer)
    

(defun cycle-to-next-buffer ()
  "Cycle to the next buffer in the buffer stack"
  ;(message (format "will look for mode %s\n" p-mode)) ;regexp-quote
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (princ (format "%s %s\n" (buffer-name buffer) mode-name)))))



;; (defun dmb-comint-exec-hook ()
;;   ""
;;   (message (format "comint command: %s %s" proc startfile)))




(defun my-bash-shell-setup ()
  "My shell-setup hook"
  ;; (setq comint-process-echoes t) ;; reported that this is no longer needed
  ;(setq explicit-bash.exe-args '( "-i")) 
  (setq comint-completion-addsuffix t
        ;w32-quote-process-args ?\"
        comint-input-ignoredups t
        comint-eol-on-send t
        ;comint-scroll-to-bottom-on-output 'this
        comint-scroll-to-bottom-on-input 'this
        comint-scroll-show-maximum-output 'this
        comint-input-ignoredups t
        comint-input-ring-size 1000
        comint-input-ring-file-name "~/.zsh_history"
        ;;make-variable-buffer-local comint-input-ring-separator
        comint-input-ring-separator "\n"
        comint-completion-autolist nil
        ;;comint-move-point-for-output 'this
        )
  ;;(message (format "coming-input-ring-file-name %s" comint-input-ring-file-name))

  ;;(add-hook 'pre-command-hook 'dmb-comint-exec-hook)
  (comint-read-input-ring)
  (define-key comint-mode-map [delete] 'delete-char)
  ;;(define-key shell-mode-map "\C-d" 
  ;;  '(lambda () 
  ;;    (interactive)
  ;;     (comint-simple-send (get-buffer-process (current-buffer)) "logout")))

  
  (make-variable-buffer-local 'comint-completion-addsuffix)
  ;;(highlight-lines-matching-regexp "^test.*started\.$" 'hi-blue-b)
  ;;(highlight-lines-matching-regexp "CategoryCache.loadOneOrAllCattegories" 'hi-blue-b)
  ;;(highlight-lines-matching-regexp "^[0-9]\{4\}\.[0-9]\{1,2\}\.[0-9]\{1,2\}.*ERROR" 'dmb-face-hi-light-line-error)
  
  ;;(shell-resync-dirs)

  )

(setq explicit-shell-args '("--login" "-i"))
(setq explicit-bash.exe-args '("--login" "-i")) 
(setq shell-file-name (concat cygwin-dir "/bin/zsh.exe")) ;;needed for inferior shells
(setq explicit-shell-file-name (concat cygwin-dir "/bin/zsh.exe")) 
(setenv "BASH_ENV" "$HOME/.bash_profile_for_emacs")
(setenv "COLUMNS" "120")
(add-hook 'shell-mode-hook 'my-bash-shell-setup)


;; a function to run windows cmd shell
;; (defun win32cmd ()
;;   ""
;;   (interactive)
;;   (remove-hook 'shell-mode-hook 'my-bash-shell-setup)
;;   (setq explicit-shell-args nil)
;;   (setq shell-file-name "e:/devtools/emacs-22.0/bin/cmdproxy.exe") ;;needed for inferior shells
;;   (setq explicit-shell-file-name shell-file-name)
;;   (shell))

;; (defun bash ()
;;   ""
;;   (interactive)

;;   (setq explicit-shell-args '("--login" "-i"))
;;   (setq explicit-bash.exe-args '("--login" "-i")) 
;;   (setenv "PATH" (concat "e:\\devtools\\cygwin\\bin;c:\\devtools\\cygwin\\usr\\bin;" (getenv "PATH")))
;;   (setq shell-file-name "e:\\devtools\\cygwin\\bin\\bash.exe") ;;needed for inferior shells
;;   (setq explicit-shell-file-name shell-file-name) 
;;   (setenv "BASH_ENV" "$HOME/.bash_profile_for_emacs")
;;   (add-hook 'shell-mode-hook 'my-bash-shell-setup)))
;;   (shell))
  

(defun spawn-new-shell ()
  "create a new shell and rename it to a unique name"
  (interactive)
  (setq new-shell-buffer-name (concat "*shell-" (format "%s" (count-buffer-by-mode "Shell$")) "*"))
  (when (get-buffer new-shell-buffer-name)
    (princ "buffer already exists\n"))
  (shell)
  ;(princ (format "new shell buffer will be named %s\n" new-shell-buffer-name))
  (rename-buffer new-shell-buffer-name))
(defalias 'bash 'spawn-new-shell)



(defadvice shell (around shell-advised (&optional BUFFER) activate compile)
  "renumber/rename buffer for shell process"
  (interactive)
  (if current-prefix-arg  
      (let* ((count-of-buffers (count-buffer-by-mode "Shell$"))
             (new-shell-buffer-name (concat "*shell-" (format "%s" count-of-buffers) "*"))
             (new-shell-buffer (get-buffer-create new-shell-buffer-name)))
        ad-do-it new-shell-buffer)
    ad-do-it))
    
  
        


;; (defadvice shell (around shell-advised (&optional BUFFER) activate compile)
;;   "renumber/rename buffer for shell process"
;;   (interactive)
;;   (let ((count-of-buffers (count-buffer-by-mode "Shell$"))
;;         (new-shell-buffer-name (concat "*shell-" (format "%s" count-of-buffers) "*"))
;;         ad-do-it
;;         (when (and (> count-of-buffers 0 ) current-prefix-arg ) 
;;           (rename-buffer new-shell-buffer-name))
;;         (when (and (= count-of-buffers 0) (not current-prefix-arg))
;;           (message "not doing anything")))))


;;(add-hook 'comint-mode-hook 'comint-add-scroll-to-bottom)



;;(setq dirtrack-list '("^|Pr0mPT|\\([^|]*\\)|" 1 nil))
(setq dirtrack-list '("|Pr0mPT|\\([^|]*\\)|" 1 nil))


(defun dirtrack-filter-out-pwd-prompt (string)
  "dirtrack-mode doesn't remove the PWD match from the prompt.  This does."
  ;; TODO: support dirtrack-mode's multiline regexp.
  (if (and (stringp string) (string-match (first dirtrack-list) string))
      (replace-match "" t t string 0)
    string))

;;
;; (while (re-search-forward "\," limit t)
;;       (debug (format "  match: %s " (match-beginning 0)) )
;;       (replace-match ",\n"))

(add-hook 'shell-mode-hook
          #'(lambda ()
              (dirtrack-mode 1)
              (add-hook 'comint-preoutput-filter-functions
                        'dirtrack-filter-out-pwd-prompt t t)))


(defun dirtrack-filter-out-pwd-prompt-dmb (string)
  "dirtrack-mode doesn't remove the PWD match from the prompt.  This does."
  ;; TODO: support dirtrack-mode's multiline regexp.
  (while (and (stringp string) (string-match (first dirtrack-list) string))
    (replace-match "" t t string 0)))

  
;;  (if (and (stringp string) (string-match (first dirtrack-list) string))
;;      (replace-match "" t t string 0)
;;    string))

;;
;; (while (re-search-forward "\," limit t)
;;       (debug (format "  match: %s " (match-beginning 0)) )
;;       (replace-match ",\n"))




(message "dmb-shell.el done.")

