
(require 'erc)
(require 'erc-dcc)
(require 'socks)
(require 'erc-match)
(require 'tls)


;; older stuff
;;(defalias 'open-network-stream 'socks-open-network-stream)

(defcustom irc-password-file "~/Dropbox/.irc.conf" "location of the irc password file")

(when (file-exists-p irc-password-file)
  (load-file irc-password-file)
  (setq filename-history (delete irc-password-file file-name-history)))

;;this works:
(custom-set-variables 
 '(socks-server (list "tor" "localhost" 9050 5))
 '(socks-override-functions 1)
 '(socks-noproxy '("localhost")))

;; the following works for just ERC
;;(setq erc-server-connect-function 'socks-open-network-stream)
(setq erc-server-connect-function 'open-network-stream)

(custom-set-variables
 '(erc-enable-logging 'erc-log-all-but-server-buffers)
 '(erc-log-channels-directory "~/.erc.logs")
 '(erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE" "TOPIC")))

;; Load authentication info from an external source.  Put sensitive
;; passwords and the like in here.
;;(load "~/.emacs.d/.erc-auth")

;; This is an example of how to make a new command.  Type "/uptime" to
;; use it.
(defun erc-cmd-UPTIME (&rest ignore)
  "Display the uptime of the system, as well as some load-related
      stuff, to the current ERC buffer."
  (let ((uname-output
         (replace-regexp-in-string
          ", load average: " "] {Load average} ["
          ;; Collapse spaces, remove
          (replace-regexp-in-string
           " +" " "
           ;; Remove beginning and trailing whitespace
           (replace-regexp-in-string
            "^ +\\|[ \n]+$" ""
            (shell-command-to-string "uptime"))))))
    (erc-send-message
     (concat "{Uptime} [" uname-output "]"))))


;;(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#erc")))
;;(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#zsh" "#clojure")))
;;(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#zsh" "#scala")))
(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#documentcloud" "#erc" "##aws")))
(setq erc-autojoin-channels-alist '(("irc.matrixinsights.com" "#devops")))

;;#defocus #freenode

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)


(custom-set-variables
 '(erc-modules '(netsplit fill button match track completion readonly
                          networks ring autojoin noncommands irccontrols
                          move-to-prompt stamp menu list scrolltobottom)))

;;notify


;;(require 'dbus)
;;(require 'notifications)
;; (defun erc-global-notify (match-type nick message)
;;   "Notify when a message is recieved."
;;   (notifications-notify
;;    :title nick
;;    :body message
;;    ;;:app-icon "/usr/share/notify-osd/icons/gnome/scalable/status/notification-message-im.svg"
;;    :urgency 'low))

;;(add-hook 'erc-text-matched-hook 'erc-global-notify)


;; (defvar growlnotify-command (executable-find "growlnotify") "The path to growlnotify")

;; (defun growl (title message)
;;   "Shows a message through the growl notification system using
;;   `growlnotify-command` as the program."
;;   (flet ((encfn (s) (encode-coding-string s (keyboard-coding-system))) )
;;     (let* ((process (start-process "growlnotify" nil
;;                                    growlnotify-command
;;                                    (encfn title)
;;                                    "-a" "Emacs"
;;                                    "-n" "Emacs")))
;;       (process-send-string process (encfn message))
;;       (process-send-string process "\n")
;;       (process-send-eof process)))
;;   t)

(defun notification-center-notify (title message)
  "Use https://github.com/alloy/terminal-notifier to put mentions in notification center."
    (flet ((encfn (s) (encode-coding-string s (keyboard-coding-system))) )
    (let* ((process (start-process "notify" (get-buffer-create "*notify*") "~/Downloads/terminal-notifier_1.4.2/terminal-notifier.app/Contents/MacOS/terminal-notifier" 
                                   "-message" message ;;(encfn title)
                                   "-group" "irc")))
      ;;(process-send-string process (encfn message))
      (process-send-string process "\n")
      (process-send-eof process)))
  )


(defun my-erc-hook (match-type nick message)
  "Shows a growl notification, when user's nick was mentioned. If the buffer is currently not visible, makes it sticky."
  ;;(unless (posix-string-match "^\\** *Users on #" message)
  ;;  (growl
  ;;   (concat "ERC: name mentioned on: " (buffer-name (current-buffer)))
  ;;   message
  ;;  )))

  (notification-center-notify "message" message))

;;(add-hook 'erc-text-matched-hook 'my-erc-hook)

(defun send-oper (server nick)
  "If we're on the core server, send the oper command."
  (when (and (string= "irc.matrixinsights.com" server)
	     (= 6667 erc-session-port))
    (erc-send-command (format "OPER %s %s" (plist-get (assoc 'matrixinsights  irc-nicks-passwords-and-servers) :oper-nick) (plist-get (assoc 'matrixinsights  irc-nicks-passwords-and-servers) :oper-password)))))
(add-hook 'erc-after-connect 'send-oper)

(custom-set-variables 
 '(erc-kill-queries-on-quit t)
 '(erc-kill-server-buffer-on-quit t)
 '(erc-ring-mode 1))

(defun erc-connect ()
  "Startup normal erc connections"
  (interactive)
  ;; This causes ERC to connect to the Freenode network upon hitting
  ;; C-c e f.
  (erc :server "irc.freenode.net" :port "6667" :nick "d8v3" :password (plist-get (assoc 'freenode  irc-nicks-passwords-and-servers) :password))
  (erc :server "irc.matrixinsights.com" :port 6667 :nick "evadnoob" :password (plist-get (assoc 'matrixinsights  irc-nicks-passwords-and-servers) :password)))

;;(erc-connect)
