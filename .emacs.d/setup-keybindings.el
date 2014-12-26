(require 'dired)

;;(global-set-key "\C-x\C-b"       'bs-show)
;;(global-set-key "\C-x\C-b"       'ibuffer)
;;(global-set-key "\C-x\C-b"       'ibuffer-bs-show)

(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "M-*") 'query-replace)
(global-set-key (kbd "M-&") 'query-replace)                                       

(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M-_") 'text-scale-decrease)
                                       
                               
(eval-after-load "buff-menu" '(require 'buff-menu+))
;;(global-set-key "\C-x\C-b"      'buffer-menu)
(global-set-key "\C-x\C-b"        'bs-show)
(global-set-key [\C-x right]        'bs-cycle-next)
(global-set-key [\C-x left]         'bs-cycle-previous)


(define-key Buffer-menu-mode-map "q" 'delete-window)
(define-key Buffer-menu-mode-map "\C-g" 'delete-window) 
(define-key Buffer-menu-mode-map [return] 'Buffer-menu-select) 

(global-set-key [apps]             'iswitchb-buffer)
(global-set-key "\M-o"           'other-window)
;;(global-set-key [f3]             'simple-grep)
(global-set-key [f3]             'ag)
(global-set-key [f2]             'ack-and-a-half)
(global-set-key [f4]             'find-dired)
(global-set-key "\C-c\C-a"       'beginning-of-line)
;;(global-set-key "\C-a"       'beginning-of-line-text)



(global-set-key "\C-a"       '(lambda()
                                (interactive)
                                (if current-prefix-arg 
                                    (beginning-of-line) 
                                  (beginning-of-line-text))))

(global-set-key "\C-b"           'backward-char)

;;(global-set-key "\M-b"           'switch-to-buffer)
;(global-set-key "\M-g"           'goto-line)
(global-set-key "\C-s"           'isearch-forward-regexp)
(global-set-key "\C-r"           'isearch-backward-regexp)
(global-set-key [f9]             '(lambda () (interactive) (cycle-buffer-by-mode "Shell$")))
;;(global-set-key [f10]             '(lambda () (interactive) (cycle-buffer-by-mode "^SQLi.*")))
;;(global-set-key [f10]             'select-oracle-schema)

;;; (unless running-xemacs
;;;   (global-set-key "\C-a"           'beginning-of-line-text))
;; (global-set-key [f6]             '(lambda ()
;;                                     (interactive)
;;                                     (kill-buffer (get-buffer "*compilation*"))
;;                                     (delete-other-windows)))


(global-set-key "\C-x\C-f" 'ido-find-file)


(global-set-key [f6]             '(lambda ()
                                    (interactive)
                                    ;;store window config so it can be retrieved later
                                    (window-configuration-to-register ?l)
                                    ;;(kill-buffer (get-buffer "*compilation*"))
                                    (delete-other-windows)))

;;(global-set-key "\C-p"           'switch-to-other-buffer)
(global-set-key [\M-delete]     'kill-word)

(global-set-key "\C-x\M-\C-t" 'toggle-truncate-lines)

(defun kill-current-buffer ()
  "Kill the current buffer, without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))

;;(global-set-key "\C-xk" 'kill-current-buffer)
(global-set-key "\C-xk" 'kill-buffer)
(global-set-key "\C-c\C-c" 'comment-region)


(define-key isearch-mode-map "\C-h" 'isearch-mode-help) 



;;(global-set-key [f12]  'top-level)

;(global-set-key [f7]           'senator-next-tag)
;;(global-set-key [f7]           'toggle-show-tabs)

;;(define-key isearch-mode-map "\C-w" 'isearch-yank-word)  ;isearch-yank-word-or-char

;;(global-set-key "\C-d"           'delete-region)

;;(global-set-key [f11]           'blink-region)
;;(global-set-key "\C-b"           'blink-region)

;;(global-set-key "\C-xb" 'icicle-buffer)


 



;;(global-set-key "\C-x 4 j" 'tempo-complete-tag)

(global-set-key "\C-c\C-j" 'join-line)

(require 'conf-mode)
(define-key conf-colon-mode-map "\C-c\C-c" 'comment-region)


(global-set-key "\M-k" 'kill-whole-line)

(global-set-key [f7] 'swap-frames)
(global-set-key "\C-c\cs" 'swap-frames);


(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)



;;(global-set-key (kbd "<C-lwindow>") (lookup-key global-map (kbd "C-x")))
;;(global-set-key (kbd "<lwindow>") (lookup-key global-map (kbd "<meta>")))
;;http://lists.gnu.org/archive/html/emacs-devel/2006-04/msg00096.html

(setq w32-lwindow-modifier 'meta)
(setq w32-rwindow-modifier 'meta)
(setq w32-pass-lwindow-to-system nil)
(setq w32-pass-rwindow-to-system nil)

(global-set-key "\C-z" 'delete-whitespace)

(global-set-key [C-f12]  'bury-buffer)

;;(global-set-key "\C-x <return> <f>"  'set-buffer-file-coding-system)
;;(global-set-key [?\C-x return f] 'set-buffer-file-coding-system)
(global-set-key [\C-x return f] 'set-buffer-file-coding-system)

(define-key dired-mode-map "b" 'dired-up-directory)


;;in transient mark mode delete is delete-char same as C-d.
;; when transient mark mode is off, I like to still delete the region
;;(global-set-key [delete]     'delete-region)
;;(global-set-key [delete]     'delete-char)
(global-set-key [C-delete]     'delete-region)

(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c C-e") 'eval-and-replace)


(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8"))
(guide-key-mode 1)  ; Enable guide-key-mode
(setq guide-key/highlight-command-regexp "rectangle")
(setq guide-key/idle-delay 0.1)
(setq guide-key/popup-window-position 'bottom)

(require 'annoying-arrows-mode)
(annoying-arrows-mode 1)
(provide 'setup-keybindings)






