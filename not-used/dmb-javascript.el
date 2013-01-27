

;; (add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/yasnippet"))
;; (require 'yasnippet)
;; (yas/initialize)
;; (setq yas/snippet-dirs '((*emacs ".emacs.x/.emacs.p/yasnippet/snippets") (*emacs ".emacs.x/snippets")))
;; (yas/load-directory (*emacs ".emacs.x/snippets/text-mode"))
;; (yas/global-mode 1)


;; http://blog.deadpansincerity.com/2011/05/setting-up-emacs-as-a-javascript-editing-environment-for-fun-and-profit/
;; http://cx4a.org/software/auto-complete/manual.html#Installation

;; (add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/auto-complete-1.3.1"))    ; This may not be appeared if you have already added.
;; (require 'auto-complete)
;; (add-to-list 'ac-dictionary-directories (*emacs ".emacs.x/.emacs.p/auto-complete-1.3.1/dict"))
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (add-to-list 'ac-modes 'javascript-mode)
;; (setq ac-ignore-case t)
;; ;; Examples
;; (set-face-background 'ac-candidate-face "lightgray")
;; (set-face-underline 'ac-candidate-face "darkgray")
;; (set-face-background 'ac-selection-face "steelblue")
;; ;; Let's have snippets in the auto-complete dropdown
;; (add-to-list 'ac-sources 'ac-source-yasnippet)

;; git clone https://github.com/davidmiller/lintnode.git
;; git clone https://github.com/davidmiller/lintnode.git
;; cd lintnode
;; npm install express connect-form haml underscore


;; (add-to-list 'load-path "~/path/to/lintnode")
;; (require 'flymake-jslint)
;; ;; Make sure we can find the lintnode executable
;; (setq lintnode-location "~/path/to/lintnode")
;; ;; JSLint can be... opinionated
;; (setq lintnode-jslint-excludes (list 'nomen 'undef 'plusplus 'onevar 'white))
;; ;; Start the server when we first open a js file and start checking
;; (add-hook 'js-mode-hook
;;           (lambda ()
;;             (lintnode-hook)))


(custom-set-variables
 '(js-indent-level 2)
 '(javascript-indent-level 2))

(require 'nxml-mode)

;; (add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/js3-mode"))
;; ;;(require 'js3-mode)

;; (autoload 'js3-mode "js3" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))

;; (custom-set-variables
;;  '(js3-auto-indent-p t)         ; it's nice for commas to right themselves.
;;  '(js3-enter-indents-newline t) ; don't need to push tab before typing
;;  '(js3-indent-on-enter-key t)   ; fix indenting before moving on
;;  '(js3-indent-level 2)
;;  '(js3-global-externs (list "console" "JSON" "$" "exports" "require")))

;; (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
;; (setq exec-path
;;       '(
;;         "/usr/local/bin"
;;         "/usr/bin"
;;         ))

;; (add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/jshint-mode"))
;; (require 'flymake-jshint)
;; (add-hook 'javascript-mode-hook
;;      (lambda () (flymake-mode t)))


(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/js2-mode"))
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-bounce-indent-p nil)
(custom-set-variables 
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p nil))

;;(define-key js2-mode-map [mouse-1] nil)


;; (c-add-style "dmb-gnu"
;;   '("gnu" (c-basic-offset . 2)
;;     (c-comment-only-line-offset . 0)
;;     (c-hanging-braces-alist .
;;                             ;;'((substatement-open before after)))
;;                             '((substatement-open after)))
;;     (c-offsets-alist
;;      (substatement-open . 0)
;;      (inexpr-class . 0)
;;      (inher-cont     . c-lineup-java-inher)
;;      (arglist-intro  . +)
;;      ;;(arglist-close  . c-lineup-arglist)
;;      (arglist-close  . 0)
     
;;      (comment-intro  . 0)
;;      (case-label        . 4)
;;      (func-decl-cont . 0)) ;;c-lineup-java-throws))
;;     (c-cleanup-list . (scope-operator
;;                        empty-defun-braces
;;                        brace-catch-brace
;;                        comment-close-slash
;;                        defun-close-semi))
;;     (c-special-indent-hook . c-gnu-impose-minimum)
;;     (c-block-comment-prefix . "")) nil)

;;(c-set-style "dmb-gnu")
;;(c-toggle-electric-state 1)



(load-library "dmb-js-templates")
