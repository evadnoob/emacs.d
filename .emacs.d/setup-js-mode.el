;;
;; setup js development mode
;;
;;(require 'mercurial)

(setq auto-mode-alist (cons '("\\.json" . javascript-mode) auto-mode-alist))

(require 'flymake)
;; (add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/less-css-mode"))
;; (require 'less-css-mode)

(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/flymake-less"))
(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/flymake-easy"))
(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/less-css-mode"))
(require 'flymake-less)
(add-hook 'less-css-mode-hook 'flymake-less-load)


;; force aspell/ispell to use this exact program
;; be sure to intall aspell like brew install --lang=en
(setq-default ispell-program-name "/usr/local/bin/aspell")

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'js2-mode-hook 'flyspell-prog-mode)
(add-hook 'js2-mode-hook 'subword-mode)


;; (add-to-list 'flymake-allowed-file-name-masks '("\\.less$" flymake-php-init))
;; (add-to-list 'flymake-allowed-file-name-masks '("\\.install$" flymake-php-init))
;; (add-to-list 'flymake-allowed-file-name-masks '("\\.inc$" flymake-php-init))
;; (add-to-list 'flymake-allowed-file-name-masks '("\\.engine$" flymake-php-init))

;; (custom-set-variables 
;;  '(setq flymake-allowed-file-name-masks nil))

(setq auto-mode-alist (cons '("\\.less" . less-css-mode) auto-mode-alist))
;; (add-hook 'css-mode-hook (lambda() 
;;                            (custom-set-variables '(css-indent-offset 2))))


(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/yasnipet"))
(require 'yasnippet)
(custom-set-variables 
 '(yas/snippet-dirs (*emacs ".emacs.x/snippets"))
 '(yas/wrap-around-region t)
 '(yas/prompt-functions '(yas/ido-prompt yas/completing-prompt))
 '(yas/root-directory (*emacs ".emacs.x/snippets")))
 
(yas/reload-all)
(add-hook 'js2-mode-hook
         '(lambda ()
            (yas/minor-mode)))

(custom-set-variables 
 '(js2-global-externs (list "console" "JSON" "$" "exports" "require" "app" "__dirname" "process" "module" "setTimeout"))
 '(js-indent-level 2)
 '(javascript-indent-level 2))

(require 'nxml-mode)


;;http://blog.deadpansincerity.com/2011/05/setting-up-emacs-as-a-javascript-editing-environment-for-fun-and-profit/
;;http://cx4a.org/software/auto-complete/manual.html#Installation

(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/auto-complete-1.3.1"))    ; This may not be appeared if you have already added.
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories (*emacs ".emacs.x/.emacs.p/auto-complete-1.3.1/dict"))
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'javascript-mode)
(setq ac-ignore-case t)
;; Examples
(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline 'ac-candidate-face "darkgray")
(set-face-background 'ac-selection-face "steelblue")
;; Let's have snippets in the auto-complete dropdown
(add-to-list 'ac-sources 'ac-source-yasnippet)

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

(auto-fill-mode 1)
(setq comment-auto-fill-only-comments t) 

(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/js2-mode"))
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-bounce-indent-p nil)
(custom-set-variables 
 '(js2-basic-offset 2)
 '(less-css-indent-level 2)
 '(js2-bounce-indent-p nil)
 '(css-indent-offset 2))


(provide 'setup-js-mode)




