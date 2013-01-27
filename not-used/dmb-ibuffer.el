(require 'ibuffer)
(require 'ibuf-ext)

;;  ibuffer-saved-filter-groups (quote (("dave-default" ("java" (mode . jde-mode)) ("xml" (mode . sgml-mode)) ("python" (mode . python-mode)) ("elisp" (mode . emacs-lisp-mode)) ))))

;; (setq ibuffer-formats 
;;       '(
;;         (mark modified read-only " "
;;               (name 35 18 :left :elide)
;;               " "
;;               (mode 16 16 :left :elide)
;;               " " filename-and-process)
;;         (mark modified read-only " "
;;               (name 35 18 :left :elide)
;;               " "
;;               (size 9 -1 :right)
;;               " "
;;               (mode 16 16 :left :elide)
;;               " " filename-and-process)

;;         (mark " "
;;               (name 30 -1)
;;               " " filename)))



;;
;; remove headers from ibuffer
;; http://www.emacswiki.org/cgi-bin/wiki/IbufferMode
;;
(defadvice ibuffer-update-title-and-summary (after kill-2-lines)
  (save-excursion
    (set-buffer "*Ibuffer*")
    (toggle-read-only 0)
    (goto-char 1)
    (search-forward "-\n" nil t)
    (delete-region 1 (point))
    (let ((window-min-height 1)) 
      ;; save a little screen estate
      (shrink-window-if-larger-than-buffer))
    (toggle-read-only)))

(ad-activate 'ibuffer-update-title-and-summary)
;;(ad-deactivate 'ibuffer-update-title-and-summary)


;; (setq ibuffer-saved-filter-groups (cons 
;;       (quote ((
;;                "dave"
;;                ("dired" (mode . dired-mode))
;;                ;;("perl" (mode . cperl-mode))
;;                ;;("erc" (mode . erc-mode))
;;                ;;("planner" (or
;; ;;;                            (name . "^\\*Calendar\\*$")
;; ;;;                            (name . "^diary$")
;; ;;;                            (mode . muse-mode)))
;;                ("emacs" (or
;;                          (name . "^\\*scratch\\*$")
;;                          (name . "^\\*Messages\\*$")))
;;                ("elisp" (mode . emacs-lisp-mode))
;;                ("sqlplus" (name . "^sql \\[.*?@.*?\\]$"))
;;                ("comint" (mode . shell-mode))
;;                ("jde" (mode . jde-mode))
;; ;;;                ("gnus" (or
;; ;;;                         (mode . message-mode)
;; ;;;                         (mode . bbdb-mode)
;; ;;;                         (mode . mail-mode)
;; ;;;                         (mode . gnus-group-mode)
;; ;;;                         (mode . gnus-summary-mode)
;; ;;;                         (mode . gnus-article-mode)
;; ;;;                         (name . "^\\.bbdb$")
;; ;;;                         (name . "^\\.newsrc-dribble")))
;;                ))) ibuffer-saved-filter-groups))



;; (define-ibuffer-sorter filename-or-dired
;;   "Sort the buffers by their pathname."
;;   (:description "filenames plus dired")
;;   (string-lessp 
;;    (with-current-buffer (car a)
;;      (or buffer-file-name
;;          (if (eq major-mode 'dired-mode)
;;              (expand-file-name dired-directory))
;;          ;; so that all non pathnames are at the end
;;          "~"))
;;    (with-current-buffer (car b)
;;      (or buffer-file-name
;;          (if (eq major-mode 'dired-mode)
;;              (expand-file-name dired-directory))
;;          ;; so that all non pathnames are at the end
;;          "~"))))
;; (define-key ibuffer-mode-map (kbd "s p") 'ibuffer-do-sort-by-filename-or-dired)


(custom-set-variables

;;;   '(ibuffer-saved-filter-groups (quote (("dave" ("jde" (mode . jde-mode)) ("grep" (mode . grep-mode)) ("elisp" (mode . emacs-lisp-mode)) ("nxml" (mode . nxml-mode)) ("shell" (mode . shell-mode)) ("dired" (mode . dired-mode)) ("sh" (mode . sh-mode)))
;;;                                         ("jde" (mode . jde-mode)) ("grep" ("elisp" (mode . emacs-lisp-mode)) ("nxml" (mode . nxml-mode)) ("shell" (mode . shell-mode)) ("dired" (mode . dired-mode)) ("sh" (mode . sh-mode))) ("sqlplus" (name . "^sql")))))
 '(ibuffer-saved-filter-groups (quote (("dave" ("jde" (mode . jde-mode)) ("sqlplus" (mode . sql-interactive-mode)) ("grep" (mode . grep-mode)) ("elisp" (mode . emacs-lisp-mode)) ("nxml" (mode . nxml-mode)) ("shell" (mode . shell-mode)) ("dired" (mode . dired-mode)) ("sh" (mode . sh-mode))))))
;;  '(ibuffer-formats (quote ((mark modified read-only " " (name 45 45 :left :elide) " " (size 9 -1 :right) " " (mode 16 16 :left :elide) " " filename-and-process) (mark " " (name 16 -1) " " filename))))
  '(ibuffer-formats (quote ((mark modified read-only " " (name 45 45 :left :elide) " " filename-and-process) (mark " " (name 16 -1) " " filename))))

;;  '(ibuffer-saved-filters
;;    (quote (
;;            ("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode))))
;;            ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode)))))))

 )

;; (add-hook 'ibuffer-mode-hook
;;           (lambda ()
;;             (ibuffer-switch-to-saved-filter-groups "dave")))



;;(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)


;; '(ibuffer-fontification-alist (quote ((10 buffer-read-only font-lock-constant-face) (15 (and buffer-file-name (string-match ibuffer-compressed-file-name-regexp buffer-file-name)) font-lock-doc-face) (20 (string-match "^*" (buffer-name)) font-lock-keyword-face) (25 (and (string-match "^ " (buffer-name)) (null buffer-file-name)) italic) (30 (memq major-mode ibuffer-help-buffer-modes) font-lock-comment-face) (35 (eq major-mode (quote dired-mode)) dmb-face-one) (36 (eq major-mode (quote jde-mode)) default))))
;;  '(ibuffer-use-other-window t)
;;(add-to-list 'ibuffer-fontification-alist '(20 (string-match ibuffer-compressed-file-name-regexp) buffer-file-name))

(define-key ibuffer-mode-map "v" '(lambda () (interactive) (view-buffer (ibuffer-current-buffer t))))

(add-to-list 'ibuffer-fontification-alist '(40 (eq major-mode 'emacs-lisp-mode) dmb-face-ibuffer-elisp) t)
;;(add-to-list 'ibuffer-fontification-alist '(45 (string-match "^sql \\[.*?\\]$" (buffer-name)) dmb-face-three) t)
(setq ibuffer-fontification-alist (cons '(45 (eq major-mode 'sql-interactive-mode) dmb-face-three) ibuffer-fontification-alist)) 
(add-to-list 'ibuffer-fontification-alist '(50 (eq major-mode 'shell-mode) dmb-face-two) t)
;;(add-to-list 'ibuffer-fontification-alist '(55 (eq major-mode 'jde-mode) dmb-face-light-blue) t)
(add-to-list 'ibuffer-fontification-alist '(55 (eq major-mode 'jde-mode) dmb-face-ibuffer-jde) t)
(setq ibuffer-fontification-alist (cons '(56  (eq major-mode 'grep-mode) dmb-face-ibuffer-grep) ibuffer-fontification-alist ))
;;(add-to-list 'ibuffer-fontification-alist '(60 (string-match "notes\\.txt$" buffer-file-name) dmb-notes-txt-face) t)
;;(add-to-list 'ibuffer-fontification-alist '(60 (eq major-mode 'xml-mode) "DarkSeaGreen4") t)
(setq ibuffer-fontification-alist (cons '(65  (eq major-mode 'nxml-mode) 'compilation-info) ibuffer-fontification-alist ))
(setq ibuffer-fontification-alist (cons '(66  (eq major-mode 'sql-mode) 'dmb-face-ibuffer-sql) ibuffer-fontification-alist ))

;;(setq ibuffer-fontification-alist (cons '(56  (eq major-mode 'grep-mode) dmb-face-ibuffer-grep) ibuffer-fontification-alist ))
(custom-set-variables 
 '(ibuffer-use-other-window t)
 ;;    ibuffer-show-empty-filter-groups nil
 ;;    ibuffer-always-show-last-buffer t
 ;;    ibuffer-case-fold-search t
 '(ibuffer-default-shrink-to-minimum-size t)
 '(ibuffer-restore-window-config-on-quit t)
 '(ibuffer-never-show-predicates (list "^\\*scratch\\*" "^\\*Messages\\*"  "^\\*WoMan-Log\\*"  "^\\*JDEE bsh\\*" "^\\*Completions\\*" "^\\*anything\\*") )
 '(ibuffer-show-empty-filter-groups nil))


;; (define-ibuffer-column display-time (:name "R" :inline t)
;;   (time-to-seconds buffer-display-time))


;; (add-to-list 'ibuffer-formats 
;;    '(mark modified read-only " "
;;        (name 18 18 :left :elide)
;;        " "
;;        (display-time 5 10)
;;        " "
;;        (size 9 -1 :right)
;;        " "
;;        (mode 16 16 :left :elide)
;;        " " filename-and-process))


(define-key ibuffer-mode-map [return] 'ibuffer-visit-buffer-1-window)

(global-set-key "\C-x\C-b"       'ibuffer)
(define-key ibuffer-mode-map "\C-g" 'ibuffer-quit)

(provide 'dmb-ibuffer)
