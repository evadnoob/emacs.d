(when is-darwin
  (setq default-frame-alist
        '(
	  (font . "-apple-Monaco-medium-normal-normal-*-10-*-*-*-m-0-iso10646-1")
          ;;(cursor-type bar . 3)
          ;;(cursor-type bar . 8)
          (cursor-color . "#1E16ED")
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

(setq initial-frame-alist default-frame-alist)


(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/color-theme-6.6.0"))
(add-to-list 'load-path (*emacs ".emacs.x/.emacs.p/color-theme-6.6.0/themes"))

;;(add-hook 'before-make-frame-hook 'turn-off-tool-bar)

;;
;; define some faces to use:
;;

;(defface dmb-x-face :foreground "MediumBlue")

(defface dmb-face-one
  '((((class color)) (:foreground "midnight blue")))
  "my own font lock faces."
  :group 'dmb-faces)

(defface dmb-face-onex
  '((((class color)) (:foreground "LightSkyBlue")))
  "my own font lock faces."
  :group 'dmb-faces)


(defface dmb-face-two
  '((((class color)) (:foreground "medium sea green")))
  "my own font lock faces."
  :group 'dmb-faces)

(defface dmb-face-three
  '((((class color)) (:foreground "DarkOrange")))
  "my own font lock faces."
  :group 'dmb-faces)

(defface dmb-face-four
  '((((class color)) (:foreground "#997009")))
  "my own font lock faces."
  :group 'dmb-faces)

(defface dmb-face-five
  '((((class color)) (:foreground "IndianRed")))
  "my own font lock faces."
  :group 'dmb-faces)

(defface dmb-face-six 
'((((class color)) (:foreground "DarkSlateBlue")))
  "my own font lock faces."
  :group 'dmb-faces)


(defface dmb-grep-match-face
'((((class color)) (:foreground "DarkSlateBlue" :underline nil)))
  "my own font lock faces."
  :group 'dmb-faces)

(defface dmb-grep-hit-face
'((((class color)) (:foreground "DarkSlateBlue" :underline nil)))
  "my own font lock faces."
  :group 'dmb-faces)



(defface dmb-mark-face
'((((class color)) (:foreground "cyan" :underline nil)))
  "my own font lock faces."
  :group 'dmb-faces)


(defface dmb-notes-txt-face
'((((class color)) (:foreground "DodgerBlue" :underline nil)))
  "my own font lock faces."
  :group 'dmb-faces)


(defface dmb-face-light-blue
  '((((class color)) (:foreground "LightSkyBlue")))
  "my own font lock faces."
  :group 'dmb-faces)


(defface dmb-face-ibuffer-jde
  '((((class color)) (:foreground "MediumAquamarine")))
  "my own font lock faces."
  :group 'dmb-faces)


(defface dmb-face-hi-light-line-error
  '((((class color)) (:foreground "maroon")))
  "my own font lock faces."
  :group 'dmb-faces)

(defface dmb-face-ibuffer-elisp
  '((((class color)) (:foreground "SeaGreen")))
  "my own font lock faces."
  :group 'dmb-faces)

(defface dmb-face-ibuffer-grep
  '((((class color)) (:foreground "yellow4")))
  "my own font lock faces."
  :group 'dmb-faces)


(defface dmb-mode-line-directory-face
  '((((class color)) (:foreground "#357797")))
  "my own font lock faces."
  :group 'dmb-faces)



(defface dmb-face-ibuffer-sql
  '((((class color)) (:foreground "grey50")))
  "face for ibuffer sql files."
  :group 'dmb-faces)

(defface dmb-face-ibuffer-sql-interactive
  '((((class color)) (:foreground "DarkOrange")))
  "face for ibuffer sql interactive buffers."
  :group 'dmb-faces)


  
  ;;
  ;; test for frame and set color theme correctly depending on tty or graphics display.
  ;;
  (defun color-theme-for-frame (frame)
    (let ((color-theme-is-global nil))
      (select-frame frame)
      (when (not (window-system frame))
        (message "window-system, setting color theme to dmb-dark")
        (add-to-list 'custom-theme-load-path (*emacs ".emacs.x/themes/"))
        (load-file (*emacs ".emacs.x/themes/dmb-bliss.el")))
      
      (when (window-system frame)
        (message "window-system, setting color theme to dmb-default3")
        (load-library "color-theme--dmb-default3")
        (color-theme--dmb-default3))
      ))

  ;; hook on after-make-frame-functions
  (add-hook 'after-make-frame-functions 'color-theme-for-frame)

  ;; Start up the color theme in this initial frame.
  (color-theme-for-frame (selected-frame))
  ;; (let ((color-theme-is-global nil))
  ;;   (when (window-system)
  ;;     (color-theme-railscasts)))



(global-hl-line-mode -1)

;; org-mode colors
(setq org-todo-keyword-faces
      '(
        ("INPR" . (:foreground "yellow" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("IMPEDED" . (:foreground "red" :weight bold))
        ))

(custom-set-variables 
 '(custom-theme-directory (*emacs ".emacs.x/themes")))


(provide 'dmb-appearance)
