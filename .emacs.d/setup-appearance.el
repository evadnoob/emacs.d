(when is-darwin
  (setq default-frame-alist
        '(
          ;;(font . "-apple-DejaVu_Sans_Mono-medium-normal-normal-*-10-*-*-*-m-0-iso10646-1")
          ;;(font . "-apple-Consolas-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1")
          ;;(font . "-apple-Menlo-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1")
	  ;;(font . "-apple-Menlo-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1")
	  (font . "-apple-Monaco-medium-normal-normal-*-10-*-*-*-m-0-iso10646-1")
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


;; (load-library "dmb-color-theme-late-night")

;; (if (not is-darwin)
;;     (progn
;;       (require 'color-theme)
;;       (color-theme-initialize)
      
;;       (load-library "dmb-color-theme-default2")
;;       (color-theme--dmb-default2)))


(if is-darwin 
    (progn
      (require 'color-theme)
      (color-theme-initialize)

      ;;(load-library "color-theme-sunburst")
      ;;(color-theme-sunburst)
      
      ;;(load-library "color-theme--dmb-default3")
      ;;(color-theme--dmb-default3)

      (load-library "color-theme-jadedragon-660650")
      (color-theme-jadedragon-660650)
      
      ;;(load-library "emacs-soothe-theme")
      ;;(color-theme-emacs-soothe)
       ))


;;(load-library "color-theme-twilight")
;;(color-theme-twilight)

;;(load-library "zenburn")
;;(color-theme-zenburn)



(global-hl-line-mode -1)

;; Customize background color of lighlighted line
;;(set-face-background 'hl-line "#FFE3AB")
;;(set-face-background 'hl-line "#D85652")
;;(set-face-background 'hl-line "#CDE1FF")
;;(set-face-foreground  'hl-line "#000000")

;; Highlight in yasnippet
;;(set-face-background 'yas/field-highlight-face "#333399")

;;(set-face-foreground 'font-lock-warning-face "#ff6666")

;; org-mode colors
(setq org-todo-keyword-faces
      '(
        ("INPR" . (:foreground "yellow" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("IMPEDED" . (:foreground "red" :weight bold))
        ))

(provide 'dmb-appearance)
