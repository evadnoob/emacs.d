(require 'color-theme)

(w32-define-rgb-color 1 1 250 "dmb-light-blue")

(defun color-theme--dmb-default ()
  "Color theme by dmb, created 2007."
  (interactive)
  (color-theme-install
   '(color-theme--dmb-default
     ((background-color . "white")
      (background-mode . light)
      ;;(border-color . "black")
      ;;(cursor-color . ((t (:background "yellow" :foreground "black")))) ;;(cursor-color . "yellow")
      ;;(cursor-color . "yellow")
      ;;(cursor-color . "dmb-light-blue")
      (foreground-color . "black")
      ;;(mouse-color . "sienna1")
      )
     ;;(modeline ((t (:background "white smoke" :foreground "dark slate grey" :box (:line-width -1 :color "gainsboro") :height 75 :width normal )))) 
     ;;(modeline-inactive ((t (:background "white smoke" :foreground "grey" :box (:line-width -1 :color "gainsboro") :height 75 :width normal )))) 
     ;;(modeline ((t (:background "white smoke" :foreground "dark slate grey" :box (:line-width -1 :color "gainsboro") :width normal)))) 
     (modeline ((t (:background "grey90" :foreground "black" :box (:line-width -1 :color "grey75") :width normal)))) 
     (modeline-inactive ((t (:background "grey90" :foreground "grey20" :box (:line-width -1 :color "grey75") :width normal)))) 
     ;;(modeline-inactive ((t (:background "white smoke" :foreground "grey" :box (:line-width -1 :color "gainsboro") :width normal ))))
     ;;(mode-line ((((class color) (min-colors 88)) (:background "SystemMenuBar" :foreground "black" :box (:line-width -1 :style released-button)))))

     ;;(comint-highlight-input ((t (:background "blue" :foreground "cyan"))));
     (comint-highlight-prompt ((t (:foreground "blue"))))
     ;;(comint-highlight-prompt ((t (:foreground "pale goldenrod"))))
     (comint-highlight-prompt ((t (:foreground "SystemBackground"))))
     (modeline-buffer-id ((t (:foreground "blue4" )))) ;;:weight bold
     (isearch ((t (:background "blue" :foreground "cyan"))))
     ;;(cursor ((t (:foreground "white" :background "blue"))))
     (dmb-grep-hit-face ((t (:foreground "LightSlateGray"))))
     (dmb-face-three ((t (:foreground "blue"))))
     ;;(dmb-face-ibuffer-elisp ((t (:foreground "LightSlateGray"))))
     (dmb-face-ibuffer-jde ((t (:foreground "LightSlateGray"))))
     ;;(zmacs-region ((t (:background "snow" :foreground "blue"))))
     (woman-bold-face ((t (:foreground "MediumBlue"))))
     (minibuffer-prompt ((t (:foreground "MediumBlue"))))
     (buffer-menu-buffer-name ((t (:foreground "MidnightBlue"))))
     (buffer-menu-star-buffer ((t (:foreground "PaleTurquoise4"))))
     (buffer-menu-directory-buffer ((t (:foreground "MediumTurquoise" ))))
     (buffer-menu-delete-mark ((t (:foreground "red4" ))))
     (buffer-menu-file-name ((t (:foreground "PaleTurquoise4" ))))
     (buffer-menu-flagged-buffer ((t (:foreground "red4" ))))
     (buffer-menu-read-only-mark ((t (:foreground "chocolate1" ))))
     (woman-italic-face ((t (:foreground "OliveDrab")))))))


(custom-set-variables
 ;;'(ansi-color-names-vector (vector "black" "sienna" "DeepSkyBlue4" "yellow" "NavyBlue" "magenta" "cyan" "white"))
 ;;'(ansi-color-names-vector (vector "black" "sienna" "black" "yellow" "MidnightBlue" "magenta" "cyan" "white"))
 '(ansi-color-names-vector (vector "black" "sienna" "black" "yellow" "MidnightBlue" "DarkRed" "DarkSlateBlue" "white"))
 )


(add-to-list 'color-themes '(color-theme--dmb-default \"Dave Defaults \" \"dmb\"))
