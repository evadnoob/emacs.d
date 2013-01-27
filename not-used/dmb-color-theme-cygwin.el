(require 'color-theme)

(w32-define-rgb-color 1 1 250 "dmb-light-blue")

(defun color-theme--dmb-cygwin ()
  "Color theme by dmb, created 2007."
  (interactive)
  (color-theme-install
   '(color-theme--dmb-cygwin
     ((background-color . "black")
      (foreground-color . "gray"))
     (mode-line ((t (:background "WhiteSmoke" :foreground "black" :overline "LightGray"))))  ;;:background "GhostWhite" ;;:background "gray91"
     (mode-line-inactive ((t (:background "WhiteSmoke" :foreground "grey60" :overline "grey" :weight bold))))
     (dmb-mode-line-directory-face ((t (:foreground "dark slate gray" ))))
     (vertical-border ((t (:foreground "gray"))))
     (nxml-element-local-name ((t (:foreground "NavyBlue"))))
     ;; (font-lock-comment-face ((t (:foreground "DimGrey" :slant normal))) )  ;;:slant italic
     ;; (font-lock-function-name-face ((t (:foreground "SlateBlue3"))))
     ;; (font-lock-builtin-face ((t (:foreground "DarkOliveGreen4"))))
     ;; (font-lock-keyword-face ((t (:foreground "#3a6ea5"))))
     ;; (font-lock-doc-face  ((t (:foreground "MediumSeaGreen" :slant italic )))) 
     (Buffer-menu-buffer ((t (:foreground "dark slate gray"))))
     (dmb-face-ibuffer-jde ((t (:foreground "dark slate gray"))))
     (dmb-face-ibuffer-grep ((t (:foreground "goldenrod4"))))
     (modeline-buffer-id ((t (:foreground "yellow4" ))))
     (ibuffer-filter-group-name-face ((t (:foreground "dark slate gray" ))))
     (sr-passive-path-face ((t (:fogreground "dark green" ))))
     (sr-active-path-face ((t (:foreground "blue" ))))
     (sh-heredoc ((t (:foreground "yellow4" ))))
     (sh-quoted-exec ((t (:foreground "DarkOliveGreen"))))
     (region ((t (:background "pale goldenrod" :foreground "gray50" ))))
     (comint-highlight-prompt ((t (:foreground "blue" ))))
     (mumamo-background-chunk-major ((t (:background nil ))))
     (mumamo-background-chunk-submode ((t (:background nil ))))
)))


(custom-set-variables
 '(ansi-color-names-vector (vector "black" "sienna" "black" "peru" "DarkOliveGreen4" "DarkRed" "DarkSlateBlue" "white"))
 )


(add-to-list 'color-themes '(color-theme--dmb-cygwin \"Dave Defaults \" \"dmb\"))

