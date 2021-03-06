(require 'color-theme)


(defun color-theme--dmb-default3 ()
  "Color theme by dmb, created 2007."
  (interactive)
  (color-theme-install
   '(color-theme--dmb-default3
     ((background-color . "white")
      (foreground-color . "black"))
     (mode-line ((t (:background "WhiteSmoke" :foreground "black" :overline "LightGray"))))  ;;:background "GhostWhite" ;;:background "gray91"
     (mode-line-inactive ((t (:background "WhiteSmoke" :foreground "grey60" :overline "grey" :weight bold))))
     (vertical-border ((t (:foreground "gray"))))
     ;;(nxml-element-local-name ((t (:foreground "NavyBlue"))))
     ;; (font-lock-comment-face ((t (:foreground "DimGrey" :slant normal))) )  ;;:slant italic
     ;; (font-lock-function-name-face ((t (:foreground "SlateBlue3"))))
     ;; (font-lock-builtin-face ((t (:foreground "DarkOliveGreen4"))))
      (font-lock-keyword-face ((t (:foreground "#3a6ea5"))))
     ;; (font-lock-doc-face  ((t (:foreground "MediumSeaGreen" :slant italic )))) 
     ;; (font-lock-warning-face  ((t (:foreground "red3" :weight bold :slant normal )))) 
     ;; (font-lock-string-face   ((t (:foreground "burlywood4" :weight normal :slant normal )))) 
     ;; (Buffer-menu-buffer ((t (:foreground "dark slate gray"))))
     ;; (dmb-face-ibuffer-jde ((t (:foreground "dark slate gray"))))
     ;; (dmb-face-ibuffer-grep ((t (:foreground "goldenrod4"))))
     (modeline-buffer-id ((t (:foreground "yellow4" ))))
     ;; (ibuffer-filter-group-name-face ((t (:foreground "dark slate gray" ))))
     ;; (sr-passive-path-face ((t (:fogreground "dark green" ))))
     ;; (sr-active-path-face ((t (:foreground "blue" ))))
     ;; (sh-heredoc ((t (:foreground "yellow4" ))))
     ;; (sh-quoted-exec ((t (:foreground "DarkOliveGreen"))))
     ;;(region ((t (:background "pale goldenrod" :foreground "gray50" ))))
     (region ((t (:background "pale goldenrod" :foreground "blue3" ))))
     ;; (comint-highlight-prompt ((t (:foreground "blue" ))))
     ;; (mumamo-background-chunk-major ((t (:background nil ))))
     ;; (mumamo-background-chunk-submode ((t (:background nil ))))
     (diredp-flag-mark-line ((t (:background nil :foreground "blue" ))))
     (diredp-flag-mark ((t (:background nil :foreground "purple" ))))
     (diredp-compressed-file-suffix ((t (:foreground "goldenrod"))))
     (diredp-dir-heading ((t (:foreground "goldenrod"))))
     (diredp-dir-priv ((t (:foreground "blue4"))))
     (diredp-exec-priv ((t (:foreground "DarkGreen"))))
     (diredp-file-name ((t (:foreground "black"))))
     (diredp-no-priv ((t (:foreground "black"))))
     (diredp-other-priv ((t (:foreground "blue4"))))
     (diredp-read-priv ((t (:foreground "blue4"))))
     (diredp-write-priv ((t (:foreground "blue4"))))
     (compilation-info ((t (:foreground "green4"))))
     (dmb-face-ibuffer-jde ((t (:foreground "blue4"))))
     (erc-timestamp-face ((t (:foreground "gray"))))
     (magit-item-highlight  ((t (:background "#ebebeb"))))
     (mode-line-buffer-id ((t (:foreground "yellow" ))))
     ;;(mode-line ((t (:background "grey8" :foreground "grey74" :overline "orange" :box (:line-width -1 :color "grey80" :style none)  ))))
     (mode-line ((t (:background "grey75" :foreground "#333" :box nil :overline "orange"))))
     (mode-line-inactive ((t (:background "gray8" :foreground "grey50" :overline "SteelBlue4" :box (:line-width -1 :color "grey80" :style none) ))))
     
     ;; (flymake-errline ((((class color)) (:underline "OrangeRed"))))
     ;; (flymake-warnline ((((class color)) (:underline "DarkSlateBlue"))))
     ;; (semantic-tag-boundary-face ((((class color)) (:slant italic))))
)))


(custom-set-variables
 '(ansi-color-names-vector (vector "black" "sienna" "black" "peru" "DarkOliveGreen4" "DarkRed" "DarkSlateBlue" "white"))
 )


(add-to-list 'color-themes '(color-theme--dmb-default3 \"Dave Defaults \" \"dmb\"))

