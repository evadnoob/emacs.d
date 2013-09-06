(defun color-theme-dmb-dark ()
  "Theme by me!"
  (interactive)
  (color-theme-install
   '(color-theme-dmb-dark
     (;;(background-color . "#0D0000")
      ;;(background-color . "#131313")
      ;;(foreground-color . "#FFFFFF")
      (foreground-color . "#dadada")
      (background-mode . dark)
      (border-color . "#323232")
      (cursor-color . "#FFFFFF")
      (mouse-color . "#323232"))
     (mode-line ((t (:foreground "#FFFFFF" :background: "" :overline "LightGray" ))))
     (mode-line-inactive ((t (:foreground "#3a3a3a" :background: nil ))))
     (modeline-buffer-id ((t (:foreground "#5fff87" ))))
     (minibuffer-prompt ((t (:foreground "##00ff87" ))))
     (region ((t (:background "pale goldenrod" :foreground "blue3" ))))
     (vertical-border ((t (:foreground "gray"))))
     ;;(region ((t (:background "#323232"))))
     (region ((t (:background "#4f6af1" :foreground "gray"))))
     ;;(font-lock-comment-face ((t (:foreground "#949494"))))
     (font-lock-comment-face ((t (:foreground "gray54"))))
     (font-lock-constant-face ((t (:foreground "#6CE7C2"))))
     (font-lock-builtin-face ((t (:foreground "#00E1AD"))))
     (font-lock-function-name-face ((t (:foreground "#5fd7ff"))))
     (font-lock-variable-name-face ((t (:foreground "#d7af87"))))
     ;;(font-lock-keyword-face ((t (:foreground "#5fafff"))))
     (font-lock-keyword-face ((t (:foreground "#00d7ff"))))
     (font-lock-string-face ((t (:foreground "#dadada"))))
     (font-lock-doc-string-face ((t (:foreground "#5fffd7"))))
     (font-lock-doc-face ((t (:foreground "#659570"))))
     ;;(font-lock-doc-face ((t (:foreground "#8BCC99"))))
     (font-lock-type-face ((t (:foreground "#00AAB1"))))
     (which-func ((t (:foreground "cornflowerblue"))))
     (isearch-lazy-highlight-face ((t (:foreground "#5fff5f" :background nil))))
     (js2-external-variable-face ((t (:foreground "YellowGreen"))))
     (js2-error-face ((t (:background nil :foreground "#ff0000"))))

     (erc-input-face ((t (:background nil :foreground "#e4e4e4"))))
     
     )))

(custom-set-variables
 '(ansi-color-names-vector (vector "black" "white" "green" "white" "gray" "DarkRed" "blue" "white"))
 )


(provide 'color-theme-dmb-dark)
