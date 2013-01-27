(eval-when-compile    (require 'color-theme))

(defun color-theme--dmb-white2 ()
  "Color theme by David M. Boon, created 2007-04-26."
  (interactive)
  (color-theme-install
   '(color-theme--dmb-white2
     ((background-color . "SystemWindow")
      (background-mode . light)
      (border-color . "black")
      ;;(cursor-color . "light green")
      (foreground-color . "SystemWindowText")
      (mouse-color . "black"))
     ((compilation-message-face . underline)
      (ibuffer-deletion-face . font-lock-type-face)
      (ibuffer-filter-group-name-face . bold)
      (ibuffer-marked-face . font-lock-warning-face)
      (ibuffer-title-face . font-lock-type-face)
      (list-matching-lines-buffer-name-face . underline)
      (list-matching-lines-face . match)
      (ps-line-number-color . "black")
      (ps-zebra-color . 0.95)
      (rmail-highlight-face . rmail-highlight)
      (tags-tag-face . default)
      (vc-annotate-very-old-color . "#3F3FFF")
      (view-highlight-face . highlight)
      (widget-mouse-face . highlight))
     (default ((t (:stipple nil :background "SystemWindow" :foreground "SystemWindowText" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 83 :width normal :family "outline-lucida console"))))
     (bold ((t (:bold t :weight bold))))
     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
     (border ((t (:background "black"))))
     (buffer-menu-buffer ((t (:bold t :weight bold))))
     (button ((t (:underline t))))
     (c-nonbreakable-space-face ((t (:bold t :background "Red1" :weight bold))))
     (calendar-today ((t (:underline t))))
     (change-log-acknowledgement ((t (:foreground "#7b1717"))))
     (change-log-conditionals ((t (:foreground "#7f5c07"))))
     (change-log-date ((t (:foreground "#826363"))))
     (change-log-email ((t (:foreground "#7f5c07"))))
     (change-log-file ((t (:foreground "#0000b0"))))
     (change-log-function ((t (:foreground "#7f5c07"))))
     (change-log-list ((t (:foreground "#6e16a6"))))
     (change-log-name ((t (:foreground "#416d6e"))))
     (comint-highlight-input ((t (:bold t :weight bold :foreground "MediumBlue"))))
     ;;(comint-highlight-input ((t (:bold t :weight bold :foreground "LightGoldenrod1"))))
     (comint-highlight-prompt ((t (:foreground "dark blue"))))
     (compilation-column-number ((t (:foreground "#176017"))))
     (compilation-error ((t (:bold t :weight bold :foreground "Red1"))))
     (compilation-info ((t (:bold t :foreground "Green3" :weight bold))))
     (compilation-line-number ((t (:foreground "#7f5c07" :underline nil))))
     (compilation-warning ((t (:bold t :foreground "Orange" :weight bold))))
     (completions-common-part ((t (:family "outline-lucida console" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "SystemWindowText" :background "SystemWindow" :stipple nil :height 83))))
     (completions-first-difference ((t (:bold t :weight normal :foreground "blue"))))
     (cursor ((t (:background "blue" :foreground "black"))))
     (custom-button ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button)))))
     (custom-button-mouse ((t (:background "grey90" :foreground "black" :box (:line-width 2 :style released-button)))))
     (custom-button-pressed ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style pressed-button)))))
     (custom-button-pressed-unraised ((t (:underline t :foreground "magenta4"))))
     (custom-button-unraised ((t (:underline t))))
     (custom-changed ((t (:background "blue1" :foreground "white"))))
     (custom-comment ((t (:background "gray85"))))
     (custom-comment-tag ((t (:foreground "blue4"))))
     (custom-documentation ((t (nil))))
     (custom-face-tag ((t (:bold t :family "helv" :weight bold :height 1.2))))
     (custom-group-tag ((t (:bold t :foreground "blue1" :weight bold :height 1.2))))
     (custom-group-tag-1 ((t (:bold t :family "helv" :foreground "red1" :weight bold :height 1.2))))
     (custom-invalid ((t (:background "red1" :foreground "yellow1"))))
     (custom-link ((t (:underline t :foreground "blue1"))))
     (custom-modified ((t (:background "blue1" :foreground "white"))))
     (custom-rogue ((t (:background "black" :foreground "pink"))))
     (custom-saved ((t (:underline t))))
     (custom-set ((t (:background "white" :foreground "blue1"))))
     (custom-state ((t (:foreground "dark green"))))
     (custom-themed ((t (:background "blue1" :foreground "white"))))
     (custom-variable-button ((t (:bold t :underline t :weight bold))))
     (custom-variable-tag ((t (:bold t :family "helv" :foreground "blue1" :weight bold :height 1.2))))
     (diary ((t (:foreground "red1"))))
     (diary-anniversary ((t (:foreground "#6e16a6"))))
     (diary-button ((t (nil))))
     (diary-time ((t (:foreground "#7f5c07"))))
     (diff-added ((t (nil))))
     (diff-changed ((t (nil))))
     (diff-context ((t (:foreground "grey50"))))
     (diff-file-header ((t (:bold t :background "grey70" :weight bold))))
     (diff-function ((t (:background "grey85"))))
     (diff-header ((t (:background "grey85"))))
     (diff-hunk-header ((t (:background "grey85"))))
     (diff-index ((t (:bold t :weight bold :background "grey70"))))
     (diff-indicator-added ((t (nil))))
     (diff-indicator-changed ((t (nil))))
     (diff-indicator-removed ((t (nil))))
     (diff-nonexistent ((t (:bold t :weight bold :background "grey70"))))
     (diff-removed ((t (nil))))
     (dired-directory ((t (:foreground "#0000b0"))))
     (dired-flagged ((t (:bold t :weight bold :foreground "Red1"))))
     (dired-header ((t (:foreground "#176017"))))
     (dired-ignored ((t (:foreground "grey50"))))
     (dired-mark ((t (:foreground "#416d6e"))))
     (dired-marked ((t (:bold t :weight bold :foreground "Red1"))))
     (dired-symlink ((t (:foreground "#6e16a6"))))
     (dired-warning ((t (:foreground "#7b1717"))))
     (dmb-blink-region-face ((t (:background "lavendar"))))
     (dmb-current-line-face ((t (:background "alice blue"))))
     (dmb-face-five ((t (:foreground "IndianRed"))))
     (dmb-face-four ((t (:foreground "#997009"))))
     (dmb-face-hi-light-line-error ((t (:foreground "maroon"))))
     (dmb-face-ibuffer-elisp ((t (:foreground "DarkOliveGreen"))))
     (dmb-face-ibuffer-jde ((t (:foreground "blue4"))))
     (dmb-face-light-blue ((t (:foreground "LightSkyBlue"))))
     (dmb-face-one ((t (:foreground "midnight blue"))))
     (dmb-face-onex ((t (:foreground "LightSkyBlue"))))
     (dmb-face-six ((t (:foreground "DarkSlateBlue"))))
     (dmb-face-three ((t (:foreground "DarkOrange"))))
     (dmb-face-two ((t (:foreground "medium sea green"))))
     (dmb-grep-hit-face ((t (:foreground "DarkSlateBlue" :underline nil))))
     (dmb-grep-match-face ((t (:foreground "DarkSlateBlue" :underline nil))))
     (dmb-mark-face ((t (:foreground "cyan" :underline nil))))
     (dmb-notes-txt-face ((t (:foreground "DodgerBlue" :underline nil))))
     (eieio-custom-slot-tag-face ((t (:foreground "blue"))))
     (escape-glyph ((t (:foreground "brown"))))
     (ffap ((t (:background "darkseagreen2"))))
     (file-name-shadow ((t (:foreground "grey50"))))
     (fixed-pitch ((t (:family "courier"))))
     (font-lock-builtin-face ((t (:foreground "#964d94"))))
     (font-lock-comment-delimiter-face ((t (:foreground "#7b1717"))))
     (font-lock-comment-face ((t (:foreground "#7b1717"))))
     (font-lock-constant-face ((t (:foreground "#416d6e"))))
     (font-lock-doc-face ((t (:foreground "#826363"))))
     (font-lock-function-name-face ((t (:foreground "#0000b0"))))
     (font-lock-keyword-face ((t (:foreground "#6e16a6"))))
     (font-lock-negation-char-face ((t (nil))))
     (font-lock-preprocessor-face ((t (:foreground "#964d94"))))
     (font-lock-regexp-grouping-backslash ((t (:bold t :weight bold))))
     (font-lock-regexp-grouping-construct ((t (:bold t :weight bold))))
     (font-lock-string-face ((t (:foreground "#826363"))))
     (font-lock-type-face ((t (:foreground "#176017"))))
     (font-lock-variable-name-face ((t (:foreground "#7f5c07"))))
     (font-lock-warning-face ((t (:bold t :foreground "Red1" :weight bold))))
     (fringe ((t (:background "grey95"))))
     (header-line ((t (:box (:line-width -1 :style released-button) :background "grey90" :foreground "grey20" :box nil))))
     (help-argument-name ((t (nil))))
     (hi-black-b ((t (:bold t :weight bold))))
     (hi-black-hb ((t (:bold t :family "helv" :weight bold :height 1.67))))
     (hi-blue ((t (:background "light blue"))))
     (hi-blue-b ((t (:bold t :foreground "blue1" :weight bold))))
     (hi-green ((t (:foreground "PaleGreen2"))))
     (hi-green-b ((t (:bold t :foreground "green1" :weight bold))))
     (hi-indian-red ((t (:background "IndianRed1"))))
     (hi-indian-red-fg ((t (:foreground "IndianRed1"))))
     (hi-lemon-chiffon ((t (:background "lemon chiffon"))))
     (hi-light-goldenrod ((t (:background "light goldenrod"))))
     (hi-pink ((t (:background "pink"))))
     (hi-red-b ((t (:bold t :foreground "red1" :weight bold))))
     (hi-yellow ((t (:background "yellow1"))))
     (highlight ((t (:background "darkseagreen2"))))
     (holiday ((t (:background "pink"))))
     (ido-first-match ((t (:bold t :weight bold))))
     (ido-incomplete-regexp ((t (:bold t :weight bold :foreground "Red1"))))
     (ido-indicator ((t (:background "red1" :foreground "yellow1" :width condensed))))
     (ido-only-match ((t (:foreground "ForestGreen"))))
     (ido-subdir ((t (:foreground "red1"))))
     (info-header-node ((t (:italic t :bold t :weight bold :slant italic :foreground "brown"))))
     (info-header-xref ((t (:foreground "blue1" :underline t))))
     (info-menu-header ((t (:bold t :family "helv" :weight bold))))
     (info-menu-star ((t (:foreground "red1"))))
     (info-node ((t (:italic t :bold t :foreground "brown" :slant italic :weight bold))))
     (info-title-1 ((t (:bold t :weight bold :family "helv" :height 1.728))))
     (info-title-2 ((t (:bold t :family "helv" :weight bold :height 1.44))))
     (info-title-3 ((t (:bold t :weight bold :family "helv" :height 1.2))))
     (info-title-4 ((t (:bold t :family "helv" :weight bold))))
     (info-xref ((t (:underline t :foreground "blue1"))))
     (info-xref-visited ((t (:foreground "magenta4" :underline t))))
     (isearch ((t (:background "magenta3" :foreground "lightskyblue1"))))
     (iswitchb-current-match ((t (:foreground "#0000b0"))))
     (iswitchb-invalid-regexp ((t (:bold t :weight bold :foreground "Red1"))))
     (iswitchb-single-match ((t (:foreground "#7b1717"))))
     (iswitchb-virtual-matches ((t (:foreground "#964d94"))))
     (italic ((t (:underline t))))
     (jde-bug-breakpoint-cursor ((t (:background "brown" :foreground "cyan"))))
     (jde-db-active-breakpoint-face ((t (:background "red" :foreground "black"))))
     (jde-db-requested-breakpoint-face ((t (:background "yellow" :foreground "black"))))
     (jde-db-spec-breakpoint-face ((t (:background "green" :foreground "black"))))
     (jde-java-font-lock-api-face ((t (:foreground "dark goldenrod"))))
     (jde-java-font-lock-bold-face ((t (:bold t :weight bold))))
     (jde-java-font-lock-code-face ((t (nil))))
     (jde-java-font-lock-constant-face ((t (:foreground "CadetBlue"))))
     (jde-java-font-lock-doc-tag-face ((t (:foreground "green4"))))
     (jde-java-font-lock-italic-face ((t (:italic t :slant italic))))
     (jde-java-font-lock-link-face ((t (:foreground "blue" :underline t :slant normal))))
     (jde-java-font-lock-modifier-face ((t (:foreground "Orchid"))))
     (jde-java-font-lock-number-face ((t (:foreground "RosyBrown"))))
     (jde-java-font-lock-operator-face ((t (:foreground "medium blue"))))
     (jde-java-font-lock-package-face ((t (:foreground "blue3"))))
     (jde-java-font-lock-pre-face ((t (nil))))
     (jde-java-font-lock-private-face ((t (:foreground "Orchid"))))
     (jde-java-font-lock-protected-face ((t (:foreground "Orchid"))))
     (jde-java-font-lock-public-face ((t (:foreground "Orchid"))))
     (jde-java-font-lock-underline-face ((t (:underline t))))
     (lazy-highlight ((t (:background "paleturquoise"))))
     (link ((t (:foreground "blue1" :underline t))))
     (link-visited ((t (:underline t :foreground "magenta4"))))
     (match ((t (:background "yellow"))))
     (menu ((t (:foreground "systemmenu" :background "systemmenutext"))))
     (minibuffer-prompt ((t (:foreground "medium blue"))))
     ;;(mode-line ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))
     ;;(mode-line-buffer-id ((t (:foreground "#7C649D3F7683" :bold t :weight bold))))
     ;;(mode-line-buffer-id ((t (:foreground "#165990A3FFFF" :bold t :weight bold))))
;;      (mode-line ((t (:background "Gray90" :foreground "black" :box (:line-width -1 :color "grey80" :style none) :height 80 :width normal :family "neep"))))
;;      (mode-line-buffer-id ((t (:foreground "black" :bold t :weight bold))))
;;      (mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
;;      (mode-line-inactive ((t (:background "grey90" :foreground "grey20" :box (:line-width -1 :color "grey75" :style nil) :weight light))))

     (mode-line ((t (:background "white smoke" :foreground "gray40 white" :box (:line-width -1 :color "grey80" :style none) :height 80 :width normal :family "neep"))))
     (mode-line-buffer-id ((t (:foreground "black" :bold t :weight bold))))
     (mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
     (mode-line-inactive ((t (:background "grey90" :foreground "grey20" :box (:line-width -1 :color "grey75" :style nil) :weight light))))

     (mouse ((t (:background "black"))))
     (next-error ((t (:background "lightgoldenrod2"))))
     (nobreak-space ((t (:foreground "brown" :underline t))))
     (outline-1 ((t (:foreground "#0000b0"))))
     (outline-2 ((t (:foreground "#7f5c07"))))
     (outline-3 ((t (:foreground "#6e16a6"))))
     (outline-4 ((t (:foreground "#964d94"))))
     (outline-5 ((t (:foreground "#7b1717"))))
     (outline-6 ((t (:foreground "#416d6e"))))
     (outline-7 ((t (:foreground "#176017"))))
     (outline-8 ((t (:foreground "#826363"))))
     (query-replace ((t (:foreground "lightskyblue1" :background "magenta3"))))
     (region ((t (:background "lightgoldenrod2"))))
     (scroll-bar ((t (:foreground "systemscrollbar"))))
     (secondary-selection ((t (:background "yellow1"))))
     (semantic-highlight-edits-face ((t (:background "gray90"))))
     (semantic-unmatched-syntax-face ((t (:underline "red"))))
     (senator-intangible-face ((t (:foreground "gray25"))))
     (senator-momentary-highlight-face ((t (:background "gray70"))))
     (senator-read-only-face ((t (:background "#CCBBBB"))))
     (sgml-namespace-face ((t (:foreground "#964d94"))))
     (shadow ((t (:foreground "grey50"))))
     (show-paren-match ((t (:background "turquoise"))))
     (show-paren-mismatch ((t (:background "purple" :foreground "white"))))
     (speedbar-button-face ((t (:foreground "green4"))))
     (speedbar-directory-face ((t (:foreground "blue4"))))
     (speedbar-file-face ((t (:foreground "cyan4"))))
     (speedbar-highlight-face ((t (:background "green"))))
     (speedbar-selected-face ((t (:foreground "red" :underline t))))
     (speedbar-separator-face ((t (:background "blue" :foreground "white" :overline "gray"))))
     (speedbar-tag-face ((t (:foreground "brown"))))
     (svn-status-directory-face ((t (:foreground "blue4"))))
     (svn-status-filename-face ((t (:foreground "chocolate"))))
     (svn-status-locked-face ((t (:bold t :foreground "Red" :weight bold))))
     (svn-status-marked-face ((t (:foreground "green3"))))
     (svn-status-marked-popup-face ((t (:foreground "green3"))))
     (svn-status-switched-face ((t (:foreground "CadetBlue"))))
     (svn-status-symlink-face ((t (:foreground "cornflower blue"))))
     (svn-status-update-available-face ((t (:foreground "magenta"))))
     (tool-bar ((t (:background "systembuttonface" :foreground "systembuttontext" :box (:line-width 1 :style released-button)))))
     (tooltip ((t (:family "helv" :background "systeminfowindow" :foreground "systeminfotext"))))
     (trailing-whitespace ((t (:background "red1"))))
     (underline ((t (:underline t))))
     (variable-pitch ((t (:family "helv"))))
     (vertical-border ((t (nil))))
     (which-func ((t (:foreground "#0000b0"))))
     (widget-button ((t (:bold t :weight bold))))
     (widget-button-pressed ((t (:foreground "red1"))))
     (widget-documentation ((t (:foreground "dark green"))))
     (widget-field ((t (:background "gray85"))))
     (widget-inactive ((t (:foreground "grey50"))))
     (widget-single-line-field ((t (:background "gray85"))))

     (file-name-shadow ((t (:foreground "LightSlateGray"))))

     (ediff-odd-diff-A ((t (:foreground "grey26" :background "white smoke"))))
     (ediff-even-diff-A ((t (:foreground "grey51") :background "grey26")))
     (ediff-odd-diff-B ((t (:foreground "grey26"))))
     (ediff-even-diff-B ((t (:foreground "grey51") :background "grey26")))
     (ediff-current-diff-B ((t (:foreground "grey" :background "#2c6a8f"))))
     (ediff-current-diff-A ((t (:foreground "grey" :background "#b38080"))))
     (ediff-fine-diff-A ((t (:foreground "black" :background "pale goldenrod"))))
     (ediff-fine-diff-B ((t (:foreground "black" :background "pale goldenrod")))))))

(add-to-list 'color-themes '(color-theme--dmb-white2  "dmb white" "david boon"))