
(defconst gud-control-frame-parameters
  (list
   '(name . "gud")
   ;;'(unsplittable . t)
   '(minibuffer . nil)
   '(user-position . t)	      ; Emacs only
   '(vertical-scroll-bars . nil)  ; Emacs only
   '(scrollbar-width . 0)         ; XEmacs only
   '(scrollbar-height . 0)        ; XEmacs only
   '(menu-bar-lines . 0)          ; Emacs only
   '(tool-bar-lines . 0)          ; Emacs 21+ only
   '(left-fringe    . 0)
   '(right-fringe   . 0)
   ;; don't lower but auto-raise
   '(auto-lower . nil)
   '(auto-raise . t)
   '(visibility . nil)
   ;; make initial frame small to avoid distraction
   '(width . 10) '(height . 10)
   ;; this blocks queries from  window manager as to where to put
   ;; ediff's control frame. we put the frame outside the display,
   ;; so the initial frame won't jump all over the screen
;;;    (cons 'top  (if (fboundp 'ediff-display-pixel-height)
;;; 		   (1+ (ediff-display-pixel-height))
;;; 		 200))
;;;    (cons 'left (if (fboundp 'ediff-display-pixel-width)
;;; 		   (1+ (ediff-display-pixel-width))
;;; 		 200))
   100
   100
   )
  "Frame parameters for displaying Ediff Control Panel.
Used internally---not a user option.")


(setq ctl-frame (make-frame gud-control-frame-parameters))
(select-frame ctl-frame)
(raise-frame ctl-frame)
;;(delete-frame ctl-frame)

