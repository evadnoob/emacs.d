
;; 
;; toggle transparency
;;
;; (eval-when-compile (require 'cl))
;;  (defun toggle-transparency ()
;;    (interactive)
;;    (if (/=
;;         (cadr (find 'alpha (frame-parameters nil) :key #'car))
;;         100)
;;        (set-frame-parameter nil 'alpha '(100 100))
;;      (set-frame-parameter nil 'alpha '(85 60))))
;;  (global-set-key (kbd "C-c t") 'toggle-transparency)  


(eval-when-compile (require 'cl))

(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (find 'alpha (frame-parameters nil) :key #'car))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(85 60))))

(defun transparency-on ()
  (interactive)
  (set-frame-parameter nil 'alpha '(85 60)))

(defun transparency-off ()
  (interactive)
  (set-frame-parameter nil 'alpha '(100 100)))


;;(set-frame-parameter (selected-frame) 'alpha '(96 96))
;;(add-to-list 'default-frame-alist '(alpha 96 96))

(set-frame-parameter (selected-frame) 'alpha '(100 100))
(add-to-list 'default-frame-alist '(alpha 100 100))


(provide 'setup-transparency)

