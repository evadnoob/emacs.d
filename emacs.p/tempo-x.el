;;; tempo-x.el --- More elements for tempo

;; Copyright (C) 2007 Free Software Foundation, Inc.
;;
;; Author: Wenbin <wenbinye@gmail.com>
;; Maintainer: Wenbin <wenbinye@gmail.com>
;; Created: 27 Dec 2007
;; Version: 0.01
;; Keywords: abbrev

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Examples:
;; Here is an example for read and insert keywords for elisp file
;; header as autoinsert does:
;; (tempo-x-test-template
;;  '((progn
;;      (require 'finder)
;;      (setq minibuffer-help-form
;;            (mapconcat (lambda (x) (format "%10s:  %s" (car x) (cdr x)))
;;                       finder-known-keywords
;;                       "\n"))
;;      nil)
;;    ";; Keywords: "
;;    (R (pi ("Keyword, C-h: " finder-known-keywords nil t) nil keyword)
;;       (& keyword (delete-backward-char 2))
;;       ", ")
;;    (setq minibuffer-help-form nil)))
;;
;; The same as sh-if:
;; (tempo-x-test-template
;;  '("if [ " (p "condition: ") " ]; then" n>
;;    p n>
;;    (R "elif [ " (p "condition: " condition)
;;       (& condition (delete-region recursion-start (point)))
;;       " ]; then" n> p n>)
;;    "else" n> p n> "fi"))
;;
;; The same examples in tempo-snippets.el:
;; (tempo-define-template
;;  "java-class"
;;  '((snippet "class " (S Class) " {\n\n"
;;             > "public " (S Class) "(" p ") {\n" > p n
;;             "}" > n n "}" > n)))
;; 
;; (tempo-define-template
;;  "java-get-set"
;;  '((snippet
;;     "private " (S Type) " _" (S Name "name") ";\n\n"
;;     > "public " (S Type) " get" (F (Name) (upcase-initials Name))
;;     "() {\n"
;;     > "return _" (S Name)  ";\n" "}" > n n
;;     > "public set" (F (Name) (upcase-initials Name))
;;     "(" (S Type) " value) {\n"
;;     > "_" (S Name) " = value;\n" "}" > n)))
;;
;; This is a perl open function example:
;; (tempo-define-template
;;  "perl-open"
;;  '((snippet
;;     "open("
;;     (S FH "my $fh" t) ", \"" (S op "<") "\", "
;;     (S file "$file")
;;     ") or die \"Can't "
;;     (F (op)
;;        (if (string-match ">" op)
;;            "create" "open"))
;;     " file "
;;     (F (file)
;;        (replace-regexp-in-string "['\"]?\\(.*\\)['\"]" "\\1" file))
;;     ": $!\";")))

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'tempo-x)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'tempo)

(defgroup tempo-x nil
  "Various tempo elements handler."
  :group 'abbrev
  :group 'convenience)

;;;###autoload 
(defun tempo-x-space ()
  "Expand tempo if complete in `tempo-local-tags' or insert space."
  (interactive "*")
  (or (tempo-expand-if-complete)
      (call-interactively 'self-insert-command)))

(defun tempo-x-rebuild ()
  "Force to rebuild collections in all buffer"
  (interactive)
  (mapc (lambda (buf)
          (with-current-buffer buf
            (if (local-variable-p 'tempo-dirty-collection)
                (tempo-invalidate-collection))))
        (buffer-list)))

(defun tempo-x-elements-handler (element)
  "Handle my tempo element"
  (if (consp element)
      (cond ((eq (car element) 'pi)
             (apply 'tempo-x-insert-prompt (cdr element)))
            ((eq (car element) 'Pi)
             (let ((tempo-interactive t))
               (apply 'tempo-x-insert-prompt (cdr element))))
            ((eq (car element) 'R)
             (funcall 'tempo-x-insert-recursion (cdr element)))
            ((eq (car element) 'snippet)
             (funcall 'tempo-x-insert-snippet (cdr element))))
    (if (eq element 'C)
        (setq tempo-marks nil))))

(defun tempo-x-insert-prompt (prompt &optional default save-name no-insert)
  "Prompt for a text string with DEFAULT.
Like `tempo-insert-prompt', but you can give parameters for
`completing-reading' in CDR of PROMPT."
  (let ((saved (and save-name (tempo-lookup-named save-name)))
        insertion)
    (cond ((and saved (not no-insert))
           (tempo-insert-named save-name)
           "")
          (saved "")
          (tempo-interactive
           (cond ((stringp prompt)
                  (setq insertion (read-from-minibuffer prompt default)))
                 ((and (listp prompt) (stringp (car prompt)))
                  (let ((old-args (cdr prompt))
                        args)
                    (setq prompt (replace-regexp-in-string ":\\s-$" "" (car prompt)))
                    (setq prompt
                          (if default
                              (format "%s (default %s): " prompt default)
                            (concat prompt ": ")))
                    (dotimes (i 7)
                      (cond ((eq i 5)
                             (push default args))
                            ;; if the collections is a symbol, convert it to symbol value
                            ((and (eq i 0) (boundp (nth i old-args)))
                             (push (symbol-value (nth i old-args)) args))
                            (t (push (nth i old-args) args))))
                    (setq insertion (apply 'completing-read prompt (nreverse args)))))
                 (t (error "The prompt (%s) is not a string or a list" prompt)))
           (if save-name (tempo-save-named save-name insertion))
           (if no-insert "" insertion))
          (t (tempo-insert-mark (point-marker)) (or default "")))))

(defun tempo-x-insert-recursion (element)
  "Hander recursive for template.
 Elements like:
 (recursive other-elements (recursive-test condition final-form) ...)

the CADR of recursive-test can be a saved name or a form to eval.
If the name is empty or the eval result is false, will eval
final-forms and abort loop.

This is an example to insert sh-if much like the skeleton does.

   (tempo-define-template
    \"sh-if\"
    '(\"if [ \" (p \"condition: \") \" ]; then\" n>
      p n>
      (R \"elif [ \" (p \"condition: \" condition)
         (& condition (delete-region recursion-start (point)))
         \" ]; then\" n> p n>)
      \"else\" n> p n> \"fi\"))
"
  (let ((tempo-named-insertions tempo-named-insertions)
        recursion-start condition)
    (catch 'exit
      (while t
        (setq recursion-start (point))
        (mapc (lambda (elt)
                (if (and (consp elt)
                         (eq (car elt) '&))
                    (progn
                      (setq condition (cadr elt))
                      (unless (if (symbolp condition)
                                  (prog1
                                      (> (length (tempo-lookup-named condition)) 0)
                                    ;; remove saved name for prompt again
                                    (tempo-save-named condition nil))
                                (eval condition))
                        (eval (nth 2 elt))
                        (throw 'exit t)))
                  (tempo-insert elt tempo-insert-region)))
              element)))
    ;; tell tempo-insert I have done all the job
    ""))

;;{{{  Snippet
;;; Most functions comes tempo-snippets.el. I make some changes
;;; to add these feature:
;;; 1. use with other tempo elements as well as users elements
;;; 2. add keymap in snippet region
;;; 3. easy interface for form.
(defvar tempo-x-exclude-chars ""
  "*Chars not to extend when add to front or end of the field.")

(defface tempo-x-editable-face
  '((((background dark)) (:background "steel blue"))
    (((background light)) (:background "light cyan")))
  "*Face used for editable text in tempo snippets."
  :group 'tempo-x)

(defface tempo-x-auto-face
  '((((background dark)) (:background "dodger blue"))
    (((background light)) (:background "light gray")))
  "*Face used for automatically updating text in tempo snippets."
  :group 'tempo-x)

(defface tempo-x-form-face
  '((((background dark)) (:background "dodger blue"))
    (((background light)) (:background "light gray")))
  "*Face used for automatically updating text in tempo snippets."
  :group 'tempo-x)

(defvar tempo-x-snippet-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'tempo-x-next-field)
    (define-key map (kbd "<backtab>") 'tempo-x-previous-field)
    map)
  "*keymap for tempo marker")

(defvar tempo-x-delete-field-text nil)

(defvar tempo-x-snippet-overlay nil
  "Overlay to install keymap")
(make-variable-buffer-local 'tempo-x-snippet-overlay)

(defvar tempo-x-snippet-sources nil
  "List of source overlays for current snippet.")
(make-variable-buffer-local 'tempo-x-snippet-sources)

(defvar tempo-x-snippet-forms nil
  "List of forms overlays for current snippet.")
(make-variable-buffer-local 'tempo-x-snippet-forms)

(defun tempo-x-insert-snippet (elements)
  "Provide snippet abbev.
Syntax of snippet:
 (snippet (S name &optional display insert)
          (F (vars) forms))

S insert a field, the first NAME will be the source and other field
with the same NAME become mirrors. Change the source will also change
mirrors. DISPLAY is the text to insert to the field, default is the
`symbol-name' of NAME. INSERT is non-nil means the DISPLAY is the
default text, you can make change to the text. Otherwise the text will
be erase after any changes in front of field.

F insert an form. The VARS is a list of NAME used in fields. the
evaled result will insert into the buffer. When any fields in VARS
changed, the text of form will change too."
  (tempo-x-snippet-clear)
  (let ((inhibit-modification-hooks t)
        (beg (point))
        ov)
    (mapc (lambda (elem)
            (if (and (listp elem) (memq (car elem) '(S F)))
                (cond ((eq (car elem) 'S)
                       (apply 'tempo-x-insert-field (cdr elem)))
                      ((eq (car elem) 'F)
                       (apply 'tempo-x-insert-form (cdr elem))))
              (tempo-insert elem nil)))
          elements)
    ;; update all forms after sources are filled
    (mapc (lambda (ov)
            (let ((text (eval (overlay-get ov 'tempo-x-form))))
              (when text
                (tempo-x-set-overlay-text ov text))))
          tempo-x-snippet-forms)
    ;; make keymap overlay
    (setq ov (make-overlay beg (point)))
    (overlay-put ov 'keymap tempo-x-snippet-map)
    (setq tempo-x-snippet-overlay ov)
    ""))

(defun tempo-x-insert-field (name &optional display insert)
  "Insert a field to buffer."
  (if (tempo-x-find-source name)
      (tempo-x-insert-mirror name)
    (tempo-x-insert-source name display insert)))

(defun tempo-x-insert-source (name display insert)
  "Insert source field to buffer"
  (tempo-insert-mark (point-marker))
  (let ((beg (point))
        (text (or display (symbol-name name)))
        ov)
    (insert text)
    (setq ov (make-overlay beg (point)))
    (mapc (lambda (pair)
            (overlay-put ov (car pair) (cdr pair)))
          `((tempo-x-name . ,name)
            (tempo-x-display . ,text)
            (tempo-x-insert . ,insert)
            (face . tempo-x-editable-face)
            (intangible . ,(not insert))
            ,@(if insert
                  '((modification-hooks tempo-x-snippet-update)
                    (insert-behind-hooks tempo-x-snippet-update)
                    (insert-in-front-hooks tempo-x-snippet-update))
                '((insert-in-front-hooks tempo-x-snippet-replace)
                  (modification-hooks tempo-x-snippet-update)))))
    (push (cons name ov) tempo-x-snippet-sources)))

(defun tempo-x-insert-mirror (name)
  "Insert mirror field to buffer"
  (let ((beg (point))
        (source (tempo-x-find-source name))
        ov)
    (when source
      (insert (tempo-x-overlay-text source))
      (setq ov (make-overlay beg (point)))
      (let ((mirrors (overlay-get source 'tempo-x-mirrors)))
        (push ov mirrors)
        (overlay-put source 'tempo-x-mirrors mirrors))
      (mapc (lambda (pair)
              (overlay-put ov (car pair) (cdr pair)))
            `((face . tempo-x-auto-face)
              (modification-hooks tempo-x-delete-field)
              (insert-in-front-hooks tempo-x-dont-grow-overlay))))))

(defun tempo-x-insert-form (vars &rest form)
  "Insert form to buffer"
  (setq form 
        `(let 
             ,(mapcar (lambda (var)
                        `(,var (tempo-x-overlay-text (tempo-x-find-source ',var))))
                      vars)
           ,@form))
  (let (ov)
    (setq ov (make-overlay (point) (point)))
    (mapc (lambda (pair)
            (overlay-put ov (car pair) (cdr pair)))
          `((tempo-x-form . ,form)
            (face . tempo-x-form-face)
            (modification-hooks tempo-x-delete-field)
            (insert-in-front-hooks tempo-x-dont-grow-overlay)))
    (mapc (lambda (name)
            (let ((source (tempo-x-find-source name))
                  forms)
              (when source
                (setq forms (overlay-get source 'tempo-x-forms))
                (push ov forms)
                (overlay-put source 'tempo-x-forms forms))))
          vars)
    (push ov tempo-x-snippet-forms)))

;;{{{  basic functions
(defun tempo-x-delete-overlay (ov)
  "Delete mirror field, add marker for navigator after deletion."
  (when (overlayp ov)
    (tempo-insert-mark (copy-marker (overlay-start ov)))
    (delete-overlay ov)))

(defun tempo-x-snippet-clear ()
  "Clear current snippet."
  (mapc (lambda (source)
          (setq source (cdr source))
          (mapc 'tempo-x-delete-overlay
                (overlay-get source 'tempo-x-mirrors))
          (and (overlayp source) (delete-overlay source)))
        tempo-x-snippet-sources)
  (mapc 'tempo-x-delete-overlay tempo-x-snippet-forms)
  (tempo-x-delete-overlay tempo-x-snippet-overlay)
  (setq tempo-x-snippet-sources nil
        tempo-x-snippet-overlay nil
        tempo-x-snippet-forms nil))

(defun tempo-x-find-source (name)
  "Return the source overlay with the NAME"
  (cdr (assq name tempo-x-snippet-sources)))

(defun tempo-x-overlay-text (ov)
  "Text of the overlay"
  (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))

(defun tempo-x-set-overlay-text (overlay text)
  "Change the text of the overlay"
  (save-excursion
    (let ((beg (overlay-start overlay)))
      (goto-char beg)
      (delete-region beg (overlay-end overlay))
      (insert text)
      (move-overlay overlay beg (point)))))

(defun tempo-x-clear-source (overlay)
  "Clear OVERLAY and its mirrors."
  (mapc 'tempo-x-delete-overlay
        (overlay-get overlay 'tempo-x-mirrors))
  (setq tempo-x-snippet-sources
        (delq (assq (overlay-get overlay 'tempo-x-name) tempo-x-snippet-sources)
              tempo-x-snippet-sources))
  (delete-overlay overlay))

(defun tempo-x-propagate-source (ov)
  "Change the mirrors and related forms."
  (let ((text (tempo-x-overlay-text ov))
        (mirrors (overlay-get ov 'tempo-x-mirrors))
        (forms (overlay-get ov 'tempo-x-forms)))
    (dolist (o mirrors)
      (unless (eq o ov)
        (tempo-x-set-overlay-text o text)))
    (dolist (o forms)
      (let ((text (eval (overlay-get o 'tempo-x-form))))
        (when text
          (tempo-x-set-overlay-text o text))))))
;;}}}

;;{{{  modification-hooks
(defun tempo-x-dont-grow-overlay (ov after-p beg end &optional r)
  "Hooks to make start of overlay unchange."
  (let ((inhibit-modification-hooks t))
    (when after-p
      (move-overlay ov end (overlay-end ov)))))

(defun tempo-x-delete-field (ov after-p beg end &optional r)
  "A wrapper to call `delete-overlay' from modification hooks."
  (if after-p
      (unless (string= tempo-x-delete-field-text
                       (buffer-substring-no-properties beg end))
        (tempo-x-delete-overlay ov))
    (setq tempo-x-delete-field-text
          (buffer-substring-no-properties beg end))))

(defun tempo-x-snippet-replace (ov after-p beg end &optional r)
  "Hooks to convert intangible overlay to ordinary"
  (when after-p
    (let ((inhibit-modification-hooks t))
      (mapc (lambda (pair)
              (overlay-put ov (car pair) (cdr pair)))
            `((intangible . nil)
              (modification-hooks tempo-x-snippet-update)
              (insert-behind-hooks tempo-x-snippet-update)
              (insert-in-front-hooks tempo-x-snippet-update)))
      (delete-region end (overlay-end ov))
      (tempo-x-snippet-update ov t beg end nil))))

(defun tempo-x-snippet-update (ov after-p beg end &optional r)
  "Update source overlay.
If insert chars in front or behind overlay that is in
`tempo-x-exclude-chars', don't grow the overlay.
Make mirrors and sources changes.
If the text become empty, if delete by deleting commands except DEL,
delete source field. Otherwise recover to beginning."
  (let ((inhibit-modification-hooks t))
    (when after-p
      ;; if the insert is not space, grow overlay
      (when (/= beg end)
        (cond ((= beg (overlay-start ov)) ; insert in the front
               (save-excursion
                 (goto-char beg)
                 (skip-chars-forward tempo-x-exclude-chars)
                 (move-overlay ov (point) (overlay-end ov))))
              ((> end (overlay-end ov)) ; insert in the end
               (save-excursion
                 (goto-char (overlay-end ov))
                 (skip-chars-forward (concat "^" tempo-x-exclude-chars))
                 (move-overlay ov (overlay-start ov) (min end (point)))))))
      (tempo-x-propagate-source ov)
      ;; if there is no text in the field
      (when (= (overlay-end ov) (overlay-start ov))
        (if (> r 1)
            ;; if delete a word, delete the overlay and mirrors
            (progn
              (tempo-x-clear-source ov))
          ;; if delete a single character, give back a prompt
          (tempo-x-set-overlay-text ov (overlay-get ov 'tempo-x-display))
          (tempo-x-propagate-source ov)
          (unless (overlay-get ov 'tempo-x-insert)
            (mapc (lambda (pair)
                    (overlay-put ov (car pair) (cdr pair)))
                  '((intangible . t)
                    (insert-behind-hooks)
                    (insert-in-front-hooks tempo-x-snippet-replace)
                    (modification-hooks tempo-x-snippet-update)))))))))
;;}}}

;;{{{  movement commands
(defun tempo-x-next-field ()
  "Move to next field, if already the next field clear all fields."
  (interactive)
  (let ((pos (point))
        (sources tempo-x-snippet-sources)
        (last (point-max))
        ov found)
    (while (and sources (not found))
      (if (and (> (overlay-start (cdar sources)) pos)
               (or (null (setq ov (cdr (cadr sources))))
                   (<= (overlay-start ov) pos)))
          (setq found t)
        (setq ov (car sources)
              sources (cdr sources))))
    (if found
        (goto-char (overlay-start (cdar sources)))
      (tempo-forward-mark)
      (tempo-x-snippet-clear))))

(defun tempo-x-previous-field ()
  "Move to previous field"
  (interactive)
  (let ((pos (point))
        (sources tempo-x-snippet-sources)
        ov found)
    (while (and sources (not found))
      (setq ov (cdar sources))
      (if (<= (overlay-end ov) pos)
          (setq found t)
        (setq sources (cdr sources))))
    (goto-char (overlay-start ov))))
;;}}}
;;}}}

(defmacro tempo-x-test-template (&rest tempo)
  "Test the template without define it."
  `(let ((tpl ,@tempo))
     (tempo-insert-template 'tpl nil)))

(add-to-list 'tempo-user-elements 'tempo-x-elements-handler)
(provide 'tempo-x)
;;; tempo-x.el ends here
