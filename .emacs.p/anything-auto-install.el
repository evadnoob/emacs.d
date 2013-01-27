;;; anything-auto-install.el --- Integrate auto-install.el with anything.el

;; Filename: anything-auto-install.el
;; Description: Integrate auto-install.el with anything.el
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-02-09 17:48:01
;; Version: 0.1
;; Last-Updated: 2009-02-09 17:48:01
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/anything-auto-install.el
;; Keywords: auto-install, anything
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `auto-install' `anything'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Integrate auto-install.el with anything.el.
;;
;; You can use command `anything-auto-install' install package
;; from EmacsWiki.org.
;;
;; You can also make this package integrate with `anything',
;; just setup like below:
;;
;; (setq anythign-sources
;;       (list
;;        anything-c-source-auto-install
;;        ))
;;

;;; Installation:
;;
;; Put anything-auto-install.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'anything-auto-install)
;;
;; And this package need `auto-install' and `anything',
;; make sure you have add package `auto-install' `auto-install'
;; in your load-path.
;;

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET anything-auto-install RET
;;

;;; Change log:
;;
;; 2009/02/09
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'anything)
(require 'auto-install)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar anything-c-source-auto-install
  '((name . "Auto Install from EmacsWiki")
    (candidates . auto-install-package-name-list)
    (action . (("Install from EmacsWiki.org" . (lambda (candidate)
                                                 (auto-install-download (concat auto-install-emacswiki-base-url candidate))))
               ("Update package name from EmacsWiki.org" . (lambda (candidate)
                                                             (auto-install-update-emacswiki-package-name)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun anything-auto-install ()
  "Launch anything with auto-install separately."
  (interactive)
  (anything 'anything-c-source-auto-install))

(provide 'anything-auto-install)

;;; anything-auto-install.el ends here
