;;; snails-backend-google-suggestion.el --- EAF PDF backend for snails

;; Filename: snails-backend-google-suggestion.el
;; Description: EAF PDF backend for snails
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2020, Andy Stewart, all rights reserved.
;; Created: 2020-01-06 19:00:31
;; Version: 0.1
;; Last-Updated: 2020-01-06 19:00:31
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/snails-backend-eaf-pdf.el
;; Keywords:
;; Compatibility: GNU Emacs 26.3
;;
;; Features that might be required by this library:
;;
;;
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
;; EAF PDF backend for snails
;;

;;; Installation:
;;
;; Put snails-backend-google-suggestion.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'snails-backend-eaf-browser-history)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET snails-backend-eaf-browser-history RET
;;

;;; Change log:
;;
;; 2020/01/06
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
(require 'snails-core)

;;; Code:
(snails-create-async-backend
 :name
 "GOOGLE SUGGESTION"

 :build-command
 (lambda (input)
   (when (and (executable-find "curl")
              (> (length input) 3))
     (if (memq system-type '(windows-nt ms-dos))
         (list  "powershell.exe" "-ExecutionPolicy" "unrestricted"
                (concat (file-name-directory (locate-library "snails")) "google-suggestion.ps1")
                (replace-regexp-in-string "\\s-+" "%20" input))
       (list (concat (file-name-directory (locate-library "snails")) "google-suggestion.sh")
             (replace-regexp-in-string "\\s-+" "%20" input)))))

 :candidate-filter
 (lambda (candidate-list)
   (let (candidates)
     (dolist (candidate candidate-list)
       (snails-add-candiate 'candidates candidate candidate))
     candidates))

 :candidate-do
 (lambda (candidate)
   (eaf-search-it candidate)
   ))

(provide 'snails-backend-google-suggestion)

;;; snails-backend-google-suggestion.el ends here
