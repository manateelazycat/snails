;;; snails-backend-fd.el --- fd backend for snails

;; Filename: snails-backend-fd.el
;; Description: fd backend for snails
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2019, Andy Stewart, all rights reserved.
;; Created: 2019-07-23 16:42:52
;; Version: 0.1
;; Last-Updated: 2019-07-23 16:42:52
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/snails-backend-fd.el
;; Keywords:
;; Compatibility: GNU Emacs 26.2
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
;; fd backend for snails
;;

;;; Installation:
;;
;; Put snails-backend-fd.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'snails-backend-fd)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET snails-backend-fd RET
;;

;;; Change log:
;;
;; 2019/07/23
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
 "FD"

 :build-command
 (lambda (input)
   (when (and (executable-find "fd")
              (> (length input) 5))
     (let ((search-dir (or snails-project-root-dir (snails-start-buffer-dir)))
           (search-input input)
           (search-info (snails-pick-search-info-from-input input)))
       ;; If the user input character includes the path separator @, replace the current directory with the entered directory.
       (when search-info
         (setq search-dir (first search-info))
         (setq search-input (second search-info)))

       (when (memq system-type '(cygwin windows-nt ms-dos))
         (setq search-input (encode-coding-string search-input locale-coding-system))
         (setq search-dir (encode-coding-string search-dir locale-coding-system)))

       (list "fd" "-c" "never" "-a" "-tf" search-input "--search-path" search-dir))
     ))

 :candidate-filter
 (lambda (candidate-list)
   (let (candidates)
     (dolist (candidate candidate-list)
       (snails-add-candiate 'candidates candidate candidate))
     candidates))

 :candidate-icon
 (lambda (candidate)
   (snails-render-search-file-icon candidate candidate))

 :candidate-do
 (lambda (candidate)
   (find-file candidate)
   ))

(provide 'snails-backend-fd)

;;; snails-backend-fd.el ends here
