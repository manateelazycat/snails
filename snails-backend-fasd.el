;;; snails-backend-fasd.el --- fasd backend for snails

;; Filename: snails-backend-fasd.el
;; Description: fasd backend for snails
;; Author: lyjdwh <lyjdwh@gmail.com>
;; Maintainer: lyjdwh <lyjdwh@gmail.com>
;; Copyright (C) 2019, Andy Stewart, all rights reserved.
;; Created: 2020-04-04 16:20:00
;; Version: 0.1
;; Last-Updated: 2020-04-04 16:20:00
;;           By: lyjdwh
;; URL: http://www.emacswiki.org/emacs/download/snails-backend-fasd.el
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
;; fasd backend for snails
;;

;;; Installation:
;;
;; Put snails-backend-fasd.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'snails-backend-fasd)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET snails-backend-fasd.el RET
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
 "FASD"

 :build-command
 (lambda (input)
   (when (and (executable-find "fasd")
              (> (length input) 3))
     (list "fasd" "-ld" (format "%s" input))))

 :candidate-filter
 (lambda (candidate-list)
   (let (candidates)
     (dolist (candidate candidate-list)
       (snails-add-candiate 'candidates candidate candidate))
     candidates))

 :candidate-do
 (lambda (candidate)
   (find-file candidate)))

(provide 'snails-backend-fasd)

;;; snails-backend-fasd.el ends here
