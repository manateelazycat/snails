;;; snails-backend-everything.el --- everything(windows, es.exe) backend for snails

;; Filename: snails-backend-everything.el
;; Description: Everything backend for snails
;; Author: Guoqiang Jin <ustczhan@gmail.com>
;; Maintainer: Guoqiang Jin <ustczhan@gmail.com>
;; Copyright (C) 2019, Andy Stewart, all rights reserved.
;; Created: 2019-07-31 12:07:44
;; Version: 0.1
;; Last-Updated: 2019-07-31 12:07:44
;;           By: Guoqiang Jin
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
;; Everything backend for snails
;;

;;; Installation:
;;
;; Put snails-backend-everything.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'snails-backend-everything)
;;
;; No need more.

;;; Change log:
;;
;; 2019/07/31
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

;;;Code:

(snails-create-async-backend
 :name
 "EVERYTHING"

 :build-command
 (lambda (input)
   (when (and (executable-find "es")
              (> (length input) 5))
     (list "es" input)))

 :candidate-filter
 (lambda (candidate-list)
   (let (candidates)
     (dolist (candidate candidate-list)
       (snails-add-candiate 'candidates (snails-wrap-file-icon candidate) candidate))
     candidates))

 :candiate-do
 (lambda (candidate)
   (find-file candidate)))

(provide 'snails-backend-everything)

;;; snails-backend-everything.el ends here
