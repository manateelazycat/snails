;;; snails-backend-directory-files.el --- Directory files backend for snails

;; Filename: snails-backend-directory-files.el
;; Description: Rencent files backend for snails
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2019, Andy Stewart, all rights reserved.
;; Created: 2019-09-21 6:41:46
;; Version: 0.1
;; Last-Updated: 2019-09-21 6:41:46
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/snails-backend-directory-files.el
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
;; Rencent files backend for snails
;;

;;; Installation:
;;
;; Put snails-backend-directory-files.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'snails-backend-directory-files)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET snails-backend-directory-files RET
;;

;;; Change log:
;;
;; 2019/09/21
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

(snails-create-sync-backend
 :name
 "DIRECTORY FILES"

 :candidate-filter
 (lambda (input)
   (let ((current-directory (snails-start-buffer-dir))
         filepath
         candidates)
     (dolist (file (cddr (directory-files current-directory)))
       (when (or
              (string-equal input "")
              (snails-match-input-p input file))
         (setq filepath (concat current-directory file))
         (snails-add-candiate 'candidates (snails-wrap-file-icon file) filepath)))
     (snails-sort-candidates input candidates 1 1)
     candidates))

 :candiate-do
 (lambda (candidate)
   (find-file candidate)))

(provide 'snails-backend-directory-files)

;;; snails-backend-directory-files.el ends here
