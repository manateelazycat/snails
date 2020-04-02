;;; snails-backend-bookmark.el --- Bookmark backend for snails

;; Filename: snails-backend-bookmark.el
;; Description: Bookmark backend for snails
;; Author: Xie Peng <pengpengxpplay@gmail.com>
;; Maintainer: Xie Peng <pengpengxpplay@gmail.com>
;; Copyright (C) 2019, Andy Stewart, all rights reserved.
;; Created: 2019-07-23 16:43:44
;; Version: 0.1
;; Last-Updated: 2019-07-23 16:43:44
;;           By: Xie Peng
;; URL: http://www.emacswiki.org/emacs/download/snails-backend-bookmark.el
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
;; Bookmark backend for snails
;;

;;; Installation:
;;
;; Put snails-backend-bookmark.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'snails-backend-bookmark)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET snails-backend-bookmark RET
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
(require 'bookmark)

;;; Code:

(snails-create-sync-backend
 :name
 "BOOKMARK"

 :candidate-filter
 (lambda (input)
   (let (candidates)
     (dolist (bookmark (bookmark-all-names))
       (when (or
              (string-equal input "")
              (snails-match-input-p input bookmark))
         (snails-add-candiate 'candidates bookmark bookmark)))
     (snails-sort-candidates input candidates 0 0)))

 :candidate-do
 (lambda (candidate)
   (bookmark-jump candidate)))

(provide 'snails-backend-bookmark)

;;; snails-backend-bookmark.el ends here
