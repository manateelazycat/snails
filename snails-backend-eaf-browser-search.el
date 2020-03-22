;;; snails-backend-eaf-browser-search.el --- Search or search in brwoser backend for snails

;; Filename: snails-backend-eaf-browser-search.el
;; Description: Buffer switch backend for snails
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2019, Andy Stewart, all rights reserved.
;; Created: 2019-07-23 16:43:17
;; Version: 0.1
;; Last-Updated: 2019-07-23 16:43:17
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/snails-backend-buffer.el
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
;; Buffer switch backend for snails
;;

;;; Installation:
;;
;; Put snails-backend-eaf-browser-search.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'snails-backend-buffer)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET snails-backend-buffer RET
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

(snails-create-sync-backend
 :name
 "EAF-BROWSER-SEARCH"

 :candidate-filter
 (lambda (input)
   (let (candidates)
     (when (and (ignore-errors (require 'eaf))
                (> (length input) 3))
       (unless (eaf-is-valid-url input)
         (snails-add-candiate 'candidates (format "Search in browser: %s" input) input)))
     candidates))

 :candidate-do
 (lambda (candidate)
   (eaf-search-it candidate)))

(provide 'snails-backend-eaf-browser-search)

;;; snails-backend-eaf-browser-search.el ends here
