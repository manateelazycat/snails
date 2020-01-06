;;; snails.el --- A modern, easy-to-expand fuzzy search framework

;; Filename: snails.el
;; Description: A modern, easy-to-expand fuzzy search framework
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2019, Andy Stewart, all rights reserved.
;; Created: 2019-07-20 01:21:07
;; Version: 0.1
;; Last-Updated: 2019-07-20 01:21:07
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/snails.el
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
;; A modern, easy-to-expand fuzzy search framework
;;

;;; Installation:
;;
;; Put snails.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'snails)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET snails RET
;;

;;; Change log:
;;
;; 2019/07/20
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
(require 'snails-backend-buffer)
(require 'snails-backend-current-buffer)
(require 'snails-backend-recentf)
(require 'snails-backend-awesome-tab-group)
(require 'snails-backend-fd)
(require 'snails-backend-mdfind)
(require 'snails-backend-imenu)
(require 'snails-backend-command)
(require 'snails-backend-bookmark)
(require 'snails-backend-rg)
(require 'snails-backend-everything)
(require 'snails-backend-projectile)
(require 'snails-backend-directory-files)
(require 'snails-backend-eaf-pdf-table)

(provide 'snails)
;;; snails.el ends here
