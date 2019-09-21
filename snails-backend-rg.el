;;; snails-backend-rg.el --- Ripgrep backend for snails

;; Filename: snails-backend-rg.el
;; Description: Ripgrep backend for snails
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2019, Andy Stewart, all rights reserved.
;; Created: 2019-07-23 16:41:05
;; Version: 0.1
;; Last-Updated: 2019-07-23 16:41:05
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/snails-backend-rg.el
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
;; Ripgrep backend for snails
;;

;;; Installation:
;;
;; Put snails-backend-rg.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'snails-backend-rg)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET snails-backend-rg RET
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
 "RG"

 :build-command
 (lambda (input)
   (when (and (executable-find "rg")
              (> (length input) 5))
     (let ((search-dir snails-project-root-dir)
           (search-input input)
           (search-info (snails-pick-search-info-from-input input)))
       ;; If the user input character includes the path separator @, replace the current directory with the entered directory.
       (when search-info
         (setq search-dir (first search-info))
         (setq search-input (second search-info)))

       ;; Search.
       (when search-dir
         (list "rg" "--no-heading" "--column" "--color" "never" "--max-columns" "300" search-input search-dir)
         ))))

 :candidate-filter
 (lambda (candidate-list)
   (let (candidates)
     (dolist (candidate candidate-list)
       (snails-add-candiate
        'candidates
        (snails-wrap-file-icon-with-candidate
         (nth 0 (split-string candidate ":"))
         (string-remove-prefix (or snails-project-root-dir "") candidate))
        candidate))
     candidates))

 :candiate-do
 (lambda (candidate)
   (let ((file-info (split-string candidate ":")))
     (when (> (length file-info) 3)
       ;; Open file and jump to position.
       (find-file (nth 0 file-info))
       (goto-line (string-to-number (nth 1 file-info)))
       (move-to-column (max (- (string-to-number (nth 2 file-info)) 1) 0))

       ;; Flash match line.
       (snails-flash-line)
       ))))

(provide 'snails-backend-rg)

;;; snails-backend-rg.el ends here
