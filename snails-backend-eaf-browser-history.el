;;; snails-backend-eaf-browser-history.el --- EAF PDF backend for snails

;; Filename: snails-backend-eaf-browser-history.el
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
;; Put snails-backend-eaf-browser-history.el to your load-path.
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
(snails-create-sync-backend
 :name
 "EAF-BROWSER-HISTORY"

 :candidate-filter
 (lambda (input)
   (let (candidates)
     (with-current-buffer snails-start-buffer
       (when (ignore-errors (require 'eaf))
         (let ((browser-history-file-path (concat user-emacs-directory
                                                  (file-name-as-directory "eaf")
                                                  (file-name-as-directory "browser")
                                                  (file-name-as-directory "history")
                                                  "log.txt")))
           (when (file-exists-p browser-history-file-path)
             (with-temp-buffer
               (insert-file-contents browser-history-file-path)
               (beginning-of-buffer)

               (while (not (eobp))
                 (beginning-of-line)
                 (when (or
                        (string-equal input "")
                        (snails-match-input-p input (buffer-substring (point-at-bol) (point-at-eol))))
                   (snails-add-candiate 'candidates
                                        (buffer-substring (point-at-bol) (point-at-eol))
                                        (buffer-substring (point-at-bol) (point-at-eol))))
                 (forward-line 1)))))))
     (snails-sort-candidates input candidates 0 0)
     candidates
     ))

 :candiate-do
 (lambda (candidate)
   (with-temp-buffer
     (insert candidate)
     (beginning-of-buffer)
     (when (search-forward-regexp "\\s-\\(http\\|ftp\\)" (point-at-eol) t)
       (backward-word 1)
       (eaf-open-browser (buffer-substring (point) (point-at-eol)))
       ))))

(provide 'snails-backend-eaf-browser-history)

;;; snails-backend-eaf-browser-history.el ends here
