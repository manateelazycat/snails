;;; snails-backend-imenu.el --- IMenu backend for snails

;; Filename: snails-backend-imenu.el
;; Description: IMenu backend for snails
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2019, Andy Stewart, all rights reserved.
;; Created: 2019-07-26 08:03:06
;; Version: 0.1
;; Last-Updated: 2019-07-26 08:03:06
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/snails-backend-imenu.el
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
;; IMenu backend for snails
;;

;;; Installation:
;;
;; Put snails-backend-imenu.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'snails-backend-imenu)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET snails-backend-imenu RET
;;

;;; Change log:
;;
;; 2019/07/26
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
(require 'imenu)

;;; Code:

(defvar snails-backend-imenu-cached-candidates nil)
(defvar snails-backend-imenu-cached-buffer nil)

(defun snails-backend-imenu-candidates (buffer)
  (with-current-buffer buffer
    (prog1
        (if
            ;; Use cache candidates when `snails-backend-imenu-cached-candidates' is non-nil.
            ;; Need re-generate cache when user switch different buffer.
            (and snails-backend-imenu-cached-candidates
                 (or
                  (not snails-backend-imenu-cached-buffer)
                  (equal snails-backend-imenu-cached-buffer buffer)))
            snails-backend-imenu-cached-candidates
          (setq snails-backend-imenu-cached-candidates
                (let ((index (ignore-errors (imenu--make-index-alist t))))
                  (when index
                    (snails-backend-imenu-build-candidates
                     (delete (assoc "*Rescan*" index) index))))))
      (setq snails-backend-imenu-cached-buffer buffer))))

(defun snails-backend-imenu-build-candidates (alist)
  (cl-remove-if
   (lambda (c)
     (or (string-equal (car c) "Types")
         (string-equal (car c) "Variables")
         ))
   (cl-loop for elm in alist
            nconc (cond
                   ((imenu--subalist-p elm)
                    (snails-backend-imenu-build-candidates
                     (cl-loop for (e . v) in (cdr elm) collect
                              (cons
                               (car elm)
                               (if (integerp v) (copy-marker v) v)))))
                   ((listp (cdr elm))
                    (and elm (list elm)))
                   (t
                    (and (cdr elm)
                         (setcdr elm (pcase (cdr elm)
                                       ((and ov (pred overlayp))
                                        (copy-overlay ov))
                                       ((and mk (or (pred markerp)
                                                    (pred integerp)))
                                        (copy-marker mk))))
                         (list elm)))))))

(snails-create-sync-backend
 :name
 "IMENU"

 :candidate-filter
 (lambda (input)
   (let ((imenu-items (snails-backend-imenu-candidates snails-start-buffer))
         candidates)
     (dolist (imenu-item imenu-items)
       (when (or
              (string-equal input "")
              (snails-match-input-p input (car imenu-item)))

         (snails-add-candiate 'candidates
                              (car imenu-item)
                              (number-to-string (marker-position (cdr imenu-item))))))

     (snails-sort-candidates input candidates 0 0)
     candidates))

 :candiate-do
 (lambda (candidate)
   (goto-char (string-to-number candidate))

   (snails-flash-line)))

(provide 'snails-backend-imenu)

;;; snails-backend-imenu.el ends here
