;;; snails-backend-projectile.el --- Projectile backend for snails

;; Filename: snails-backend-projectile.el
;; Description: Projectile backend for snails
;; Author: Cosven <http://cosven.me>
;; Maintainer: Cosven <http://cosven.me>
;; Copyright (C) 2019, Andy Stewart, all rights reserved.
;; Created: 2019-08-07 05:50:41
;; Version: 0.1
;; Last-Updated: 2019-08-07 05:50:41
;;           By: Cosven
;; URL: http://www.emacswiki.org/emacs/download/snails-backend-projectile.el
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
;; Projectile backend for snails
;;

;;; Installation:
;;
;; Put snails-backend-projectile.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'snails-backend-projectile)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET snails-backend-projectile RET
;;

;;; Change log:
;;
;; 2019/08/07
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

(defun snails-backend-projectile-project-root ()
  "Find projectile root."
  (projectile-project-root (snails-start-buffer-dir)))

(defun snails-backend-projectile-candidates ()
  "List project files."
  (when (featurep 'projectile)
    (let ((project-root (snails-backend-projectile-project-root)))
      (when project-root
        (projectile-project-files project-root)))))

(snails-create-sync-backend
 :name
 "PROJECTILE"

 :candidate-filter
 (lambda (input)
   (let ((candidates)
         (project-root (snails-backend-projectile-project-root))
         (project-files (snails-backend-projectile-candidates)))
     (when project-files
       (dolist (file project-files)
         (when (or
                (string-equal input "")
                (snails-match-input-p input file))
           (setq file-path (expand-file-name file project-root))
           (snails-add-candiate 'candidates file file-path))))
     (snails-sort-candidates input candidates 1 1)))

 :candidate-icon
 (lambda (candidate)
   (snails-render-file-icon candidate))

 :candidate-do
 (lambda (candidate)
   (find-file candidate)))

(provide 'snails-backend-projectile)

;;; snails-backend-projectile.el ends here
