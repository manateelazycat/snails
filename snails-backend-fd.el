;; Description: fd backend for snails
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2019, Andy Stewart, all rights reserved.
;; Created: 2019-07-23 16:42:52
;; Version: 0.1
;; Last-Updated: 2019-07-23 16:42:52
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/snails-backend-fd.el
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
;; fd backend for snails
;;

;;; Installation:
;;
;; Put snails-backend-fd.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'snails-backend-fd)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET snails-backend-fd RET
;;

;;; Change log:
;;
;; 2019/09/10
;;      * Make fd backend support search file content in the customize search directory.
;; 2019/07/23
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;; 1. asyn udpate rg search results back to the candidate-filter
;;
;;


(require 'subr-x)
(defvar snails-backend-fd-search-info nil
  "The latest search info from the user.")
(defvar snails-backend-fd-home-path (expand-file-name (getenv "HOME"))
  "The root path for fd to search directories when the combine-search is applied. You can customize it for your own usage.")

(snails-create-async-backend
 :name
 "FD"

 :build-command
 (lambda (input)
   (setq snails-backend-fd-search-info (split-string input "@"))
   (if (> (length snails-backend-fd-search-info) 1)
       ;; combine search
       (when (and (executable-find "fd")
                  (executable-find "rg")
                  (> (length (nth 0 snails-backend-fd-search-info)) 5)
                  (> (length (nth 1 snails-backend-fd-search-info)) 5))
         (list "fd" "-c" "never" "-a" "-td" "--full-path" (nth 1 snails-backend-fd-search-info) "--search-path" snails-backend-fd-home-path))
     ;; fd search only
     (when (and (executable-find "fd")
                (> (length input) 5))
       (list "fd" "-c" "never" "-a" "-tf" input "--search-path" (or snails-project-root-dir (expand-file-name default-directory)))
       )))


 :candidate-filter
 (lambda (candidate-list)
   (if (< (length  snails-backend-fd-search-info) 2)
       ;; fd search only
       (let (candidates)
         (dolist (candidate candidate-list)
           (snails-add-candiate
            'candidates
            (snails-wrap-file-icon-with-candidate candidate candidate)
            candidate))
         candidates)

     ;; combine search
     (let (candidates)
       (dolist (candidate candidate-list)
         (let ((rg-search-str "rg --no-heading --color never --column --max-columns 300 " ))
           (setq rg-search-str(concat rg-search-str  (nth 0 snails-backend-fd-search-info) " " candidate ))
           (let ((file-list (butlast (split-string (shell-command-to-string rg-search-str) "\n"))))
             (when file-list
               ;; add folder. todo: show number of found files? fast-jump among folders?
               (snails-add-candiate
                'candidates
                (snails-wrap-file-icon-with-candidate
                 candidate
                 (string-remove-prefix (or snails-backend-fd-home-path "") candidate))
                candidate)
               ;; add single file
               (dolist (f file-list)
                 (snails-add-candiate
                  'candidates
                  (snails-wrap-file-icon-with-candidate
                   (nth 0 (split-string f ":"))
                   (string-remove-prefix (or snails-backend-fd-home-path "") f))
                  f))))))
       candidates)
     ))


 :candiate-do
 (lambda (candidate)
   (if (file-directory-p candidate )
       ;; folder
       (find-file candidate)
     ;; file
     (let ((file-info (split-string candidate ":")))
       (when (> (length file-info) 3)
         ;; Open file and jump to position.
         (find-file (nth 0 file-info))
         (goto-line (string-to-number (nth 1 file-info)))
         (move-to-column (max (- (string-to-number (nth 2 file-info)) 1) 0))
         ;; Flash match line.
         (snails-flash-line)
         )))
   ))

(provide 'snails-backend-fd)
