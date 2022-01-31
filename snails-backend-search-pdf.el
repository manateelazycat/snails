;;; snails-backend-search-pdf.el --- search pdf with rga

;; Filename: snails-backend-search-pdf.el
;; Description: search pdf with rga
;; Author: lyjdwh <lyjdwh@gmail.com>
;; Maintainer: lyjdwh <lyjdwh@gmail.com>
;; Copyright (C) 2019, Andy Stewart, all rights reserved.
;; Created: 2020-07-29 12:00
;; Version: 0.1
;; Last-Updated: 2020-07-29 12:00:00
;;           By: lyjdwh
;; URL: http://www.emacswiki.org/emacs/download/snails-backend-search-pdf.el
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
;; fasd backend for snails
;;

;;; Installation:
;;
;; Put snails-backend-fasd.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'snails-backend-search-pdf)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET snails-backend-fasd.el RET
;;

;;; Change log:
;;
;; 2019/07/23
;;      * First released.
;;

(require 'snails-core)

(defvar snails-backend-search-pdf-dir nil
  "the dir which snails-backend-search-pdf search at.")

(snails-create-async-backend
 :name
 "search-pdf"

 :build-command
 (lambda (input)
   (when (and (executable-find "rga")
              (> (length input) 3))
     (let ((search-dir (or snails-project-root-dir (expand-file-name (snails-start-buffer-dir) )))
           (search-input input)
           (search-info (snails-pick-search-info-from-input input)))

       ;; If the user input character includes the path separator @, replace the current directory with the entered directory.
       (when search-info
         (setq search-dir (cl-first search-info))
         (setq search-input (cl-second search-info)))

       (when (memq system-type '(cygwin windows-nt ms-dos))
         (setq search-input (encode-coding-string search-input locale-coding-system))
         (setq search-dir (encode-coding-string search-dir locale-coding-system)))

       (setq snails-backend-search-pdf-dir search-dir)
       (message (format "searching pdf at %s" search-dir))

       ;; Search.
       (when search-dir
         (list "rga" "--no-heading" "--column" "--color" "never" "--max-columns" "300" "--rga-adapters=poppler" search-input search-dir)
         ))))

 :candidate-filter
 (lambda (candidate-list)
   (let (candidates)
     (dolist (candidate candidate-list)
       (let ((candidate-info (split-string candidate ":")))
         (snails-add-candiate
          'candidates
          (format "%s: %s"
                  (file-relative-name (nth 0 candidate-info) snails-backend-search-pdf-dir )
                  (string-join (cddr candidate-info)))
          candidate)))
     candidates))

 :candidate-icon
 (lambda (candidate)
   (snails-render-search-file-icon (nth 0 (split-string candidate ":"))))

 :candidate-do
 (lambda (candidate)
   (let ((file-info (split-string candidate ":")))
     (when (> (length file-info) 3)
       ;; Open pdf and jump to page.
       (if (require 'eaf nil 'noerror)
           (progn
             (eaf-open (nth 0 file-info))
             (eaf-call-async "handle_input_message"
                       eaf--buffer-id
                       "jump_page"
                       (string-to-number (substring (nth 2 file-info) 5))))
         (progn
           (find-file (nth 0 file-info))
           (pdf-view-goto-page (string-to-number (substring (nth 2 file-info) 5)))))

       ;; Flash match line.
       (snails-flash-line)
       ))))

(provide 'snails-backend-search-pdf)
