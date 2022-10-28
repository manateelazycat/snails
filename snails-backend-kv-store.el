;;; snails-backend-kv-store.el --- Key-Value Store backend for snails

;; Filename: snails-backend-kv-store.el
;; Description: Key-value Store backend for snails
;; Author: EdmondFrank <Edmomdfrank@yahoo.com>
;; Maintainer: EdmondFrank <Edmomdfrank@yahoo.com>
;; Copyright (C) 2022, Edmondfrank, all rights reserved.
;; Created: 2022-10-26 12:00:00
;; Version: 0.1
;; Last-Updated: 2022-10-28 19:00:31
;; URL: http://www.emacswiki.org/emacs/download/snails-backend-kv-store.el
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
;; Key-Value Store backend for snails
;;

;;; Installation:
;;
;; Put snails-backend-kv-store.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'snails-backend-kv-store)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET snails-backend-kv-store RET
;;

;;; Require
(require 'snails-core)
(require 'emacsql)
(require 'emacsql-sqlite)

;;; Code:

(defvar snails-backend-kv-store-db
  (emacsql-sqlite (expand-file-name "snails.db" user-emacs-directory)))

(emacsql snails-backend-kv-store-db
         [:create-table
          :if-not-exists
          kvstore ([(key string :primary-key :unique) (value string)])])

(snails-create-sync-backend
 :name
 "kv-store"

 :candidate-filter
 (lambda (input)
   (let (candidates)
     (dolist (row
              (emacsql
               snails-backend-kv-store-db
               [:select [key value] :from kvstore :where (like key $r1)] (concat "%" input "%")))
       (snails-add-candiate 'candidates (car row) (cadr row)))

     (if (equal (string-match "\\(set\s[^\s]+\s[^\s]+\\)" input) 0)
         (snails-add-candiate 'candidates input input))

     (if (equal (string-match "\\(del\s[^\s]+\\)" input) 0)
         (snails-add-candiate 'candidates input input))
     candidates))

 :candidate-icon
 (lambda (candidate)
   (snails-render-faicon-icon "database"))

 :candidate-do
 (lambda (candidate)
   (if (equal (string-match "\\(set\s[^\s]+\s[^\s]+\\)" candidate) 0)
       (save-match-data ; is usually a good idea
         (and (string-match "set\s\\([^\s]+\\)\s\\([^\s]+\\)" candidate))
         (let* ((key (match-string 1 candidate))
                (value (match-string 2 candidate)))
           (if (equal 1 (caar (emacsql
                               snails-backend-kv-store-db
                               [:select 1 :from kvstore :where (= key $r1)] (format "\"%s\"" key))))
               (progn
                 (emacsql snails-backend-kv-store-db
                          [:update kvstore :set (= value $r1) :where (= key $r2)] value (format "\"%s\"" key))
                 (message "Updated kv-pair (%s %s) successfully" key value))
             (progn
               (emacsql snails-backend-kv-store-db
                        [:insert :into kvstore :values $v1] (list (vector key value)))
               (message "Inserted kv-pair (%s %s) successfully" key value)))))
     (kill-new candidate))

   (if (equal (string-match "\\(del\s[^\s]+\\)" candidate) 0)
       (save-match-data
         (and (string-match "del\s\\([^\s]+\\)" candidate))
         (let* ((key (match-string 1 candidate)))
           (if (equal 1 (caar (emacsql
                               snails-backend-kv-store-db
                               [:select 1 :from kvstore :where (= key $r1)] (format "\"%s\"" key))))
               (progn
                 (emacsql snails-backend-kv-store-db
                          [:delete :from kvstore :where (= key $r1)] (format "\"%s\"" key))
                 (message "Deleted key: %s  successfully" key))
             (message "No such key: %s" key)))))))

(provide 'snails-backend-kv-store)

;;; snails-backend-kv-store.el ends here
