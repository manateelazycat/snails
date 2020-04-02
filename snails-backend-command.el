;;; snails-backend-command.el --- Command backend for snails

;; Filename: snails-backend-command.el
;; Description: Command backend for snails
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2019, Andy Stewart, all rights reserved.
;; Created: 2019-08-25 12:36:06
;; Version: 0.1
;; Last-Updated: 2019-08-25 12:36:06
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/snails-backend-command.el
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
;; Command backend for snails
;;

;;; Installation:
;;
;; Put snails-backend-command.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'snails-backend-command)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET snails-backend-command RET
;;

;;; Change log:
;;
;; 2019/08/25
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
(defvar snails-backend-command-list nil)

(defvar snails-backend-command-filter-number 20)

(defun snails-backend-command-get-commands ()
  (let (cmds)
    (mapatoms (lambda (s) (when (commandp s) (push (symbol-name s) cmds))))
    (setq snails-backend-command-list cmds)))

(add-hook 'after-init-hook 'snails-backend-command-get-commands)

(run-with-idle-timer 2 nil 'snails-backend-command-get-commands)

(run-with-idle-timer 60 t 'snails-backend-command-get-commands)

(defun snails-backend-command-wrap-command-with-key (command)
  (let ((keys (mapconcat
               'key-description
               (where-is-internal (intern command))
               " ")))
    (if (equal keys "")
        command
      (format "%s 「 %s 」" command keys))))

(snails-create-sync-backend
 :name
 "COMMAND"

 :candidate-filter
 (lambda (input)
   (let (candidates)
     (catch 'search-end
       (dolist (command snails-backend-command-list)
         (when (or
                (string-equal input "")
                (snails-match-input-p input command))
           (snails-add-candiate 'candidates (snails-backend-command-wrap-command-with-key command) command)

           (when (> (length candidates) snails-backend-command-filter-number)
             (throw 'search-end nil))
           )))
     (snails-sort-candidates input candidates 0 0)))

 :candidate-do
 (lambda (candidate)
   (call-interactively (intern candidate))))

(provide 'snails-backend-command)

;;; snails-backend-command.el ends here
