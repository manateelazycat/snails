;;; snails-extensions.el --- Extensions function for snails.

;; Filename: snails-extensions.el
;; Description: Extensions function for snails.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2020, Andy Stewart, all rights reserved.
;; Created: 2020-04-05 15:24:41
;; Version: 0.1
;; Last-Updated: 2020-04-05 15:24:41
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/snails-extensions.el
;; Keywords:
;; Compatibility: GNU Emacs 28.0.50
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
;; Extensions function for snails.
;;

;;; Installation:
;;
;; Put snails-extensions.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'snails-extensions)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET snails-extensions RET
;;

;;; Change log:
;;
;; 2020/04/05
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

(defun snails-candiate-alternate-do ()
  "Alternate do for current candidate."
  (interactive)
  (let ((candidate-info (snails-candidate-get-info)))
    (if candidate-info
        (let ((backend-name (nth 0 candidate-info))
              (candidate (nth 1 candidate-info)))
          (snails-quit)
          (with-selected-frame snails-init-frame
            (cond ((string-equal backend-name "EAF-BROWSER-HISTORY")
                   (cond
                    ;; If url starts with github, only pick `github.com/user/repo'
                    ((and (string-prefix-p "https://github.com/" candidate)
                          (> (length (split-string candidate "/")) 4))
                     (eaf-open-browser (string-join (subseq (split-string candidate "/") 2 5) "/")))
                    ;; Otherwise pick url host from url string.
                    (t
                     (eaf-open-browser (url-host (url-generic-parse-url candidate)))))
                   ))))
      (message "Nothing selected."))))

(define-key snails-mode-map (kbd "C-j") 'snails-candiate-alternate-do)

(provide 'snails-extensions)

;;; snails-extensions.el ends here
