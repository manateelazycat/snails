;;; snails.el --- A modern, easy-to-expand fuzzy search framework

;; Filename: snails.el
;; Description: A modern, easy-to-expand fuzzy search framework
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2019, Andy Stewart, all rights reserved.
;; Created: 2019-05-16 21:26:09
;; Version: 0.1
;; Last-Updated: 2019-05-16 21:26:09
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/snails.el
;; Keywords:
;; Compatibility: GNU Emacs 26.1.92
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
;; 2019/05/16
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

;;; Code:

(defvar snails-input-buffer " *snails input*")

(defvar snails-content-buffer " *snails content*")

(defvar snails-frame nil)

(defvar snails-parent-frame nil)

(defvar snails-select-line-overlay nil)

(defvar snails-select-line-number 0)

(defvar snails-header-line-overlays nil)

(defcustom snails-mode-hook '()
  "snails mode hook."
  :type 'hook
  :group 'snails)

(defface snails-header-line-face
  '((t (:foreground "#3F90F7" :underline t :height 1.2)))
  "Face for header line"
  :group 'snails)

(defface snails-candiate-name-face
  '((t))
  "Face for candidate name"
  :group 'snails)

(defface snails-candiate-content-face
  '((t))
  "Face for candidate content"
  :group 'snails)

(defface snails-select-line-face
  '((t (:background "#3F90F7" :foreground "#FFF")))
  "Face for select line"
  :group 'snails)

(defvar snails-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") 'snails-quit)
    (define-key map (kbd "C-n") 'snails-select-next-item)
    (define-key map (kbd "C-p") 'snails-select-prev-item)
    (define-key map (kbd "C-m") 'snails-do)
    map)
  "Keymap used by `snails-mode'.")

(define-derived-mode snails-mode text-mode "snails"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'snails-mode)
  (setq mode-name "snails")
  (use-local-map snails-mode-map)
  (run-hooks 'snails-mode-hook))

(defun snails ()
  (interactive)
  (snails-create-input-buffer)
  (snails-create-content-buffer)
  (snails-search "")
  (snails-create-frame)
  )

(defun snails-create-input-buffer ()
  (with-current-buffer (get-buffer-create snails-input-buffer)
    (erase-buffer)
    (snails-mode)
    (buffer-face-set '(:background "#222" :foreground "gold" :height 250))
    (set-face-background 'hl-line "#222")
    (setq-local global-hl-line-overlay nil)
    (setq-local header-line-format nil)
    (setq-local mode-line-format nil)
    ))

(defun snails-create-content-buffer ()
  (with-current-buffer (get-buffer-create snails-content-buffer)
    (erase-buffer)
    (buffer-face-set '(:background "#111" :height 130))
    (setq-local header-line-format nil)
    (setq-local mode-line-format nil)
    (setq-local cursor-type nil)
    ))

(defun snails-monitor-input (begin end length)
  (when (string-equal (buffer-name) snails-input-buffer)
    (with-current-buffer snails-content-buffer
      (let* ((input (with-current-buffer snails-input-buffer
                      (buffer-substring (point-min) (point-max)))))
        (snails-search input)))))

(defun snails-create-frame ()
  (let* ((edges (frame-edges))
         (x (nth 0 edges))
         (y (nth 1 edges))
         (width (nth 2 edges))
         (height (nth 3 edges))
         (frame-width (truncate (* 0.6 width)))
         (frame-height (truncate (* 0.5 height)))
         (frame-x (/ (- width frame-width) 2))
         (frame-y (/ (- height frame-height) 4)))
    (setq snails-frame
          (make-frame
           '((minibuffer . nil)
             (visibility . nil)
             (internal-border-width . 0)
             )))

    (with-selected-frame snails-frame
      (set-frame-position snails-frame frame-x frame-y)
      (set-frame-size snails-frame frame-width frame-height t)
      (set-frame-parameter nil 'undecorated t)
      (split-window (selected-window) (nth 3 (window-edges (selected-window))) nil t)
      (switch-to-buffer snails-input-buffer)
      (set-window-margins (selected-window) 1 1)
      (other-window 1)
      (switch-to-buffer snails-content-buffer)
      (set-window-margins (selected-window) 1 1)
      (other-window 1)


      (add-hook 'after-change-functions 'snails-monitor-input nil t)
      )

    (setq snails-parent-frame (selected-frame))
    (make-frame-visible snails-frame)))

(defun snails-quit ()
  (interactive)
  (delete-frame snails-frame))

(defvar snails-backends nil)

(defvar snails-input-ticker 0)

(defvar snails-candiate-list nil)

(defvar snails-candiate-list-update-p nil)

(defun snails-search (input)
  (setq snails-input-ticker (+ snails-input-ticker 1))
  (setq snails-candiate-list (make-list (length snails-backends) nil))

  (dolist (backend snails-backends)
    (let ((search-func (cdr (assoc "search" (eval backend)))))
      (funcall search-func input snails-input-ticker 'snails-update-callback))))

(defun snails-update-callback (backend-name input-ticker candidates)
  (when (and (equal input-ticker snails-input-ticker)
             candidates)
    (let* ((backend-names (snails-get-backend-names))
           (backend-index (cl-position backend-name backend-names)))
      (when backend-index
        (setq snails-candiate-list (snails-update-list-by-index snails-candiate-list backend-index candidates))
        (snails-render-bufer)
        (setq snails-candiate-list-update-p t)
        ))))

(defun snails-get-backend-names ()
  (mapcar (lambda (b) (eval (cdr (assoc "name" (eval b))))) snails-backends))

(defun snails-render-bufer ()
  (with-current-buffer snails-content-buffer
    (erase-buffer)

    (setq snails-header-line-overlays nil)

    (setq snails-select-line-number 0)
    (setq snails-select-line-overlay (make-overlay (point) (point)))
    (overlay-put snails-select-line-overlay 'face `snails-select-line-face)

    (let ((candiate-index 0)
          (backend-names (snails-get-backend-names))
          header-line-start
          header-line-end
          candidate-name-start
          candidate-name-end
          candidate-content-start
          candidate-content-end
          )
      (dolist (candiate-list snails-candiate-list)
        (when candiate-list
          (setq header-line-start (point))
          (insert (format "%s\n" (nth candiate-index backend-names)))
          (backward-char)
          (setq header-line-end (point))
          (let ((header-line-overlay (make-overlay header-line-start header-line-end)))
            (add-to-list 'snails-header-line-overlays header-line-overlay)
            (overlay-put header-line-overlay
                         'face
                         'snails-header-line-face
                         ))
          (forward-char)

          (dolist (candiate candiate-list)
            (setq candidate-name-start (point))
            (insert (nth 0 candiate))
            (setq candidate-name-end (point))
            (overlay-put (make-overlay candidate-name-start candidate-name-end)
                         'face
                         'snails-candiate-name-face)

            (setq candidate-content-start (point))
            (insert (format "%s" (nth 1 candiate)))
            (setq candidate-content-end (point))
            (let ((candidate-content-overlay (make-overlay candidate-content-start candidate-content-end)))
              (overlay-put candidate-content-overlay 'display "")
              (overlay-put candidate-content-overlay
                           'face
                           'snails-candiate-content-face))
            (insert "\n")
            )
          (insert "\n"))
        (setq candiate-index (+ candiate-index 1)))

      (goto-char (point-min))
      (snails-select-next-item)
      )))

(defun snails-update-list-by-index (list n val)
  (nconc (subseq list 0 n)
         (cons val (nthcdr (1+ n) list))))

(defun snails-select-next-item ()
  (interactive)
  (with-current-buffer snails-content-buffer
    (goto-line snails-select-line-number)
    (snails-jump-to-next-item)
    (setq snails-select-line-number (line-number-at-pos))
    (move-overlay snails-select-line-overlay
                  (point-at-bol)
                  (point-at-eol))
    (snails-keep-cursor-visible)))

(defun snails-select-prev-item ()
  (interactive)
  (with-current-buffer snails-content-buffer
    (goto-line snails-select-line-number)
    (snails-jump-to-previous-item)
    (setq snails-select-line-number (line-number-at-pos))
    (move-overlay snails-select-line-overlay
                  (point-at-bol)
                  (point-at-eol))
    (snails-keep-cursor-visible)))

(defun snails-jump-to-next-item ()
  (forward-line)
  (while (and (not (eobp))
              (or
               (snails-empty-line-p)
               (snails-header-line-p)))
    (forward-line))
  (when (and (eobp)
             (snails-empty-line-p))
    (previous-line 2))
  )

(defun snails-do ()
  (interactive)
  (with-current-buffer snails-content-buffer
    (goto-line snails-select-line-number)
    (end-of-line)
    (backward-char)
    (let ((overlays (overlays-at (point))))
      (catch 'do
        (while overlays
          (let ((overlay (car overlays)))
            (when (eq (overlay-get overlay 'face) 'snails-candiate-content-face)
              (snails-backend-do
               (snails-get-candidate-backend-name (point))
               (buffer-substring (overlay-start overlay) (overlay-end overlay)))
              (throw 'do nil)
              ))
          (setq overlays (cdr overlays))))
      )))

(defun snails-get-candidate-backend-name (candidate-point)
  (catch 'backend-name
    (dolist (header-line-overlay snails-header-line-overlays)
      (when (> candidate-point (overlay-end header-line-overlay))
        (throw 'backend-name (buffer-substring (overlay-start header-line-overlay) (overlay-end header-line-overlay))))
      )))

(defun snails-backend-do (backend-name candidate)
  (catch 'backend-do
    (dolist (backend snails-backends)
      (let ((name (cdr (assoc "name" (eval backend))))
            (do-func (cdr (assoc "do" (eval backend)))))

        (when (equal (eval name) backend-name)
          (snails-quit)
          (funcall do-func candidate)
          (throw 'backend-do nil)
          )))))

(defun snails-keep-cursor-visible ()
  (when (get-buffer-window snails-content-buffer)
    (set-window-point (get-buffer-window snails-content-buffer) (point))))

(defun snails-jump-to-previous-item ()
  (previous-line)
  (while (and (not (bobp))
              (or
               (snails-empty-line-p)
               (snails-header-line-p)))
    (previous-line))
  (when (bobp)
    (forward-line)))

(defun snails-header-line-p ()
  (let ((overlays (overlays-at (point)))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (eq (overlay-get overlay 'face) 'snails-header-line-face)
            (setq found t)))
      (setq overlays (cdr overlays)))
    found))

(defun snails-empty-line-p ()
  (= (point-at-eol) (point-at-bol)))

(defun snails-wrap-buffer-icon (buf)
  (if (featurep 'all-the-icons)
      (format "%s %s"
              (with-current-buffer buf
                (all-the-icons-icon-for-buffer))
              (string-trim-left (buffer-name buf)))
    (buffer-name buf)))

(defun snails-wrap-file-icon (file)
  (if (featurep 'all-the-icons)
      (format "%s %s"
              (all-the-icons-icon-for-file file :height 1)
              (string-trim-left file))
    file))

(defmacro snails-create-backend (name candidate-search-function candiate-do-function)
  (let* ((backend-template-name (string-join (split-string (downcase name)) "-"))
         (backend-name (intern (format "snails-backend-%s" backend-template-name)))
         (search-function (intern (format "snails-backend-%s-search" backend-template-name))))
    `(progn
       (defun ,search-function(input input-ticker update-callback)
         (funcall
          update-callback
          ,name
          input-ticker
          (funcall ,candidate-search-function input)))

       (defvar ,backend-name
         '(("name" . ,name)
           ("search" . ,search-function)
           ("do" . ,candiate-do-function)
           )))))

(provide 'snails-core)

;;; snails.el ends here
