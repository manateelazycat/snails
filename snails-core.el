;; -*- lexical-binding: t; -*-
;; ;;; snails-core.el --- A modern, easy-to-expand fuzzy search framework

;; Filename: snails-core.el
;; Description: A modern, easy-to-expand fuzzy search framework
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2019, Andy Stewart, all rights reserved.
;; Created: 2019-05-16 21:26:09
;; Version: 6.6
;; Last-Updated: 2019-12-05 18:31:01
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/snails-core.el
;; Keywords:
;; Compatibility: GNU Emacs 26.1.92
;;
;; Features that might be required by this library:
;;
;; `cl-lib' `subr-x'
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
;; If you are using Mac, install exec-path-from-shell from https://github.com/purcell/exec-path-from-shell.
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
;; 2019/12/05
;;      * Within two seconds of the call only once snails function, to avoid problems start blinking twice interface under KDE environment.
;;
;; 2019/09/01
;;      * We need init `snails-candiate-list' with `snails-backends', otherwise first launch will fail.
;;
;; 2019/08/27
;;      * Make `snails' function support customize search string.
;;      * Fix search-object condition order.
;;      * Add new options `snails-default-backends' and `snails-prefix-backends'.
;;      * Fix issue #29
;;
;; 2019/08/25
;;      * Support search content with input prefix.
;;
;; 2019/08/20
;;      * Call `snails-init-face-with-theme' when user execute snails command.
;;
;; 2019/08/08
;;      * Run snails-mode-hook in `snails-create-input-buffer', evil users should be hook evil code after `snails-mode-hook'.
;;
;; 2019/07/29
;;      * Make `snails-mode-hook' works, sorry i forgot add `run-hooks'.
;;
;; 2019/07/28
;;      * Optimize performance: fixed rendering every 100 milliseconds, instead of rendering once backend return candidates, avoiding rendering computation waste.
;;      * `snails-select-line-number' is not need anymore, `snails-select-line-overlay' is enough.
;;      * Add `snails-render-bufer' to timer when first start.
;;      * Keep offset of selected candidate.
;;      * Optimize start performance: delay 50 milliseconds to start search backend.
;;
;; 2019/07/26
;;      * Foucs out to hide snails frame on Mac.
;;      * Snails will search symbol around point when you press prefix key before call snails.
;;      * Make async process buffer's name starts with " *" to hide process buffer tab when search.
;;      * Make `snails' support backends and search-object arguments.
;;      * Add new command `snails-search-point'.
;;      * Add `snails-flash-line'.
;;      * Improve `snails-sort-candidates'
;;      * Add option `snails-fame-width-proportion' and `snails-fame-height-proportion'.
;;
;; 2019/07/25
;;      * Set undecorated parameter in `make-frame' function.
;;      * Try to raise snails frame when focus default frame by alt + tab switcher of OS.
;;      * Quit snails when lost input focus.
;;      * Support ansi color from asynchronous backend process.
;;      * Adjust ansi color code.
;;
;; 2019/07/24
;;      * Don't ask user when snails kill buffer of backend process.
;;      * Test GUI environment when start snails.
;;      * Don't wrap long line in content buffer.
;;      * Call `exec-path-from-shell' at snails-core.el
;;      * Use fuzz match algorithm provide by `fuz' libary.
;;
;; 2019/07/23
;;      * Kill old subprocess immediately, don't wait `run-with-idle-timer'
;;      * Split input window with on line height.
;;      * Make color along with current theme.
;;      * Quit snails if it has opened.
;;      * Add device to disable window configuration change snail frame.
;;      * Exit snails when enter to minibuffer.
;;      * Add new command `snails-candidate-copy'
;;      * Add parent-frame parameter, then snails frame won't hide when switch to other application.
;;
;; 2019/07/22
;;      * Delete other window first, make sure only one window in frame.
;;      * Finish `snails-select-next-backend' and `snails-select-prev-backend'
;;      * Use setq in macro, we can update backend code later.
;;      * Make `snails' support customize backend.
;;      * Fixed error that `set-buffer' on killed buffer.
;;      * Use `expand-file-name' expand default-directory, fd don't like unexpand directory.
;;      * Fix selected delete buffer error when call `buffer-string' in `snails-create-async-process'
;;      * Give up creating subprocess if input ticker already expired.
;;      * Kill all subprocess and process buffers when call `snails-quit'
;;      * Fix bug that select previous candidate item will select header line sometimes.
;;      * Add header index after header line.
;;      * Use `run-with-idle-timer' instead `run-with-timer' to improve performance of subprocess search.
;;      * 0.1 second is enough for `run-with-idle-timer'.
;;      * Fix `string-join' depend.
;;      * Disable scrollbar and fringle in new frame even user not disable them in theme.
;;
;; 2019/07/20
;;      * Finish document.
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
(require 'cl-lib)
(require 'subr-x)

(when (featurep 'cocoa)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;;; Code:

(defcustom snails-mode-hook '()
  "Snails mode hook."
  :type 'hook
  :group 'snails)

(defcustom snails-fame-width-proportion 0.618
  "The width of snails frame, width ratio of the parent frame."
  :type 'integer
  :group 'snails)

(defcustom snails-fame-height-proportion 0.618
  "The height of snails frame, height ratio of the parent frame."
  :type 'integer
  :group 'snails)

(defcustom snails-default-backends
  '(snails-backend-awesome-tab-group snails-backend-buffer snails-backend-recentf snails-backend-eaf-pdf-table snails-backend-directory-files snails-backend-bookmark)
  "The default backend"
  :type 'cons
  :group 'snails)

(defcustom snails-prefix-backends
  '((">" '(snails-backend-command))
    ("@" '(snails-backend-imenu))
    ("#" '(snails-backend-current-buffer))
    ("!" '(snails-backend-rg))
    ("?" '(snails-backend-projectile snails-backend-fd snails-backend-mdfind snails-backend-everything)))
  "The prefix/backends pair."
  :type 'cons
  :group 'snails)

(defface snails-header-line-face
  '((t (:inherit font-lock-function-name-face :underline t :height 1.3)))
  "Face for header line"
  :group 'snails)

(defface snails-header-index-face
  '((t (:inherit font-lock-function-name-face :underline t)))
  "Face for header index"
  :group 'snails)

(defface snails-candiate-content-face
  '((t))
  "Face for candidate content.
Note, candidate name is display name you can see in content buffer.
Candidate content use for confirm, it's invisible, it doesn't
need to set face attribute, such as foreground and background."
  :group 'snails)

(defface snails-select-line-face
  '((t (:inherit region)))
  "Face for select line."
  :group 'snails)

(defface snails-input-buffer-face
  '((t (:height 250)))
  "Face for input area."
  :group 'snails)

(defface snails-content-buffer-face
  '((t (:height 130)))
  "Face for content area."
  :group 'snails)

(defface snails-copy-candidate-face
  '((t (:foreground "Gold" :bold t)))
  "Face copy candidate."
  :group 'snails)

(defvar snails-input-buffer " *snails input*"
  "The buffer name of search input buffer.")

(defvar snails-content-buffer " *snails content*"
  "The buffer name of search content buffer.")

(defvar snails-frame nil
  "The popup frame use for show search result.")

(defvar snails-start-buffer nil
  "The buffer before snails start.")

(defvar snails-start-buffer-lines nil
  "The line number of start buffer.")

(defvar snails-frame-active-p nil
  "The parent frame of popup frame.")

(defvar snails-frame-active-time 0
  "The last time of snails frame active.")

(defvar snails-select-line-overlay nil
  "Select line overlay, use to highlight selected candidate.")

(defvar snails-header-line-overlays nil
  "The list overlay to render backend header line.")

(defvar snails-backends nil
  "Contain the real backends use in `snails'.")

(defvar snails-search-backends nil
  "Search backends, default is nil will search with input prefix.
Or backends pass from function `snails'.")

(defvar snails-input-ticker 0
  "Input ticker to unique search request.
If a search result return with old input ticker,
search result will be drop.")

(defvar snails-candiate-list nil
  "The list to contain candidate list,
use for find candidate position to change select line.")

(defvar snails-backend-subprocess-hash
  (make-hash-table :test 'equal)
  "The hash table contain the subprocess of async backend.")

(defvar snails-fuz-library-load-status "uncheck"
  "The variable use for check `fuz' library is load.

Init status with `uncheck'.
If `fuz' library is not found, set with `unload'.
If `fuz' library has load, set with `load'.")

(defvar snails-project-root-dir nil
  "The project dir when start snails.")

(defvar snails-need-render nil
  "Mark if you need to re-render after the word selection.")

(defvar snails-select-backend-name nil
  "Record backend name of selected candidate.")

(defvar snails-select-candidate-offset nil
  "Record candidate offset of selected candidate.")

(defvar snails-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") 'snails-quit)
    (define-key map (kbd "ESC ESC ESC") 'snails-quit)
    (define-key map (kbd "M-h") 'snails-quit)
    (define-key map (kbd "C-n") 'snails-select-next-item)
    (define-key map (kbd "C-p") 'snails-select-prev-item)
    (define-key map (kbd "M-n") 'snails-select-next-item)
    (define-key map (kbd "M-p") 'snails-select-prev-item)
    (define-key map (kbd "C-v") 'snails-select-next-backend)
    (define-key map (kbd "M-v") 'snails-select-prev-backend)
    (define-key map (kbd "M-j") 'snails-select-next-backend)
    (define-key map (kbd "M-k") 'snails-select-prev-backend)
    (define-key map (kbd "C-m") 'snails-candidate-do)
    (define-key map (kbd "RET") 'snails-candidate-do)
    map)
  "Keymap used by `snails-mode'.")

(define-derived-mode snails-mode text-mode "snails"
  (interactive)
  ;; Kill all local variables.
  (kill-all-local-variables)
  ;; Switch new mode.
  (setq major-mode 'snails-mode)
  (setq mode-name "snails")
  ;; Injection keymap.
  (use-local-map snails-mode-map))

(defun snails (&optional backends search-object)
  "Start snails to search.

`backends' is list of backend, default is nil, you can customize your own search backend list.

`search-object', default is nil, nothing fill in input buffer,
you can set `search-object' with t to search symbol around point,
or set it with any string you want."
  (interactive)
  (if (and
       ;; Only running snails when in GUI environment.
       (display-graphic-p)
       ;; Only running once in 2 seconds.
       (> (- (float-time) snails-frame-active-time) 2))
      (if (and
           snails-frame
           (frame-live-p snails-frame))
          ;; Quit snails if it has opened.
          (progn
            (snails-quit))

        ;; Set `snails-search-backends' if argument backends is set.
        (when (and (listp backends)
                   (> (length backends) 0))
          (setq snails-search-backends backends))

        ;; Record buffer before start snails.
        (setq snails-start-buffer (current-buffer))
        (setq snails-start-buffer-lines (line-number-at-pos (point-max)))

        ;; Init face with theme.
        (snails-init-face-with-theme)

        ;; Create input and content buffer.
        (snails-create-input-buffer)
        (snails-create-content-buffer)

        ;; Create popup frame to show search result.
        (snails-create-frame)

        ;; Search.
        (cond
         ;; Search with customize string when `search-object' is string.
         ((and (stringp search-object)
               (not (string-empty-p search-object)))
          (snails-search search-object))
         ;; Search symbol around point when `search-object' is t.
         (search-object
          (run-with-timer
           0.05 nil
           (lambda ()
             (let ((search-string (or (with-current-buffer snails-start-buffer
                                        (snails-pointer-string)) "")))
               (with-current-buffer snails-input-buffer
                 (insert search-string))
               (snails-search search-string)))))
         ;; Just launch with empty string when `search-object' is nil.
         (t
          (snails-search ""))))
    (message "Snails render candidates in new frame that only can be run in a graphical environment.")))

(defun snails-search-point ()
  "Search symbol at point"
  (interactive)
  (snails nil t))

(defun snails-select-next-item ()
  "Select next candidate item."
  (interactive)
  (with-current-buffer snails-content-buffer
    ;; Goto current line.
    (goto-char (overlay-start snails-select-line-overlay))
    ;; Jump to next candidate item position.
    (snails-jump-to-next-item)
    ;; Update select line
    (snails-update-select-line)
    ))

(defun snails-select-prev-item ()
  "Select previous candidate item."
  (interactive)
  (with-current-buffer snails-content-buffer
    ;; Goto current line.
    (goto-char (overlay-start snails-select-line-overlay))
    ;; Jump to previous candidate item position.
    (snails-jump-to-previous-item)
    ;; Update select line.
    (snails-update-select-line)
    ))

(defun snails-select-next-backend ()
  (interactive)
  (with-current-buffer snails-content-buffer
    (snails-select-backend-first-candidate
     (snails-get-next-backend-overlay))
    ))

(defun snails-select-prev-backend ()
  (interactive)
  (with-current-buffer snails-content-buffer
    (snails-select-backend-first-candidate
     (snalis-get-prev-backend-overlay))))

(defun snails-candidate-do ()
  "Confirm current candidate."
  (interactive)
  (let ((candidate-info (snails-candidate-get-info)))
    (when candidate-info
      (snails-backend-do
       (nth 0 candidate-info)
       (nth 1 candidate-info)))))

(defun snails-quit ()
  "Quit snails."
  (interactive)
  ;; Delete frame first.
  (ignore-errors (delete-frame snails-frame t))
  (setq snails-frame nil)
  (setq snails-frame-active-p nil)
  (setq snails-project-root-dir nil)
  (setq snails-start-buffer nil)
  (setq snails-select-line-overlay nil)
  (setq snails-need-render nil)
  (setq snails-select-backend-name nil)
  (setq snails-select-candidate-offset nil)
  (setq snails-search-backends nil)
  ;; Kill all subprocess and process buffers.
  (maphash
   (lambda (name process)
     (when process
       (kill-buffer (process-buffer process)))
     (when (and process
                (process-live-p process))
       (kill-process process)))
   snails-backend-subprocess-hash))

(defun snails-create-input-buffer ()
  "Create input buffer."
  (with-current-buffer (get-buffer-create snails-input-buffer)
    ;; Clean buffer.
    (erase-buffer)
    ;; Switch snails mode.
    (snails-mode)
    (run-hooks 'snails-mode-hook)
    ;; Set input buffer face.
    (buffer-face-set 'snails-input-buffer-face)
    ;; Disable hl-line, header-line and mode-line in input buffer.
    (setq-local global-hl-line-overlay nil)
    (setq-local header-line-format nil)
    (setq-local mode-line-format nil)
    ))

(defun snails-create-content-buffer ()
  "Create content buffer."
  (with-current-buffer (get-buffer-create snails-content-buffer)
    ;; Clean buffer.
    (erase-buffer)
    ;; Set coent buffer face.
    (buffer-face-set 'snails-content-buffer-face)
    ;; Disable header-line, mode-line, long line and cursor shape in content buffer.
    (setq-local header-line-format nil)
    (setq-local mode-line-format nil)
    (setq-local truncate-lines t)
    (setq-local cursor-type nil)
    ))

(defun snails-monitor-input (begin end length)
  "This is input monitor callback to hook `after-change-functions'."
  ;; Send new input to all backends when user change input.
  (when (string-equal (buffer-name) snails-input-buffer)
    (with-current-buffer snails-content-buffer
      (let* ((input (with-current-buffer snails-input-buffer
                      (buffer-substring (point-min) (point-max)))))
        (snails-search input)))))

(defun snails-create-frame ()
  "Create popup frame."
  (let* ((edges (frame-edges))
         (x (nth 0 edges))
         (y (nth 1 edges))
         (width  (- (nth 2 edges) x))
         (height (nth 3 edges))
         (frame-width (truncate (* snails-fame-width-proportion width)))
         (frame-height (truncate (* snails-fame-height-proportion height)))
         (frame-x (+ x (/ (- width frame-width) 2)))
         (frame-y (+ y (/ (- height frame-height) 3))))

    ;; Set project directory.
    (setq snails-project-root-dir
          (let ((project (project-current)))
            (when project
              (expand-file-name (cdr project))
              )))

    ;; Make popup frame, and position at center of current frame.
    (setq snails-frame
          (make-frame
           '((parent-frame . (window-frame))
             (skip-taskbar . t)
             (minibuffer . nil)
             (visibility . nil)
             (internal-border-width . 0)
             (left-fringe . 0)
             (right-fringe . 0)
             (vertical-scroll-bars . nil)
             (horizontal-scroll-bars . nil)
             (undecorated . t)
             (unsplittable . t)
             )))

    ;; Configuration frame.
    (with-selected-frame snails-frame
      ;; Delete other window first, make sure only one window in frame.
      (delete-other-windows)

      ;; Disable menu in snails frame.
      (set-frame-parameter snails-frame 'menu-bar-lines 0)

      ;; Set frame position and size.
      (set-frame-position snails-frame frame-x frame-y)
      (set-frame-size snails-frame frame-width frame-height t)

      ;; Set input window margin and switch to input buffer.
      (switch-to-buffer snails-input-buffer)
      (set-window-margins (selected-window) 1 1)

      ;; Split window with one line height of input buffer.
      (split-window (selected-window) (line-pixel-height) nil t)

      ;; Set content window margin and switch to content buffer.
      (other-window 1)
      (switch-to-buffer snails-content-buffer)
      (set-window-margins (selected-window) 1 1)

      ;; Add monitor callback in input change hook.
      (other-window 1)
      (add-hook 'after-change-functions 'snails-monitor-input nil t)

      ;; Focus out to hide snails frame on Mac.
      (when (featurep 'cocoa)
        (add-hook 'focus-out-hook 'snails-quit)))

    ;; Set active flag, use for advice-add detect.
    (setq snails-frame-active-p t)
    (setq snails-frame-active-time (float-time))

    ;; Show popup frame.
    ;; `select-frame-set-input-focus' is necessary for gnome-shell DE.
    (make-frame-visible snails-frame)
    (select-frame-set-input-focus snails-frame)))

(defun snails-search (input)
  "Search input with backends."
  (let ((search-content input))
    ;; Update input ticker.
    (setq snails-input-ticker (+ snails-input-ticker 1))

    ;; Set backends.
    (if snails-search-backends
        ;; Search special backends if `snails-search-backends' is not nil.
        ;; Snails won't filter with prefix in this situation.
        (setq snails-backends snails-search-backends)

      ;; Try search backends with prefix if `snails-search-backends' is nil.
      (let ((prefix (snails-input-prefix input))
            match-prefix)

        ;; Search prefix backend if match prefix in `snails-prefix-backends'.
        (catch 'search-prefix-backend
          (dolist (prefix-backend snails-prefix-backends)
            (when (equal prefix (car prefix-backend))
              (setq snails-backends (eval (cadr prefix-backend)))
              (setq search-content (substring input 1))
              (setq match-prefix t)
              (throw 'search-prefix-backend nil)
              )))

        ;; Search default backends if not match any prefix in `snails-prefix-backends'.
        (unless match-prefix
          (setq snails-backends snails-default-backends))))

    ;; Init `snails-candiate-list' with `snails-backends'.
    (setq snails-candiate-list (make-list (length snails-backends) nil))

    ;; Search.
    (snails-input-search search-content)))

(defun snails-input-prefix (input)
  "Get input prefix, return \"\" if input is empty."
  (cond ((equal (length input) 0)
         "")
        (t
         (substring input 0 1))))

(defun snails-input-search (input)
  "Search input with backends inf `snails-backends'."
  ;; Call all backends with new input.
  (dolist (backend snails-backends)
    (let ((search-func (cdr (assoc "search" (eval backend)))))
      (funcall search-func input snails-input-ticker 'snails-update-callback))))

(defun snails-update-callback (backend-name input-ticker candidates)
  "Update candiate callback, use by backend."
  ;; Just update candidates when input ticker is newest.
  ;; Candidate will not render if backend return input ticker is old.
  (when (and (equal input-ticker snails-input-ticker)
             candidates)
    (let* ((backend-names (snails-get-backend-names))
           (backend-index (cl-position backend-name backend-names)))
      (when backend-index
        ;; Update candidates by backend index.
        (setq snails-candiate-list (snails-update-list-by-index snails-candiate-list backend-index candidates))

        ;; Flag to re-render.
        (setq snails-need-render t)
        ))))

(defun snails-update-list-by-index (list n val)
  "Update candidates with backend index."
  (when list
    (nconc (cl-subseq list 0 n)
           (cons val (nthcdr (1+ n) list)))))

(defun snails-get-backend-names ()
  "Get all backend names."
  (mapcar (lambda (b) (eval (cdr (assoc "name" (eval b))))) snails-backends))

;; Add `snails-render-bufer' to timer when first start.
(when (not (featurep 'snails))
  (run-with-timer 0 0.1 'snails-render-bufer))

(defun snails-render-bufer ()
  "Render candidates when `snails-need-render' flag is set."
  (when snails-need-render
    (with-current-buffer snails-content-buffer
      ;; Record select line offset.
      (snails-record-select-line-offset)

      ;; Clean buffer first.
      (erase-buffer)

      ;; Clean all header line overlays.
      (setq snails-header-line-overlays nil)

      ;; Reset select line variables.
      (setq snails-select-line-overlay (make-overlay (point) (point) (current-buffer) t))
      (overlay-put snails-select-line-overlay 'face `snails-select-line-face)

      (let* ((candiate-index 0)
             (backend-names (snails-get-backend-names))
             (effective-backend-index 1)
             (effective-backend-number (length (cl-remove-if #'booleanp snails-candiate-list)))
             header-line-start
             header-line-end
             header-index-start
             header-index-end
             candidate-content-start
             candidate-content-end)
        ;; Render backend result.
        (dolist (candiate-list snails-candiate-list)
          ;; Just render backend result when return candidate is not nil.
          (when candiate-list
            ;; Render header line with overlay.
            (setq header-line-start (point))
            (insert (nth candiate-index backend-names))
            (setq header-line-end (point))
            (let ((header-line-overlay (make-overlay header-line-start header-line-end)))
              (add-to-list 'snails-header-line-overlays header-line-overlay)
              (overlay-put header-line-overlay
                           'face
                           'snails-header-line-face
                           ))

            ;; Insert backend index.
            (setq header-index-start (point))
            (insert (format " [%s/%s] (%s)\n" effective-backend-index effective-backend-number (length candiate-list)))
            (backward-char)
            (setq header-index-end (point))
            (overlay-put (make-overlay header-index-start header-index-end)
                         'face
                         'snails-header-index-face)
            (forward-char)
            (setq effective-backend-index (+ effective-backend-index 1))

            ;; Render candidate list.
            (dolist (candiate candiate-list)
              ;; Render candidate display name.
              (insert (nth 0 candiate))

              ;; Render candidate real content. ;
              (setq candidate-content-start (point))
              (insert (format "%s" (nth 1 candiate)))
              (setq candidate-content-end (point))
              (let ((candidate-content-overlay (make-overlay candidate-content-start candidate-content-end)))
                (overlay-put candidate-content-overlay 'display "")
                (overlay-put candidate-content-overlay
                             'face
                             'snails-candiate-content-face))
              (insert "\n"))

            ;; Insert new empty line at last candiate of backend.
            (insert "\n"))
          ;; Update candidate index to fetch name of next backend.
          (setq candiate-index (+ candiate-index 1))))

      ;; Restore select line offset.
      (snails-restore-select-line-offset)
      )

    ;; Reset render flag.
    (setq snails-need-render nil)))

(defun snails-record-select-line-offset ()
  "Record select line offset."
  (if snails-select-line-overlay
      ;; Record select line offset when `snails-select-line-overlay' is non-nil.
      (catch 'line-offset
        (dolist (header-line-overlay snails-header-line-overlays)
          (when (> (overlay-end snails-select-line-overlay) (overlay-end header-line-overlay))
            (setq snails-select-backend-name (buffer-substring (overlay-start header-line-overlay) (overlay-end header-line-overlay)))
            (setq snails-select-candidate-offset
                  (- (line-number-at-pos (overlay-end snails-select-line-overlay))
                     (line-number-at-pos (overlay-start header-line-overlay))))

            (throw 'line-offset nil))
          ))
    ;; Select first candiate offset if `snails-select-line-overlay' is nil.
    (setq snails-select-backend-name (nth 0 (snails-get-backend-names)))
    (setq snails-select-candidate-offset 1)
    ))

(defun snails-restore-select-line-offset ()
  "Restore select line offset."
  (unless (catch 'restore-line-offset
            (dolist (header-line-overlay snails-header-line-overlays)
              ;; Restore select line offset before render content buffer.
              (when (string-equal snails-select-backend-name (buffer-substring (overlay-start header-line-overlay) (overlay-end header-line-overlay)))
                (goto-char (overlay-start header-line-overlay))
                (forward-line snails-select-candidate-offset)
                (snails-update-select-line)

                (throw 'restore-line-offset t)
                ))
            nil)
    ;; Select first candidate if backend not exists when restore select line offset.
    (goto-char (point-min))
    (snails-select-next-item)
    (snails-update-select-line)
    ))

(defun snails-pointer-string ()
  "Get string around cursor."
  (if (use-region-p)
      ;; Get region string if mark is set.
      (buffer-substring-no-properties (region-beginning) (region-end))
    ;; Get current symbol or string, and remove prefix char before return.
    (let* ((current-string (if (snails-in-string-p)
                               (buffer-substring-no-properties
                                (1+ (car (snails-string-start+end-points)))
                                (cdr (snails-string-start+end-points)))
                             ""))
           (current-symbol (if (or (string-empty-p current-string)
                                   (string-match-p "[[:space:]]" current-string))
                               ;; Get symbol around point if string around point is empty or include spaces.
                               (thing-at-point 'symbol)
                             ;; Otherwise, get string around point.
                             current-string)))
      (cond ((string-prefix-p "." current-symbol)
             (string-remove-prefix "." current-symbol))
            ((string-prefix-p "#" current-symbol)
             (string-remove-prefix "#" current-symbol))
            (t current-symbol)))
    ))

(defun snails-string-start+end-points (&optional state)
  "Return a cons of the points of open and close quotes of the string.
The string is determined from the parse state STATE, or the parse state
  from the beginning of the defun to the point.
This assumes that `snails-in-string-p' has already returned true, i.e.
  that the point is already within a string."
  (save-excursion
    (let ((start (nth 8 (or state (snails-current-parse-state)))))
      (goto-char start)
      (forward-sexp 1)
      (cons start (1- (point))))))

(defun snails-current-parse-state ()
  "Return parse state of point from beginning of defun."
  (let ((point (point)))
    (beginning-of-defun)
    (parse-partial-sexp (point) point)))

(defun snails-in-string-p (&optional state)
  (or (nth 3 (or state (snails-current-parse-state)))
      (and
       (eq (get-text-property (point) 'face) 'font-lock-string-face)
       (eq (get-text-property (- (point) 1) 'face) 'font-lock-string-face))
      (and
       (eq (get-text-property (point) 'face) 'font-lock-doc-face)
       (eq (get-text-property (- (point) 1) 'face) 'font-lock-doc-face))
      ))

(defun snails-color-blend (c1 c2 alpha)
  "Blend two colors C1 and C2 with ALPHA.
C1 and C2 are hexidecimal strings.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (apply #'(lambda (r g b)
             (format "#%02x%02x%02x"
                     (ash r -8)
                     (ash g -8)
                     (ash b -8)))
         (cl-mapcar
          (lambda (x y)
            (round (+ (* x alpha) (* y (- 1 alpha)))))
          (color-values c1) (color-values c2))))

(defun snails-get-theme-colors ()
  "We need adjust snails's colors when user switch new theme."
  (let* ((white "#FFFFFF")
         (black "#000000")
         (bg-mode (frame-parameter nil 'background-mode))
         (bg-unspecified (string= (face-background 'default) "unspecified-bg"))
         (fg-unspecified (string= (face-foreground 'default) "unspecified-fg"))
         (fg (cond
              ((and fg-unspecified (eq bg-mode 'dark)) "gray80")
              ((and fg-unspecified (eq bg-mode 'light)) "gray20")
              (t (face-foreground 'default))))
         (bg (cond
              ((and bg-unspecified (eq bg-mode 'dark)) "gray20")
              ((and bg-unspecified (eq bg-mode 'light)) "gray80")
              (t (face-background 'default))))
         ;; for light themes
         (bg-dark (snails-color-blend black bg 0.1))
         (bg-more-dark (snails-color-blend black bg 0.15))
         (fg-dark (snails-color-blend fg bg-dark 0.7))
         (fg-more-dark (snails-color-blend black fg 0.2))
         ;; for dark themes
         (bg-light (snails-color-blend white bg 0.05))
         (bg-more-light (snails-color-blend white bg 0.1))
         (fg-light (snails-color-blend fg bg 0.7))
         (fg-more-light (snails-color-blend white fg 0.3)))
    (cond
     ((eq bg-mode 'dark)
      (list bg-light fg-dark bg-more-light fg-more-light))
     (t
      (list bg-dark fg-light bg-more-dark fg-more-dark)
      ))))

(defun snails-init-face-with-theme ()
  (let* ((colors (snails-get-theme-colors))
         (content-bg-color (nth 0 colors))
         (input-bg-color (nth 2 colors))
         (input-fg-color (nth 3 colors)))
    ;; Set input buffer face.
    (set-face-attribute 'snails-input-buffer-face nil
                        :background input-bg-color
                        :foreground input-fg-color)
    ;; Set coent buffer face.
    (set-face-attribute 'snails-content-buffer-face nil
                        :background content-bg-color)
    ))

(defun snails-jump-to-next-item ()
  "Select next candidate item."
  ;; Forward line.
  (forward-line)
  ;; Skip empty line and header line.
  (while (and (not (eobp))
              (or
               (snails-empty-line-p)
               (snails-header-line-p)))
    (forward-line))
  ;; Adjust line if reach bottom line.
  (when (and (eobp)
             (snails-empty-line-p))
    (previous-line 2)))

(defun snails-jump-to-previous-item ()
  "Select previous candidate item."
  ;; Previous line.
  (previous-line)
  (move-beginning-of-line 1)
  ;; Skip empty line and header line.
  (while (and (not (bobp))
              (or
               (snails-empty-line-p)
               (snails-header-line-p)))
    (previous-line)
    (move-beginning-of-line 1))
  ;; Adjust line if reach to line.
  (when (bobp)
    (forward-line)))

(defun snails-get-next-backend-overlay ()
  (catch 'backend-overlay
    (let (next-backend-overaly)
      (dolist (header-line-overlay snails-header-line-overlays)
        (when (> (point) (overlay-end header-line-overlay))
          (throw 'backend-overlay next-backend-overaly))
        (setq next-backend-overaly header-line-overlay)
        ))))

(defun snalis-get-prev-backend-overlay ()
  (catch 'backend-overlay
    (let (found-current)
      (dolist (header-line-overlay snails-header-line-overlays)
        (when found-current
          (throw 'backend-overlay header-line-overlay))
        (setq found-current (> (point) (overlay-end header-line-overlay)))
        ))))

(defun snails-select-backend-first-candidate (backend-overlay)
  (when backend-overlay
    (goto-line (line-number-at-pos (overlay-start backend-overlay)))
    (snails-jump-to-next-item)
    (snails-update-select-line)
    ))

(defun snails-get-candidate-backend-name (candidate-point)
  "Get backend name at selected candidate position."
  (catch 'backend-name
    (dolist (header-line-overlay snails-header-line-overlays)
      (when (> candidate-point (overlay-end header-line-overlay))
        (throw 'backend-name (buffer-substring (overlay-start header-line-overlay) (overlay-end header-line-overlay))))
      )))

(defun snails-backend-do (backend-name candidate)
  "Confirm candidate with special backend."
  (catch 'backend-do
    (dolist (backend snails-backends)
      (let ((name (cdr (assoc "name" (eval backend))))
            (do-func (cdr (assoc "do" (eval backend)))))

        (when (equal (eval name) backend-name)
          ;; Quit frame first.
          (snails-quit)

          ;; Call backend do function.
          (funcall do-func candidate)
          (throw 'backend-do nil)
          )))))

(defun snails-keep-cursor-visible ()
  "Scrol window to keep cursor in visible area of window."
  ;; Scrol window to keep cursor in visible area of window.
  (when (get-buffer-window snails-content-buffer)
    (set-window-point (get-buffer-window snails-content-buffer) (point))))

(defun snails-header-line-p ()
  "Detect whether at header line."
  (let ((overlays (overlays-at (point)))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (eq (overlay-get overlay 'face) 'snails-header-line-face)
            (setq found t)))
      (setq overlays (cdr overlays)))
    found))

(defun snails-empty-line-p ()
  "Detect current line whether empty line."
  (= (point-at-eol) (point-at-bol)))

(defun snails-wrap-buffer-icon (buf)
  "Wrap display name with buffer icon, use for buffer search backend."
  (if (featurep 'all-the-icons)
      (format "%s %s"
              (with-current-buffer buf
                (all-the-icons-icon-for-buffer))
              (string-trim-left (buffer-name buf)))
    (buffer-name buf)))

(defun snails-wrap-file-icon (file)
  "Wrap display name with file icon, use for file search backend."
  (if (featurep 'all-the-icons)
      (format "%s %s"
              (all-the-icons-icon-for-file file :height 1)
              (string-trim-left file))
    file))

(defun snails-wrap-file-icon-with-candidate (file candidate &optional no-trim)
  "Wrap display name with file icon, use for file search backend."
  (if (featurep 'all-the-icons)
      (format "%s %s"
              (all-the-icons-icon-for-file (format "hello.%s" (file-name-extension file)) :height 1)
              (if no-trim
                  candidate
                (string-trim-left candidate)
                ))
    candidate))

(defun snails-format-line-number (line-number max-line-number)
  "Format line number with same width."
  (concat (make-string (- (length (format "%s" max-line-number)) (length line-number)) ?\ ) line-number))

(defun snails-update-select-line ()
  "Update select line status."
  ;; Update select line overlay postion.
  (move-overlay snails-select-line-overlay
                (point-at-bol)
                (point-at-eol))
  ;; Scroll window to keep cursor visible.
  (snails-keep-cursor-visible))

(defun snails-generate-proces-buffer-name ()
  "Create unique buffer for subprocess buffer of async backend."
  (format " *%04x-%04x-%04x-%04x-%04x-%04x-%04x"
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4)) ))

(defun snails-kill-backend-subprocess (name)
  "Kill subprocess of async backend."
  (let ((current-process (gethash name snails-backend-subprocess-hash)))
    ;; Kill process buffer.
    (when current-process
      (kill-buffer (process-buffer current-process)))
    ;; Kill process if deserted process still live.
    (when (and current-process
               (process-live-p current-process))
      (kill-process current-process))
    ))

(defun snails-update-backend-subprocess (name process)
  "Update subprocess of async backend."
  ;; Update new process with backend name.
  (puthash name process snails-backend-subprocess-hash))

(defun snails-create-async-process (name input input-ticker build-command candidate-filter update-callback)
  "Create subprocess of async backend.
And render result when subprocess finish search."
  (interactive)
  (let ((commands (funcall build-command input)))
    ;; Only make subprocess when commands is not nil.
    (when commands
      ;; Kill old subprocess.
      (snails-kill-backend-subprocess name)

      ;; We need delay 0.1 second to call make subprocess,
      ;; avoid create many deserted subprocess when user enter character too fast like me. ;)
      (run-with-idle-timer
       0.1
       nil
       (lambda ()
         (when (equal input-ticker snails-input-ticker)
           ;; Make subprocess if input ticker still is newest.
           ;; Give up creating subprocess if input ticker already expired.
           (let ((process-buffer (get-buffer-create (snails-generate-proces-buffer-name))))
             ;; Don't ask anything when snails kill process buffer.
             (with-current-buffer process-buffer
               (setq-local kill-buffer-query-functions
                           (remq 'process-kill-buffer-query-function
                                 kill-buffer-query-functions)))

             (snails-update-backend-subprocess
              name
              (make-process
               :name ""
               :buffer process-buffer
               :command commands
               :sentinel (lambda (process event)
                           ;; Render result to content buffer when subprocess finish.
                           (when (string= (substring event 0 -1) "finished")
                             (let ((buffer (process-buffer process)))
                               ;; Do nothing if process buffer has killed.
                               (when (get-buffer buffer)
                                 (ignore-errors
                                   (with-current-buffer buffer
                                     (let ((candidate-list (butlast (split-string (buffer-string) "\n"))))
                                       ;; If `candidate-list' is nil, it cause by call `buffer-string' but process buffer has killed.
                                       (when candidate-list
                                         (funcall
                                          update-callback
                                          name
                                          input-ticker
                                          (funcall candidate-filter candidate-list)
                                          ))))

                                   ;; Clean process buffer.
                                   (kill-buffer buffer)))))
                           )))))
         )))))

(defun snails-frame-is-active-p ()
  (and snails-frame
       (frame-live-p snails-frame)
       snails-frame-active-p
       (eq (window-frame (selected-window)) snails-frame)))

(defun snails-candidate-get-info ()
  (with-current-buffer snails-content-buffer
    ;; Goto candidate content overlay position.
    (goto-char (overlay-start snails-select-line-overlay))
    (end-of-line)
    (backward-char)

    ;; Pickup candidate content and confirm by corresponding backend.
    (let ((overlays (overlays-at (point))))
      (catch 'candidate
        (while overlays
          (let ((overlay (car overlays)))
            ;; Find overlay that face is `snails-candiate-content-face'.
            (when (eq (overlay-get overlay 'face) 'snails-candiate-content-face)
              (throw 'candidate
                     (list
                      (snails-get-candidate-backend-name (point))
                      (buffer-substring (overlay-start overlay) (overlay-end overlay))))))
          (setq overlays (cdr overlays))))
      )))

(defun snails-fuz-library-load-p ()
  "Test `fuz' libary is load."
  (cond ((string-equal snails-fuz-library-load-status "uncheck")
         (if (ignore-errors (load-library "fuz"))
             (progn
               (setq snails-fuz-library-load-status "load")
               t)
           (setq snails-fuz-library-load-status "unload")
           nil))
        ((string-equal snails-fuz-library-load-status "load")
         t)
        (t
         nil)))

(defun snails-build-fuzzy-regex (input)
  "Create a fuzzy regexp of PATTERN."
  (mapconcat (lambda (ch)
               (let ((s (char-to-string ch)))
                 (format "[^%s]*%s" s (regexp-quote s))))
             input ""))

(defun snails-add-candiate (candidate-list candidate-name candidate-content)
  "Append candiate display name and content to candiate list."
  (add-to-list candidate-list (list candidate-name candidate-content) t))

(defun snails-sort-candidates (input candidates match-index content-index)
  "If `fuz' library load, sort candidates with fuzz scrore.
If `fuz' library not found, not sorting.

`input' is user input string to build match regex.
`candidates' is candidate list need sort.
`match-index' is index to fetch match part from candiate, use for calculate sort score.
`content-index' is index to content part from candiate, use for compare content when fuzz score is same."
  (when (and (snails-fuz-library-load-p)
             candidates)
    (let ((fuzzy-re (snails-build-fuzzy-regex input))
          retval)

      (while candidates
        (when (string-match-p fuzzy-re (nth match-index (car candidates)))
          (push (pop candidates) retval)))

      (cl-sort (mapcar (lambda (it)
                         (cons it (fuz-calc-score-skim input (nth match-index it))))
                       retval)
               (pcase-lambda (`(,candidate1 . ,fuzz-score1) `(,candidate2 . ,fuzz-score2))
                 (if (equal fuzz-score1 fuzz-score2)
                     (string> (nth content-index candidate1) (nth content-index candidate2))
                   (< fuzz-score1 fuzz-score2)))))))

(defun snails-match-input-p (input candidate-content)
  "If `fuz' library load, use fuzz match algorithm.
If `fuz' not found, use normal match algorithm."
  (if (snails-fuz-library-load-p)
      (string-match-p (snails-build-fuzzy-regex input) candidate-content)
    (string-match-p (regexp-quote input) candidate-content)))

(defun snails-start-buffer-dir ()
  "Get directory of `snails-start-buffer'.

If `snails-start-buffer' is nil, get path of HOME.
If `snails-start-buffer' is dired-mode, get path by `dired-directory'.
Otherwise get path by `buffer-file-name'."
  (cond ((not snails-start-buffer)
         (expand-file-name "~"))
        ((equal 'dired-mode (with-current-buffer snails-start-buffer major-mode))
         (with-current-buffer snails-start-buffer
           (expand-file-name dired-directory)))
        ((not (buffer-file-name snails-start-buffer))
         (expand-file-name "~"))
        (t
         (file-name-directory (buffer-file-name snails-start-buffer)))))

(defun snails-pick-search-info-from-input (input)
  "If nothing after @ , return HOME path and search string.
If type soemthing after @ , split input with input-dir and search-content.
Otherwise return nil."
  (when (string-match-p "@" input)
    (let (search-content input-dir)
      (setq search-content (split-string input "@"))
      (setq input-dir (second search-content))
      (cond ((equal input-dir "")
             (list (expand-file-name "~") (first search-content)))
            ((file-exists-p input-dir)
             (list input-dir (first search-content)))
            (t nil))
      )))

(defun snails-flash-line ()
  (let ((pulse-iterations 1)
        (pulse-delay 0.3))
    (pulse-momentary-highlight-one-line (point) 'highlight)))

(advice-add 'other-window
            :around
            (lambda (orig &rest args)
              "Disable `other-window' in snails frame."
              (unless (snails-frame-is-active-p)
                (apply orig args))))

(advice-add 'delete-other-windows
            :around
            (lambda (orig &rest args)
              "Disable `delete-other-windows' in snails frame."
              (unless (snails-frame-is-active-p)
                (apply orig args))))

(advice-add 'delete-window
            :around
            (lambda (orig &rest args)
              "Disable `delete-window' in snails frame."
              (unless (snails-frame-is-active-p)
                (apply orig args))))

(advice-add 'delete-buffer-window
            :around
            (lambda (orig &rest args)
              "Disable `delete-buffer-window' in snails frame."
              (unless (snails-frame-is-active-p)
                (apply orig args))))

(advice-add 'kill-buffer
            :around
            (lambda (orig &rest args)
              "Disable `kill-buffer' in snails frame."
              (unless (equal (buffer-name (current-buffer)) snails-input-buffer)
                (apply orig args))
              ))

(defun snails-monitor-minibuffer-enter ()
  (when (and snails-frame
             (frame-live-p snails-frame))
    (snails-quit)))

(add-hook 'minibuffer-setup-hook 'snails-monitor-minibuffer-enter)

(cl-defmacro snails-create-sync-backend (&rest args &key name candidate-filter candiate-do)
  "Macro to create sync backend code.

`name' is backend name, such 'Foo Bar'.
`candidate-filter' is function that accpet input string, and return candidate list, example format: ((display-name-1 candidate-1) (display-name-2 candidate-2))
`candidate-do-function' is function that confirm candidate, accpet candidate search, and do anything you want.
"
  (let* ((backend-template-name (string-join (split-string (downcase name)) "-"))
         (backend-name (intern (format "snails-backend-%s" backend-template-name)))
         (search-function (intern (format "snails-backend-%s-search" backend-template-name))))
    `(progn
       (defun ,search-function(input input-ticker update-callback)
         (funcall
          update-callback
          ,name
          input-ticker
          (funcall ,candidate-filter input)))

       (defvar ,backend-name nil)

       (setq ,backend-name
             '(("name" . ,name)
               ("search" . ,search-function)
               ("do" . ,candiate-do)
               )
             ))))

(cl-defmacro snails-create-async-backend (&rest args &key name build-command candidate-filter candiate-do)
  "Macro to create sync backend code.

`name' is backend name, such 'Foo Bar'.
`candidate-filter' is function that accpet input string, and return candidate list, example format: ((display-name-1 candidate-1) (display-name-2 candidate-2))
`candidate-do-function' is function that confirm candidate, accpet candidate search, and do anything you want.
"
  (let* ((backend-template-name (string-join (split-string (downcase name)) "-"))
         (backend-name (intern (format "snails-backend-%s" backend-template-name)))
         (search-function (intern (format "snails-backend-%s-search" backend-template-name))))
    `(progn
       (defun ,search-function(input input-ticker update-callback)
         (funcall
          'snails-create-async-process
          ,name
          input
          input-ticker
          ,build-command
          ,candidate-filter
          update-callback
          ))

       (defvar ,backend-name nil)

       (setq ,backend-name
             '(("name" . ,name)
               ("search" . ,search-function)
               ("do" . ,candiate-do)
               )
             ))))

(provide 'snails-core)

;;; snails-core.el ends here
